;;; kite.el --- WebKit inspector front-end

;; Copyright (C) 2012 Julian Scheid

;; Author: Julian Scheid <julians37@gmail.com>
;; Keywords: tools
;; Version: 0.1
;; Package-Requires: ((json "1.2") (websocket "0.93.1"))
;; Compatibility: GNU Emacs 24

;; This file is not part of GNU Emacs.

;; Kite is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Kite is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Kite.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Kite is a front-end for the WebKit inspector remote API.  It allows
;; to debug web applications running in a WebKit browser instance and
;; aims to provide functionality similar to the default inspector
;; front-end built into WebKit browsers.


;;; Code:

(require 'json)
(require 'websocket)
(require 'url-parse)
(eval-when-compile (require 'cl))

(require 'kite-debug)
(require 'kite-dom)
(require 'kite-memory)
(require 'kite-net)
(require 'kite-repl)
(require 'kite-object)
(require 'kite-console)
(require 'kite-breakpoint)
(require 'kite-global)

(make-variable-buffer-local 'kite-buffer-type)
(set-default 'kite-buffer-type nil)

(make-variable-buffer-local 'kite-session)
(set-default 'kite-session nil)

(setq websocket-debug t)
(setq websocket-callback-debug-on-error t)

(defvar kite-tab-history nil)
(defvar kite-most-recent-session nil)
(defvar kite-after-mode-hooks nil)

(defstruct (kite-session)
  websocket
  page-favicon-url
  page-thumbnail-url
  page-url
  page-title
  breakpoint-ewoc
  unique-name
  (script-infos (make-hash-table :test 'equal))
  (debugger-state kite--debugger-state-resumed)
  (next-request-id 0)
  (pending-requests (make-hash-table))
  (buffers nil))

(defstruct (kite-script-info)
  url
  start-line
  start-column
  end-line
  end-column)

(defvar kite-active-sessions
  (make-hash-table :test 'equal :weakness 'value))

(defvar kite-active-session-list nil)

(defun kite-send (method &optional params callback callback-args)
  (let ((callback-buffer (current-buffer))
        (request-id (incf (kite-session-next-request-id kite-session))))
    (puthash request-id (list (or callback (lambda (response) nil))
                              callback-buffer
                              callback-args)
             (kite-session-pending-requests kite-session))
    (let ((json-request (json-encode
                         (list
                          (cons :jsonrpc "2.0")
                          (cons :method method)
                          (cons :params params)
                          (cons :id request-id)))))
      (kite--log "Sending request: %s" json-request)
      (websocket-send-text (kite-session-websocket kite-session)
                           json-request))))

(defun kite--session-remove-current-buffer ()
  (setf (kite-session-buffers kite-session)
        (delete (current-buffer) (kite-session-buffers kite-session)))
  (when (null (kite-session-buffers kite-session))
    (when (equal kite-most-recent-session
                 (websocket-url (kite-session-websocket kite-session)))
      (setq kite-most-recent-session nil))
    (remhash (websocket-url (kite-session-websocket kite-session))
             kite-active-sessions)
    (setq kite-active-session-list
          (remove kite-session kite-active-session-list))
    (when t ; workaround for websocket.el bug? TODO: revisit once latest websocket is working again
      (process-send-eof (websocket-conn (kite-session-websocket kite-session)))
      (kill-process (websocket-conn (kite-session-websocket kite-session))))
    (websocket-close (kite-session-websocket kite-session))))

(defun kite--on-message (websocket frame)
  ;;;(kite--log "received frame: %s" frame)
  (let ((buf (current-buffer))
        (kite-session (gethash (websocket-url websocket)
                               kite-active-sessions)))
    (when (and (eq (aref frame 0) 'cl-struct-websocket-frame)
               (eq (aref frame 1) 'text))
      (let* ((json-object-type 'plist)
             (response (json-read-from-string (aref frame 2))))

        (kite--log "received response: %s"
                   (pp-to-string response))
        (when (listp response)
          (with-current-buffer buf
            (let ((response-id (plist-get response :id)))
              (if response-id
                  (let ((callback-info
                         (gethash response-id
                                  (kite-session-pending-requests
                                   kite-session))))
                    (remhash response-id (kite-session-pending-requests
                                          kite-session))
                    (when (buffer-live-p (nth 1 callback-info))
                      (with-current-buffer (nth 1 callback-info)
                        (apply (nth 0 callback-info)
                               response
                               (nth 2 callback-info)))))

                (run-hook-with-args
                 (intern
                  (concat "kite-"
                          (replace-regexp-in-string
                           "\\."
                           "-"
                           (plist-get response :method))
                          "-hooks"))
                 (websocket-url websocket)
                 (plist-get response :params))))))))))

(defun kite--on-close (websocket)
  (kite--log "websocket connection closed"))

(defun kite--connect-webservice (tab-alist)
  (let ((websocket-url (plist-get tab-alist :webSocketDebuggerUrl)))

    (kite--log "Connecting to %s" websocket-url)

    (set (make-local-variable 'kite-session)
         (make-kite-session
          :websocket (websocket-open
                      websocket-url
                      :on-message (function kite--on-message)
                      :on-close (function kite--on-close))
          :page-favicon-url (plist-get tab-alist :faviconUrl)
          :page-thumbnail-url (plist-get tab-alist :thumbnailUrl)
          :page-url (plist-get tab-alist :url)
          :page-title (plist-get tab-alist :title)
          :unique-name (kite--unique-session-name
                        (plist-get tab-alist :title))))

    (puthash websocket-url kite-session kite-active-sessions)
    (if kite-active-session-list
        (setcdr kite-active-session-list (cons kite-session nil))
      (setq kite-active-session-list (cons kite-session nil)))

    (kite-send "Page.enable" nil
               (lambda (response) (kite--log "Page notifications enabled.")))
    (kite-send "Inspector.enable" nil
               (lambda (response) (kite--log "Inspector enabled.")))
    (kite-send "Debugger.enable" nil
               (lambda (response) (kite--log "Debugger enabled.")))
    (kite-send "CSS.enable" nil
               (lambda (response) (kite--log "CSS enabled.")))
    (kite-send "Debugger.canSetScriptSource" nil
               (lambda (response) (kite--log "got response: %s" response)))))

(defun kite--find-buffer (websocket-url type)
  (let ((buffer-iterator (buffer-list))
        found)
    (while (and buffer-iterator (not found))
      (let ((buffer-kite-session (buffer-local-value
                                  'kite-session
                                  (car buffer-iterator))))
        (when (and buffer-kite-session
                   (string= (websocket-url (kite-session-websocket buffer-kite-session))
                            websocket-url)
                   (eq type (buffer-local-value 'kite-buffer-type (car buffer-iterator))))
          (setq found (car buffer-iterator))))
      (setq buffer-iterator (cdr buffer-iterator)))
    found))

(defun kite--get-buffer-create (websocket-url type mode)
  (lexical-let*
      ((-kite-session (gethash websocket-url kite-active-sessions))
       (buf (or (kite--find-buffer websocket-url type)
                (generate-new-buffer
                 (format "*kite %s %s*"
                         type
                         (kite-session-unique-name -kite-session))))))
    (switch-to-buffer buf)
    (with-current-buffer buf
      (funcall mode)
      (setq kite-session -kite-session)
      (set (make-local-variable 'kite-buffer-type) type)
      (add-hook 'kill-buffer-hook 'kite--kill-buffer nil t)
      (run-hooks 'kite-after-mode-hooks))
    buf))

(defun kite-connect (&optional host port)
  (let* ((url-request-method "GET")
         (url-package-name "kite.el")
         (url-package-version "0.1")
         (url-http-attempt-keepalives nil)
         (use-host (or host "127.0.0.1"))
         (use-port (or port 9222))
         (url
          (url-parse-make-urlobj "http" nil nil use-host use-port "/json")))
    (message "using url-http-attempt-keepalives: %s" url-http-attempt-keepalives)
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char 0)
      (if (and (looking-at "HTTP/1\\.. 200")
               (re-search-forward "\n\n" nil t))
          (let* ((debugger-tabs (let ((json-array-type 'list)
                                      (json-object-type 'plist))
                                  (json-read)))
                 (available-debuggers (make-hash-table))
                 (available-strings (make-hash-table :test 'equal))
                 (completion-strings (make-hash-table :test 'equal))
                 (completion-candidates nil))

            ;; Gather debuggers from server response

            (mapcar (lambda (el)
                      (when (plist-member el :webSocketDebuggerUrl)
                        (puthash
                         (plist-get el :webSocketDebuggerUrl)
                         (cons el nil)
                         available-debuggers)))
                    debugger-tabs)

            ;; Gather debuggers currently open

            (maphash (lambda (websocket-url kite-session)
                       (puthash
                        websocket-url
                        `((:webSocketDebuggerUrl
                           ,websocket-url
                           :thumbnailUrl ,(kite-session-page-thumbnail-url kite-session)
                           :faviconUrl ,(kite-session-page-favicon-url kite-session)
                           :title ,(kite-session-page-title kite-session)
                           :url ,(kite-session-page-url kite-session)) . ,kite-session)
                        available-debuggers))
                     kite-active-sessions)

            ;; For each human readable identifier (url or title), see
            ;; if it is ambiguous

            (flet ((add-item (item url)
                             (let ((existing (gethash item available-strings '(0))))
                               (puthash item
                                        (cons (1+ (car existing))
                                              (cons url (cdr existing)))
                                        available-strings))))

              (maphash (lambda (key value)
                         (let ((url (plist-get (car value) :url))
                               (title (plist-get (car value) :title))
                               (websocket-url (plist-get (car value)
                                                         :webSocketDebuggerUrl)))
                           (add-item url websocket-url)
                           (when (not (equal title url))
                             (add-item title websocket-url))))
                       available-debuggers))

            ;; Final pass, disambiguate and rearrange

            (flet ((disambiguate (string websocket-url)
                                 (let ((existing
                                        (gethash string available-strings)))
                                   (if (<= (car existing) 1)
                                       string
                                     (concat string
                                             " ("
                                             (substring websocket-url
                                                        (length (kite--longest-prefix
                                                                 (cdr existing))))
                                             ")")))))

              (maphash (lambda (key value)
                         (let ((url (plist-get (car value) :url))
                               (title (plist-get (car value) :title))
                               (websocket-url (plist-get
                                               (car value)
                                               :webSocketDebuggerUrl)))

                           (puthash (disambiguate url websocket-url)
                                    value
                                    completion-strings)
                           (puthash (disambiguate title websocket-url)
                                    value
                                    completion-strings)))
                       available-debuggers))

            ;; Map to keys

            (maphash (lambda (key value)
                       (setq completion-candidates
                             (cons key completion-candidates)))
                     completion-strings)

            (let ((selection (completing-read
                              "Choose tab: "
                              completion-candidates
                              nil t nil 'kite-tab-history)))
              (when (> (length selection) 0)
                (kite--connect-webservice
                 (car (gethash selection completion-strings)))
                (plist-get (car (gethash selection completion-strings)) :webSocketDebuggerUrl))))
        (error "Could not contact remote debugger at %s:%s, check host and port%s"
               use-host
               use-port
               (if (> (length (buffer-string)) 0)
                   (concat ": " (buffer-string)) ""))))))

(defun kite-reload-page (&optional arg)
  "Reload the page associated with the current buffer.  With a
prefix argument ARG, ignore (force-refresh) the browser cache."
  (interactive "P")
  (unless kite-most-recent-session
    (error "No kite session active"))
  (lexical-let ((bool-prefix (not (null arg)))
                (kite-session (gethash kite-most-recent-session kite-active-sessions)))
    (kite-send "Page.reload"
               `((ignoreCache . ,(if bool-prefix t :json-false)))
               (lambda (response)
                 (if bool-prefix
                     (message "Page reloaded (with cache ignored)")
                   (message "Page reloaded"))))))

(defun kite--unique-session-name (title)
  ;; FIXME
  title)

(defvar kite-profiler-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "j" 'kite-javascript-profiler)
    (define-key map "c" 'kite-css-profiler)
    (define-key map "h" 'kite-heap-profiler)
    map))

(defvar kite-global-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "c" 'kite-console)
    (define-key map "d" 'kite-debug)
    (define-key map "h" 'kite-heap)
    (define-key map "m" 'kite-dom)
    (define-key map "n" 'kite-network)
    (define-key map "p" kite-profiler-keymap)
    (define-key map "r" 'kite-resources)
    (define-key map "t" 'kite-timeline)
    (define-key map "k" 'kite-repl)
    (define-key map "!" 'kite-reload-page)
    map))

(global-set-key "\C-ck" kite-global-keymap)

(defun kite--find-default-session (prefix)
  (cond
   ((equal '(4) prefix)
    (kite-connect))
   ((equal '(16) prefix)
    (kite-connect
     (read-from-minibuffer "Host: " "localhost" nil nil 'kite-remote-host-history "localhost")
     (let ((port (read-from-minibuffer "Port: " "9222" nil t 'kite-remote-port-history "9222")))
       (unless (and (numberp port)
                    (>= port 1)
                    (<= port 65535))
         (error "Port must be a number between 1 and 65535"))
       port)))
   ((numberp prefix)
    (unless (and (>= prefix 1)
                 (<= prefix (length kite-active-session-list)))
      (error "No such kite session index: %s" prefix))
    (websocket-url
     (kite-session-websocket
      (nth (- prefix 1) kite-active-session-list))))
   ((gethash kite-most-recent-session kite-active-sessions)
    kite-most-recent-session)
   ((> 0 (hash-table-count kite-active-sessions))
    (let (random-session)
      (maphash (lambda (key value)
                 (unless random-session
                   (setq random-session key)))
               kite-active-sessions)))
   (t
    (kite-connect))))

(defun kite-maybe-goto-buffer (prefix type)
  (let ((session (kite--find-default-session prefix)))
    (when session
      (kite--get-buffer-create session type
                               (intern (format "kite-%s-mode" type))))))

(defun kite-console (prefix)
  (interactive "P")
  (kite-maybe-goto-buffer prefix 'console))

(defun kite-debug (prefix)
  (interactive "P")
  (kite-maybe-goto-buffer prefix 'debug))

(defun kite-dom (prefix)
  (interactive "P")
  (kite-maybe-goto-buffer prefix 'dom))

(defun kite-network (prefix)
  (interactive "P")
  (kite-maybe-goto-buffer prefix 'network))

(defun kite-repl (prefix)
  (interactive "P")
  (kite-maybe-goto-buffer prefix 'repl))

(defun kite-javascript-profiler (prefix)
  (interactive "P")
  (error "kite-javascript-profiler not yet implemented"))

(defun kite-css-profiler (prefix)
  (interactive "P")
  (error "kite-css-profiler not yet implemented"))

(defun kite-heap-profiler (prefix)
  (interactive "P")
  (error "kite-heap-profiler not yet implemented"))

(defun kite-close-all-sessions ()
  (interactive)
  (dolist (process (process-list))
    (when (string-match-p "^websocket to" (process-name process))
      (process-send-eof process)))
  (clrhash kite-active-sessions)
  (setq kite-active-session-list nil)
  (setq kite-most-recent-session nil))

(defun kite--kill-buffer ()
  (ignore-errors
    (kite--session-remove-current-buffer)))

(defun kite-remember-recent-session ()
  (setq kite-most-recent-session
        (or (when kite-session
              (websocket-url (kite-session-websocket kite-session)))
            kite-most-recent-session)))

(add-hook 'post-command-hook 'kite-remember-recent-session)

(provide 'kite)
