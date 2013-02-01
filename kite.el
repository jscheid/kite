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
(require 'cl)

(require 'kite-debug)
(require 'kite-dom)
(require 'kite-memory)
(require 'kite-net)
(require 'kite-scratch)
(require 'kite-object)
(require 'kite-console)
(require 'kite-breakpoint)
(require 'kite-modeline)
(require 'kite-sourcemap)
(require 'kite-global)

(defcustom kite-default-remote-host "127.0.0.1"
  "Default host for connection to WebKit remote debugging API."
  :group 'kite)

(defcustom kite-default-remote-port 9222
  "Default port for connection to WebKit remote debugging API."
  :group 'kite)

(defcustom kite-default-emulate-touch-events nil
  "Whether new sessions should emulate touch events by default."
  :group 'kite)

(defcustom kite-default-show-paint-rects nil
  "Whether new sessions should show paint rectangles by
  default.."
  :group 'kite)

(defcustom kite-default-disable-cache nil
  "Whether new sessions should disable the cache by default."
  :group 'kite)

(setq websocket-debug t)
(setq websocket-callback-debug-on-error t)

(defvar kite-tab-history nil
  "Keeps completion history of debugger tabs")

(defface kite-session-closed
  '((((class color) (min-colors 88) (background light))
     (:inherit default :background "RosyBrown1"))
    (((class color) (min-colors 88) (background dark))
     (:inherit default :background "red4"))
    (((class color) (min-colors 16))
     (:inherit default :background "red"))
    (((class color) (min-colors 8))
     (:inherit default :background "red"))
    (((class color grayscale))
     :foreground "grey")
    (t (:inverse-video t)))
  "Face for displaying 'session closed' message in kite buffers."
  :version "24.1"
  :group 'kite)

(defun kite--session-remove-current-buffer ()
  "Remove the current buffer from the list of the current
session's buffers.  If no buffers remain in the current session,
kill the session.  The current session is retrieved from variable
`kite-session', which is buffer-local or taken from a let
binding."
  (setf (kite-session-buffers kite-session)
        (delete (current-buffer) (kite-session-buffers kite-session)))
  (when (null (kite-session-buffers kite-session))
    (when (equal kite-most-recent-session
                 (websocket-url (kite-session-websocket
                                 kite-session)))
      (setq kite-most-recent-session nil))
    (remhash (websocket-url (kite-session-websocket kite-session))
             kite-active-sessions)
    (setq kite-active-session-list
          (remove kite-session kite-active-session-list))
    (websocket-close (kite-session-websocket kite-session)))
  (kite--mode-line-update))

(defun kite--on-message (websocket frame)
  "Invoked when a message is received from the JSON-RPC server.
If the message contains a request ID, the callback passed to
`kite-send' when the request was sent is invoked.  Otherwise, the
hooks corresponding to the response method are run.  For example,
if the response method is `Page.frameNavigated' then
`kite-Page-frameNavigated-hooks' are run."
  ;;;(kite--log "received frame: %s" frame)
  (let ((buf (current-buffer))
        (kite-session (gethash (websocket-url websocket)
                               kite-active-sessions)))
    (when (and (eq (aref frame 0) 'cl-struct-websocket-frame)
               (eq (aref frame 1) 'text))
      (let* ((json-object-type 'plist)
             (response (json-read-from-string (aref frame 2))))

        (kite--log "received message: %s"
                   (pp-to-string response))
        (when (listp response)
          (with-current-buffer buf
            (let ((response-id (plist-get response :id)))
              (if response-id
                  (let ((callback-info
                         (gethash response-id
                                  (kite-session-pending-requests
                                   kite-session))))
                    (remhash response-id
                             (kite-session-pending-requests
                              kite-session))
                    (when (buffer-live-p (nth 2 callback-info))
                      (with-current-buffer (nth 2 callback-info)
                        (if (plist-member response :error)
                            (apply (nth 1 callback-info)
                                   (plist-get response :error)
                                   (nth 3 callback-info))
                          (apply (nth 0 callback-info)
                                 (plist-get response :result)
                                 (nth 3 callback-info))))))

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
  "Invoked when the JSON-RPC server closed the connection, most
likely because the tab or browser was closed.  Removes the
session from the list of active sessions and adds a header line
to all session buffers saying that the session is closed."
  (let ((kite-session (gethash (websocket-url websocket)
                               kite-active-sessions)))
    (when kite-session
      (message "\
Kite session was closed by the remote debugging server: %s"
               (kite-session-page-title kite-session))
      (dolist (kite-buffer (kite-session-buffers kite-session))
        (with-current-buffer kite-buffer
          (set (make-local-variable 'header-line-format)
               (propertize "*** Kite session closed ***"
                           'face 'kite-session-closed))))
      (remhash (websocket-url (kite-session-websocket kite-session))
               kite-active-sessions)
      (setq kite-active-session-list
            (remove kite-session kite-active-session-list))
      (kite--mode-line-update))))

(defun kite--connect-webservice (tab-alist)
  "Create a new kite session for the given browser tab.
TAB-ALIST is actually a plist that should contain the following
fields fetched from the remote debugger via
HTTP: :webSocketDebuggerUrl, :faviconUrl, :thumbnailUrl, :url,
and :title."
  (let ((websocket-url (plist-get tab-alist :webSocketDebuggerUrl)))

    (when (null websocket-url)
      (error "Internal error, null websocket-url"))

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

    (kite--mode-line-update)

    ;; Query capabilities

    (kite-send "Page.canOverrideDeviceMetrics"
               :success-function
               (lambda (result)
                 (setf (kite-session-can-override-device-metrics
                        kite-session)
                       (eq t (plist-get result :result)))))

    (kite-send "Page.canOverrideGeolocation"
               :success-function
               (lambda (result)
                 (setf (kite-session-can-override-geo-location
                        kite-session)
                       (eq t (plist-get result :result)))))

    (kite-send "Page.canOverrideDeviceOrientation"
               :success-function
               (lambda (result)
                 (setf (kite-session-can-override-device-orientation
                        kite-session)
                       (eq t (plist-get result :result)))))

    (kite-send "Network.canClearBrowserCache"
               :success-function
               (lambda (result)
                 (setf (kite-session-can-clear-browser-cache
                        kite-session)
                       (eq t (plist-get result :result)))))

    (kite-send "Network.canClearBrowserCookies"
               :success-function
               (lambda (result)
                 (setf (kite-session-can-clear-browser-cookies
                        kite-session)
                       (eq t (plist-get result :result)))))

    (kite-send "Debugger.canSetScriptSource"
               :success-function
               (lambda (result)
                 (setf (kite-session-can-set-script-source
                        kite-session)
                       (eq t (plist-get result :result)))))

    ;; Set default user preferences on remote side

    (kite--set-emulate-touch-events
     kite-default-emulate-touch-events)
    (kite--set-show-paint-rects
     kite-default-show-paint-rects)
    (kite--set-cache-disabled
     kite-default-disable-cache)

    ;; Enable subsystems

    (kite-send "Runtime.enable")
    (kite-send "Page.enable")
    (kite-send "Inspector.enable")
    (kite-send "Debugger.enable")
    (kite-send "CSS.enable")

    ;; Get initial state

    (kite--dom-initialize)

    (kite-send "Page.getResourceTree"
               :success-function
               (lambda (result)
                 (kite--log "got resource tree result: %s" result)
                 (setf (kite-session-frame-tree kite-session)
                       (plist-get result :frameTree))
                 (let ((console-buffer
                        (kite--find-buffer
                         (websocket-url
                          (kite-session-websocket kite-session))
                         'console)))
                   (when console-buffer
                     (with-current-buffer console-buffer
                       (kite--console-update-mode-line))))))))

(defun kite-connect (&optional host port)
  "Connect to the remote debugger instance running on the given
HOST and PORT using HTTP, retrieve a list of candidate tabs for
debugging, prompt the user to pick one, and create a new session
for the chosen tab.  Return the new session or nil if the user
enters the empty string at the prompt."
  (let* ((url-request-method "GET")
         (url-package-name "kite.el")
         (url-package-version "0.1")
         (url-http-attempt-keepalives nil)
         (use-host (or host kite-default-remote-host))
         (use-port (or port kite-default-remote-port))
         (url
          (url-parse-make-urlobj
           "http" nil nil use-host use-port "/json")))
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

            (mapc (lambda (el)
                    (when (plist-member el :webSocketDebuggerUrl)
                      (puthash
                       (plist-get el :webSocketDebuggerUrl)
                       (cons el nil)
                       available-debuggers)))
                  debugger-tabs)

            ;; Gather debuggers currently open

            (maphash
             (lambda (websocket-url kite-session)
               (puthash
                websocket-url
                `((:webSocketDebuggerUrl
                   ,websocket-url
                   :thumbnailUrl ,(kite-session-page-thumbnail-url
                                   kite-session)
                   :faviconUrl ,(kite-session-page-favicon-url
                                 kite-session)
                   :title ,(kite-session-page-title
                            kite-session)
                   :url ,(kite-session-page-url kite-session))
                  . ,kite-session)
                available-debuggers))
             kite-active-sessions)

            ;; For each human readable identifier (url or title), see
            ;; if it is ambiguous

            (flet
                ((add-item (item url)
                           (let ((existing (gethash item
                                                    available-strings
                                                    '(0))))
                 (puthash item
                          (cons (1+ (car existing))
                                (cons url (cdr existing)))
                          available-strings))))

             (maphash
              (lambda (key value)
                (let ((url (plist-get (car value) :url))
                      (title (plist-get (car value) :title))
                      (websocket-url
                       (plist-get (car value) :webSocketDebuggerUrl)))
                  (add-item url websocket-url)
                  (when (not (equal title url))
                    (add-item title websocket-url))))
              available-debuggers))

            ;; Final pass, disambiguate and rearrange

            (flet
             ((disambiguate
               (string websocket-url)
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
                (plist-get (car (gethash selection
                                         completion-strings))
                           :webSocketDebuggerUrl))))
        (error "\
Could not contact remote debugger at %s:%s, check host and port%s"
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
                (kite-session (gethash kite-most-recent-session
                                       kite-active-sessions)))
    (kite-send "Page.reload"
               :params
               (list :ignoreCache (if bool-prefix t :json-false))
               :success-function
               (lambda (result)
                 (if bool-prefix
                     (message "Page reloaded (with cache ignored)")
                   (message "Page reloaded"))))))

(defun kite--unique-session-name (title)
  "Create a unique name for the session, given the tab TITLE string.
May rename existing sessions.  FIXME: this currently just returns
the title and makes no attempt at uniquifying it."
  title)

(defvar kite-profiler-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "j" 'kite-javascript-profiler)
    (define-key map "c" 'kite-css-profiler)
    (define-key map "h" 'kite-heap-profiler)
    map)
  "The keymap associated with the profiler prefix key.")

(defvar kite-breakpoint-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "b" 'kite-toggle-next-instruction-breakpoint)
    (define-key map "e" 'kite-toggle-exception-breakpoint)
    (define-key map "x" 'kite-set-xhr-breakpoint)
    (define-key map "m" 'kite-set-dom-event-breakpoint)
    (define-key map "i" 'kite-set-instrumentation-breakpoint)
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
    (define-key map "s" 'kite-scratch)
    (define-key map "!" 'kite-reload-page)
    (define-key map (kbd "C-b") kite-breakpoint-keymap)
    map)
  "The keymap associated with the Kite prefix key.")

(global-set-key (kbd "C-c C-k") kite-global-keymap)

(defun kite--find-default-session (prefix)
  "Reuse an existing Kite session or create a new one, depending
on PREFIX.

If PREFIX is `(4)', create a new session for the default host and
port.

If PREFIX is `(16)', create a new session, prompting for host and
port.

Otherwise, if PREFIX is a number, use the session with that
index.

Otherwise, if any sessions are already open, reuse the most
recently used session.

Otherwise, create a new session using default host and port."
  (cond
   ((equal '(4) prefix)
    (kite-connect))
   ((equal '(16) prefix)
    (kite-connect
     (read-from-minibuffer "Host: "
                           kite-default-remote-host
                           nil
                           nil
                           'kite-remote-host-history
                           kite-default-remote-host)
     (let ((port (read-from-minibuffer
                  "Port: "
                  (number-to-string kite-default-remote-port)
                  nil
                  t
                  'kite-remote-port-history
                  (number-to-string kite-default-remote-port))))
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
   ((and (not (null kite-most-recent-session))
         (gethash kite-most-recent-session kite-active-sessions))
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
  "Find a session using `kite--find-default-session' and the
given PREFIX argument.  If this results in a vaid session, switch
to the buffer of the given TYPE for that session, creating it if
it doesn't exist yet."
  (let ((session (kite--find-default-session prefix)))
    (when session
      (kite--get-buffer-create session type))))

;;;###autoload
(defun kite-console (prefix)
  "Go to the Kite Console buffer for the session specified by
PREFIX.  Session and buffer are created as needed.  An existing
session is reused if possible, unless a prefix argument of (4) is
given in which case a new session is established.  With a prefix
of (16), Kite will prompt for remote host name and port.  With a
numeric prefix (1 or higher), Kite will reuse the Nth session,
where sessions are counted in the order in which they were
created."
  (interactive "P")
  (kite-maybe-goto-buffer prefix 'console))

;;;###autoload
(defun kite-debug (prefix)
  "Go to the Kite Debug buffer for the session specified by
PREFIX.  Session and buffer are created as needed.  An existing
session is reused if possible, unless a prefix argument of (4) is
given in which case a new session is established.  With a prefix
of (16), Kite will prompt for remote host name and port.  With a
numeric prefix (1 or higher), Kite will reuse the Nth session,
where sessions are counted in the order in which they were
created."
  (interactive "P")
  (kite-maybe-goto-buffer prefix 'debug))

;;;###autoload
(defun kite-dom (prefix)
  "Go to the Kite DOM buffer for the session specified by
PREFIX.  Session and buffer are created as needed.  An existing
session is reused if possible, unless a prefix argument of (4) is
given in which case a new session is established.  With a prefix
of (16), Kite will prompt for remote host name and port.  With a
numeric prefix (1 or higher), Kite will reuse the Nth session,
where sessions are counted in the order in which they were
created."
  (interactive "P")
  (kite-maybe-goto-buffer prefix 'dom))

;;;###autoload
(defun kite-network (prefix)
  "Go to the Kite Network buffer for the session specified by
PREFIX.  Session and buffer are created as needed.  An existing
session is reused if possible, unless a prefix argument of (4) is
given in which case a new session is established.  With a prefix
of (16), Kite will prompt for remote host name and port.  With a
numeric prefix (1 or higher), Kite will reuse the Nth session,
where sessions are counted in the order in which they were
created."
  (interactive "P")
  (kite-maybe-goto-buffer prefix 'network))

;;;###autoload
(defun kite-scratch (prefix)
  "Go to the Kite scratch buffer for the session specified by
PREFIX.  Session and buffer are created as needed.  An existing
session is reused if possible, unless a prefix argument of (4) is
given in which case a new session is established.  With a prefix
of (16), Kite will prompt for remote host name and port.  With a
numeric prefix (1 or higher), Kite will reuse the Nth session,
where sessions are counted in the order in which they were
created."
  (interactive "P")
  (kite-maybe-goto-buffer prefix 'scratch))

;;;###autoload
(defun kite-timeline (prefix)
  "Go to the Kite Timeline buffer for the session specified by
PREFIX.  Session and buffer are created as needed.  An existing
session is reused if possible, unless a prefix argument of (4) is
given in which case a new session is established.  With a prefix
of (16), Kite will prompt for remote host name and port.  With a
numeric prefix (1 or higher), Kite will reuse the Nth session,
where sessions are counted in the order in which they were
created."
  (interactive "P")
  (error "kite-timeline not yet implemented"))

;;;###autoload
(defun kite-javascript-profiler (prefix)
  "Go to the Kite JavaScript Profiler buffer for the session
specified by PREFIX.  Session and buffer are created as needed.
An existing session is reused if possible, unless a prefix
argument of (4) is given in which case a new session is
established.  With a prefix of (16), Kite will prompt for remote
host name and port.  With a numeric prefix (1 or higher), Kite
will reuse the Nth session, where sessions are counted in the
order in which they were created."
  (interactive "P")
  (error "kite-javascript-profiler not yet implemented"))

;;;###autoload
(defun kite-css-profiler (prefix)
  "Go to the Kite CSS Profiler buffer for the session specified
by PREFIX.  Session and buffer are created as needed.  An
existing session is reused if possible, unless a prefix argument
of (4) is given in which case a new session is established.  With
a prefix of (16), Kite will prompt for remote host name and port.
With a numeric prefix (1 or higher), Kite will reuse the Nth
session, where sessions are counted in the order in which they
were created."
  (interactive "P")
  (error "kite-css-profiler not yet implemented"))

;;;###autoload
(defun kite-heap-profiler (prefix)
  "Go to the Kite Heap Profiler buffer for the session specified
by PREFIX.  Session and buffer are created as needed.  An
existing session is reused if possible, unless a prefix argument
of (4) is given in which case a new session is established.  With
a prefix of (16), Kite will prompt for remote host name and port.
With a numeric prefix (1 or higher), Kite will reuse the Nth
session, where sessions are counted in the order in which they
were created."
  (interactive "P")
  (error "kite-heap-profiler not yet implemented"))

(defun kite-close-all-sessions ()
  "Close all active Kite sessions.  This is intended mostly for
debugging, since Kite should handle session closing
transparently."
  (interactive)
  (dolist (process (process-list))
    (when (string-match-p "^websocket to" (process-name process))
      (process-send-eof process)))
  (clrhash kite-active-sessions)
  (setq kite-active-session-list nil)
  (setq kite-most-recent-session nil)
  (kite--mode-line-update))

(defun kite--kill-buffer ()
  "Invoked by `kill-buffer-hook' for all Kite major modes. Kills
the current buffer, which should be a Kite buffer (one created by
Kite and using one of the Kite major modes), ignoring any errors
in the process."
  (ignore-errors
    (kite--session-remove-current-buffer)))

(defun kite-remember-recent-session ()
  "Invoked by `post-command-hook'. Remembers the most recently
used kite session."
  (setq kite-most-recent-session
        (or (when kite-session
              (websocket-url (kite-session-websocket kite-session)))
            kite-most-recent-session)))

(add-hook 'post-command-hook 'kite-remember-recent-session)

(defun kite--execution-context-created (websocket-url packet)
  "Callback invoked for the `Runtime.executionContextCreated'
notification, which the remote debugger sends when a new
JavaScript execution context is created."
  (let ((execution-context (plist-get packet :context)))
    (push execution-context
          (kite-session-execution-context-list kite-session))
    (when (null (kite-session-default-context kite-session))
      (setf (kite-session-default-context kite-session)
            execution-context)
      (setf (kite-session-current-context kite-session)
            execution-context))))

(defun kite--messageAdded (websocket-url packet)
  "Update session error count if message is on error level."
  (setf (kite-session-last-message kite-session)
        (plist-get packet :message))
  (when (string= "error" (kite--get packet :message :level))
    (incf (kite-session-error-count kite-session)))
  (kite--mode-line-update))

(defun kite--messageRepeatCountUpdated (websocket-url packet)
  "Update session error count for repeated errors."
  (when (string= "error"
                 (plist-get (kite-session-last-message kite-session)
                            :level))
    (incf (kite-session-error-count kite-session)))
  (kite--mode-line-update))

(defun kite--globalObjectCleared (websocket-url packet)
  "Reset session state.

FIXME: this needs to reset many more state properties."
  (setf (kite-session-error-count kite-session) 0)
  (clrhash (kite-session-source-map-cache kite-session))
  (setf (kite-session-default-context kite-session))
  (setf (kite-session-current-context kite-session))
  (kite--mode-line-update))

(add-hook 'kite-Runtime-executionContextCreated-hooks
          'kite--execution-context-created)
(add-hook 'kite-Console-messageAdded-hooks
          'kite--messageAdded)
(add-hook 'kite-Console-messageRepeatCountUpdated-hooks
          'kite--messageRepeatCountUpdated)
(add-hook 'kite-Debugger-globalObjectCleared-hooks
          'kite--globalObjectCleared)

(defun kite--json-truth (t-or-nil)
  (if t-or-nil t :json-false))

(defun kite--set-remote-setting (method
                                 param-key
                                 enabledp
                                 &optional enable-function)
  "Enable or disable a remote setting in the session identified
by `kite-session', using the given JSON-RPC METHOD and depending
on ENABLEDP.  On success, call ENABLE-FUNCTION if given, with
ENABLEDP as the only argument."
  (lexical-let ((lex-enabledp enabledp)
                (lex-enable-function enable-function))
    (kite-send
     method
     :params `(,param-key ,(kite--json-truth enabledp))
     :success-function
     (lambda (result)
       (when lex-enable-function
         (funcall lex-enable-function lex-enabledp))))))

(defalias 'kite--set-emulate-touch-events
  (apply-partially 'kite--set-remote-setting
                   "Page.setTouchEmulationEnabled"
                   :enabled))

(defalias 'kite--set-show-paint-rects
  (apply-partially 'kite--set-remote-setting
                   "Page.setShowPaintRects"
                   :result))

(defalias 'kite--set-cache-disabled
  (apply-partially 'kite--set-remote-setting
                   "Network.setCacheDisabled"
                   :cacheDisabled))

(defun kite-toggle-emulate-touch-events ()
  "Toggle whether touch emulation is enabled in the current or
most recent session."
  (interactive)
  (let ((kite-session (kite--select-session)))
    (kite--set-emulate-touch-events
     (not (kite-session-emulate-touch-events kite-session))
     (lambda (enabledp)
       (setf (kite-session-emulate-touch-events kite-session)
             enabledp)
       (message "Touch emulation is %s"
                (if enabledp "enabled" "disabled"))))))

(defun kite-toggle-show-paint-rects ()
  "Toggle whether paint rectangles are shown in the current or
most recent session."
  (interactive)
  (let ((kite-session (kite--select-session)))
    (kite--set-show-paint-rects
     (not (kite-session-show-paint-rects kite-session))
     (lambda (enabledp)
       (setf (kite-session-show-paint-rects kite-session) enabledp)
       (message "Paint rectangles are %s"
                (if enabledp "shown" "hidden"))))))

(defun kite-toggle-cache ()
  "Toggle whether the cache is enabled in the current or most
recent session."
  (interactive)
  (let ((kite-session (kite--select-session)))
    (kite--set-cache-disabled
     (not (kite-session-disable-cache kite-session))
     (lambda (enabledp)
       (setf (kite-session-disable-cache kite-session) enabledp)
       (message "Cache is %s"
                (if enabledp "disabled" "enabled"))))))

(defun kite--reload ()
  "Force reloading kite and all dependent modules after closing
any running sessions.

FIXME: there must be a more elegant way to do this."
  (interactive)
  (kite-close-all-sessions)
  (unload-feature 'kite)
  (unload-feature 'kite-debug)
  (unload-feature 'kite-dom)
  (unload-feature 'kite-memory)
  (unload-feature 'kite-net)
  (unload-feature 'kite-scratch)
  (unload-feature 'kite-object)
  (unload-feature 'kite-console)
  (unload-feature 'kite-breakpoint)
  (unload-feature 'kite-modeline)
  (unload-feature 'kite-sourcemap)
  (unload-feature 'kite-global)

  ;; Quote this way to avoid the dependency generator picking it up
  (require (quote kite)))

(provide 'kite)

;;; kite.el ends here
