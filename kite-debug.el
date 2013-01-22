;;; kite-debug.el --- Kite debugger module implementation

;; Copyright (C) 2012 Julian Scheid

;; Author: Julian Scheid <julians37@gmail.com>
;; Keywords: tools
;; Package: kite
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

;; This package implements the WebKit debugger buffer, which is used
;; to manage breakpoints.
;;
;; It is part of Kite, a WebKit inspector front-end.


;;; Code:

(require 'kite-breakpoint)
(require 'kite-sourcemap)
(require 'kite-global)
(require 'kite-object)
(require 'cl)
(require 'url-expand)
(require 'ewoc)
(require 'wid-edit)

(defvar kite-script-id nil
  "Keeps the scriptId in a buffer-local variable in buffers that
correspond to one.")

(defvar kite-debug-mode-map
  (let ((map (make-keymap))
	(ctl-c-b-map (make-keymap))
	(menu-map (make-sparse-keymap)))
    (suppress-keymap map t)
    (kite--define-global-mode-keys map)
    (define-key map "C" 'kite-console)
    (define-key map "p" 'kite-toggle-next-instruction-breakpoint)
    (define-key map "b" 'kite-toggle-exception-breakpoint)
    (define-key map "c" 'kite-debug-continue)
    (define-key map "r" 'kite-debug-reload)
    (define-key map "R" 'kite-repl)
    (define-key map "D" 'kite-dom-inspect)
    (define-key map "N" 'kite-network)
    (define-key map "T" 'kite-timeline)
    (define-key map "M" 'kite-memory)
    (define-key mode-specific-map "b" ctl-c-b-map)
    (define-key ctl-c-b-map "x" 'kite-set-xhr-breakpoint)
    (define-key ctl-c-b-map "d" 'kite-set-dom-event-breakpoint)
    (define-key ctl-c-b-map "i" 'kite-set-instrumentation-breakpoint)
    (define-key ctl-c-b-map "b" 'kite-toggle-exception-breakpoint)
    (define-key
      ctl-c-b-map "p" 'kite-toggle-next-instruction-breakpoint)
    map)
  "Local keymap for `kite-connection-mode' buffers.")

(defvar kite-connection-ewoc)

(define-derived-mode kite-debug-mode special-mode "kite-debug"
  "Toggle kite debug mode."
  :group 'kite
  (setq case-fold-search nil)
  (kite--connect-buffer-insert)
  (run-mode-hooks 'kite-debug-mode-hook))

(defun kite-debug-pause ()
  (interactive)
  (kite-send "Debugger.pause"))

(defun kite-debug-continue ()
  (interactive)
  (kite-send "Debugger.resume"))

(defun kite-debug-reload ()
  (interactive)
  (with-current-buffer (if (boundp 'kite-connection)
                           kite-connection
                         (current-buffer))
    (kite-send "Page.reload")))

(defun kite--insert-favicon-async (favicon-url)
  (lexical-let ((favicon-marker (point-marker))
                (buf (current-buffer)))
    (url-retrieve
     favicon-url
     (lambda (status)
       (goto-char 0)
       (when (and (looking-at "HTTP/1\\.. 200")
                  (re-search-forward "\n\n" nil t))
         (ignore-errors
           (let* ((favicon-image
                   (create-image (buffer-substring (point)
                                                   (buffer-size))
                                 nil
                                 t)))
             (save-excursion
               (with-current-buffer buf
                 (goto-char (marker-position favicon-marker))
                 (let ((inhibit-read-only t))
                   (insert-image favicon-image)))))))))))

(defun kite--connect-buffer-insert ()
  (let ((favicon-url (kite-session-page-favicon-url kite-session)))
    (when (and favicon-url
               (not (string= favicon-url "")))
      (kite--insert-favicon-async favicon-url))

    (let* ((inhibit-read-only t)
           (ewoc (ewoc-create
                  (lambda (session)
                    (insert (propertize
                             (concat " "
                                     (kite-session-page-title
                                      kite-session)
                                     "\n\n")
                             'face 'info-title-1)
                            (propertize "URL: " 'face 'bold)
                            (kite-session-page-url kite-session)
                            "\n"
                            (propertize "Status: " 'face 'bold)
                            (kite-session-debugger-state session)
                            "\n\n"
                            "Press ? for help\n")))))

      (set (make-local-variable 'kite-connection-ewoc) ewoc)

      (ewoc-enter-last ewoc kite-session)

      (goto-char (point-max)))))

(defun kite--Debugger-resumed (websocket-url packet)
  (kite-send "Debugger.setOverlayMessage")
  (message "Execution resumed"))

(defun kite--insert-call-frame-widget (call-frame
                                       target-window
                                       activate-function)
  "Insert a widget whose label describes CALL-FRAME using format
`FUNCTION (URL:LINE:COLUMN)' and that visits the call frame
location in TARGET-WINDOW when activated.  If ACTIVE-FUNCTION is
not nil, invoke it with the widget as only argument when the
widget is activated."
  (lexical-let
      ((-call-frame call-frame)
       (-target-window target-window)
       (-activate-function activate-function))
    (widget-create
     'link
     :size 1
     :offset 0
     :notify (lambda (widget &rest ignore)
               (with-selected-window -target-window
                 (kite-visit-location
                  (plist-get -call-frame :location)))
               (when -activate-function
                 (funcall -activate-function widget)))
     (concat
      (plist-get call-frame :functionName)
      " ("
      (kite-script-info-url
       (gethash
        (kite--get call-frame
                   :location
                   :scriptId)
        (kite-session-script-infos kite-session)))
      ":"
      (number-to-string (kite--get call-frame
                                   :location
                                   :lineNumber))
      ":"
      (number-to-string (kite--get call-frame
                                   :location
                                   :columnNumber))
      ")\n"))))


(defun kite--Debugger-paused (websocket-url packet)
  (kite-send "Debugger.setOverlayMessage"
             :params
             (list :message "Paused in kite debugger"))

  (lexical-let* ((call-frames (plist-get packet :callFrames))
                 (first-call-frame (elt call-frames 0))
                 (location (plist-get first-call-frame :location))
                 (script-info
                  (gethash (plist-get location :scriptId)
                           (kite-session-script-infos kite-session)))
                 (line-number (plist-get location :lineNumber))
                 (column-number (plist-get location :columnNumber))
                 (kite-session (gethash websocket-url
                                        kite-active-sessions)))
    (kite-visit-script
     script-info
     line-number
     column-number
     (lambda ()
       (kite-script-mode)
       (set (make-local-variable 'kite-script-id)
            (plist-get location :scriptId))
       (set (make-local-variable 'kite-session)
            kite-session)

       (let* ((current-window (selected-window))
              (stack-buffer (get-buffer-create "*kite stack*"))
              (window (display-buffer
                       stack-buffer
                       (list (cons 'display-buffer-pop-up-window
                                   nil)))))
         (with-current-buffer stack-buffer
           (special-mode)
           (set (make-local-variable 'kite-session) kite-session)
           (set (make-local-variable 'widget-link-prefix) "")
           (set (make-local-variable 'widget-link-suffix) "")
           (let ((inhibit-read-only t))
             (erase-buffer)
             (save-excursion
               (mapc (lambda (call-frame)
                       (kite--insert-call-frame-widget call-frame
                                                       current-window)
                       (mapc (lambda (scope)
                               (kite--insert-object-widget
                                (kite--get scope :object :objectId)
                                (concat "  " (kite--get scope :type))
                                2)
                               (insert "\n"))
                             (plist-get call-frame :scopeChain)))
                     call-frames)
               (use-local-map widget-keymap)
               (widget-setup)))))
       (message "Execution paused")))))

(defun kite--Debugger-scriptParsed (websocket-url packet)
  (puthash
   (plist-get packet :scriptId)
   (make-kite-script-info
    :id (plist-get packet :scriptId)
    :url (plist-get packet :url)
    :start-line (plist-get packet :startLine)
    :start-column (plist-get packet :startColumn)
    :end-line (plist-get packet :endLine)
    :end-column (plist-get packet :endColumn)
    :source-map-url (plist-get packet :sourceMapURL))
   (kite-session-script-infos kite-session)))

(add-hook 'kite-Debugger-paused-hooks
          'kite--Debugger-paused)
(add-hook 'kite-Debugger-resumed-hooks
          'kite--Debugger-resumed)
(add-hook 'kite-Debugger-scriptParsed-hooks
          'kite--Debugger-scriptParsed)


;;; Augmented javascript-mode; loading of remote .js files

(defvar kite-script-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c i") 'kite-step-into)
    (define-key map (kbd "C-c o") 'kite-step-over)
    (define-key map (kbd "C-c u") 'kite-step-out)
    (define-key map (kbd "C-c r") 'kite-resume)
    (define-key map (kbd "C-c c") 'kite-continue-to-location)
    (define-key map (kbd "C-c C-c") 'kite-set-script-source)
    map)
  "Local keymap for the `kite-script-mode' minor mode")

(defvar kite--script-mode-line-element
  '(:eval (kite--script-buffer-changed))
  "Indicator used in mode-line-modified to show whether buffer
changes have been sent to the server.")

(define-minor-mode kite-script-mode
  "Toggle kite JavaScript debugging in this buffer."
  :group 'kite
  :lighter (:eval (kite--debug-stats-mode-line-indicator))
  :keymap kite-script-mode-map
  (if kite-script-mode
      (add-to-list 'mode-line-modified
                   kite--script-mode-line-element
                   t)
    (setq mode-line-modified
          (remq kite--script-mode-line-element
                mode-line-modified))))

(defun kite--script-buffer-changed ()
  "Return a marker indicating whether changes to the buffer have
been sent to the server."
  (if (or (not (boundp 'kite-set-script-source-tick))
          (< kite-set-script-source-tick
             (buffer-chars-modified-tick)))
      "*"
    "-"))

(defun kite-step-into ()
  "Step into the next instruction in the current or most recent
session.  Sends `Debugger.stepInto' to the remote debugger."
  (interactive)
  (let ((kite-session (kite--select-session)))
    (kite-send "Debugger.stepInto")))

(defun kite-step-over ()
  "Step over the next instruction in the current or most recent
session.  Sends `Debugger.stepOver' to the remote debugger."
  (interactive)
  (let ((kite-session (kite--select-session)))
    (kite-send "Debugger.stepOver")))

(defun kite-step-out ()
  "Step out of the current block in the current or most recent
session.  Sends `Debugger.stepOut' to the remote debugger."
  (interactive)
  (let ((kite-session (kite--select-session)))
    (kite-send "Debugger.stepOut")))

(defun kite-resume ()
  "Resume execution in the current or most recent session.  Sends
`Debugger.resume' to the remote debugger."
  (interactive)
  (let ((kite-session (kite--select-session)))
    (kite-send "Debugger.resume")))

(defun kite-continue-to-location ()
  "Continue to the instruction at or after point.  Sends
`Debugger.continueToLocation' to the remote debugger."
  (interactive)
  (let ((kite-session (kite--select-session)))
    (kite-send "Debugger.continueToLocation"
               :params
               (list :location
                     (list :scriptId kite-script-id
                           :lineNumber (save-restriction
                                         (widen)
                                         (line-number-at-pos (point)))
                           :columnNumber (current-column))))))

(defun kite-set-script-source ()
  "Send the buffer contents as the new contents for the script.
Causes `Debugger.setScriptSource' to be sent to the remote
debugger."
  (interactive)
  (if (kite-session-can-set-script-source kite-session)
      (kite-send "Debugger.setScriptSource"
                 :params
                 (list :scriptId kite-script-id
                       :scriptSource (save-restriction
                                       (widen)
                                       (buffer-string))
                       :preview :json-false)
                 :success-function
                 (lambda (result)
                   ;; FIXME: use :callFrames to update context
                   ;; information
                   (set (make-local-variable
                         'kite-set-script-source-tick)
                        (buffer-chars-modified-tick))))
    (message "Sorry, the remote debugger doesn't support setting\
 the script source")))

(defun kite--create-remote-script-buffer (script-info
                                          after-load-function)
  (lexical-let* ((url (kite-script-info-url script-info))
                 (url-parts (url-generic-parse-url url))
                 (after-load-function after-load-function)
                 (new-buffer (generate-new-buffer url)))
    (kite-send "Debugger.getScriptSource"
               :params
               (list :scriptId (plist-get script-info :scriptId))
               :success-function
               (lambda (result)
                 (with-current-buffer new-buffer
                   (setq buffer-file-name (url-filename url-parts))
                   (insert (plist-get result :scriptSource))
                   (setq buffer-read-only t)
                   (set-buffer-modified-p nil)
                   (normal-mode)
                   (funcall after-load-function))))
    new-buffer))

(defun kite-script-info--source-map (script-info)
  "Return the parsed source map for the given SCRIPT-INFO as a
`kite-source-map' struct, or nil if there is no source map for
the SCRIPT-INFO.  Raise an error if the source map can't be
loaded or parsed."
  (when (kite-script-info-source-map-url script-info)
    (with-current-buffer
        (url-retrieve-synchronously
         (url-expand-file-name
          (kite-script-info-source-map-url script-info)
          (kite-script-info-url script-info)))
      (goto-char 0)
      (if (and (or (looking-at "HTTP/1\\.. 200")
                   (not (looking-at "HTTP/")))
               (re-search-forward "\n\n" nil t))
          (kite--source-map-decode
           (let ((json-array-type 'list)
                 (json-object-type 'plist))
             (json-read)))
        (error "Could not retrieve source map: %s"
               (buffer-substring-no-properties
                (point-min) (point-max)))))))

(defun kite-script-info--source-map-cached (script-info)
  "Return the parsed source map for the given SCRIPT-INFO as a
`kite-source-map' struct, or nil if there is no source map for
the SCRIPT-INFO.  Raise an error if the source map can't be
loaded or parsed.  Uses a cache in the session so that each
source map is loaded and parsed only once."
  (when (kite-script-info-source-map-url script-info)
    (let ((cached-entry
           (gethash (kite-script-info-source-map-url script-info)
                    (kite-session-source-map-cache kite-session))))
      (cond
       ((kite-source-map-p cached-entry)
        cached-entry)
       ((consp cached-entry)
        (signal (car cached-entry) (cdr cached-entry)))
       (t
        (condition-case err
            (puthash (kite-script-info-source-map-url script-info)
                     (kite-script-info--source-map script-info)
                     (kite-session-source-map-cache kite-session))
          (error
           (puthash (kite-script-info-source-map-url script-info)
                    err
                    (kite-session-source-map-cache kite-session))
           (signal (car err) (cdr err)))))))))

(defun kite-script-info--original-source (script-info line column)
  "Return original URL, line, and column corresponding to the
given SCRIPT-INFO, LINE, and COLUMN.  The original location is
returned as a plist with keys `:url', `:line' and `:column'."
  (let ((source-map
         (condition-case err
             (kite-script-info--source-map-cached script-info)
           (error
            ;; In case of error, display error and fall back to
            ;; generated source
            (message (cdr err))
            nil))))
    (if source-map
        (let ((original-pos
               (kite-source-map-original-position-for
                source-map
                line
                column)))
          (list :url
                (url-expand-file-name
                 (plist-get original-pos :source)
                 (kite-script-info-url script-info))
                :line (plist-get original-pos :line)
                :column (plist-get original-pos :column)))
      (list :url (kite-script-info-url script-info)
            :line line
            :column column))))

(defun kite--debug-stats-mode-line-indicator ()
  "Returns a string to be displayed in the mode line"
  (concat " (" (kite-session-debugger-state kite-session) ")"))

(defun kite-session-script-info-for-url (url)
  "Return the script-info entry for the given URL in the session
bound to `kite-session', or nil if not found."
  (let (result)
    (maphash (lambda (key value)
               (when (string= url (kite-script-info-url value))
                 (setq result value)))
             (kite-session-script-infos kite-session))
    result))

(defun kite-visit-location (location-plist)
  "Visit the source file corresponding to the call frame given by
LOCATION-PLIST, which should be a plist with at least the
properties `:scriptId', `:lineNumber' and `:columnNumber'.  The
variable `kite-session' should be bound to the session in which
to visit the source file."
  (kite-visit-script
   (gethash
    (plist-get location-plist :scriptId)
    (kite-session-script-infos kite-session))
   (1+ (plist-get location-plist :lineNumber))
   (plist-get location-plist :columnNumber)))

(defun kite-visit-stack-frame (stack-frame-plist)
  "Visit the source file corresponding to the stack frame given
by STACK-FRAME-PLIST, which should be a plist with at least the
properties `:url', `:lineNumber' and `:columnNumber'.  The
variable `kite-session' should be bound to the session in which
to visit the source file."
  (kite-visit-script
    (kite-session-script-info-for-url
     (plist-get stack-frame-plist :url))
    (plist-get stack-frame-plist :lineNumber)
    (plist-get stack-frame-plist :columnNumber)))

(provide 'kite-debug)

;;; kite-debug.el ends here
