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
(require 'kite-files)
(require 'cl)
(require 'url-expand)
(require 'ewoc)
(require 'wid-edit)

(defface kite-call-frame-face
  '((t (:inherit highlight-current-line)))
  "Face used to highlight the current call frame."
  :group 'kite-highlighting-faces)

(defface kite-script-breakpoint-face
  '((t (:inherit warning)))
  "Face used to highlight breakpoint markers."
  :group 'kite-highlighting-faces)

(defvar kite-stack-mode-map
  (let ((map (make-composed-keymap
              (copy-keymap widget-keymap)
              (copy-keymap special-mode-map))))
    (suppress-keymap map t)
    (kite--define-global-mode-keys map)
    map)
  "Local keymap for `kite-stack-mode' buffers.")

(defun kite--kill-stack-buffer ()
  "If the stack buffer still lives in the window that was created
for it, delete the window."
  (let ((kite-stack-window
         (window-with-parameter
          'kite-stack-buffer
          (current-buffer))))
    (when (and kite-stack-window
               (eq (window-buffer kite-stack-window)
                   (current-buffer)))
      (ignore-errors
        (delete-window kite-stack-window)))))

(define-derived-mode kite-stack-mode special-mode "kite-stack"
  "Toggle kite stack mode."
  :group 'kite
  (setq kite-buffer-type 'stack)
  (setq buffer-read-only nil)
  (set (make-local-variable 'paragraph-start) "^[^ ]")
  (add-hook 'kill-buffer-hook 'kite--kill-stack-buffer nil t)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (remove-overlays))
  (setq widget-global-map
        (let ((map (copy-keymap widget-global-map)))
          (define-key map (kbd "RET") 'kite--object-toggle-disclosure)
          map))
  (run-mode-hooks 'kite-stack-mode-hook))

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
  (let ((kite-stack-buffer
         (kite--find-buffer websocket-url 'stack)))
    (when kite-stack-buffer
      (kill-buffer kite-stack-buffer)))
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
      (if (not (string= "" (plist-get call-frame :functionName)))
          (plist-get call-frame :functionName)
        "(anonymous function)")
      " ("
      (kite-script-info-url
       (gethash
        (kite--get call-frame
                   :location
                   :scriptId)
        (kite-session-script-infos kite-session)))
      ":"
      (number-to-string (1+ (kite--get call-frame
                                       :location
                                       :lineNumber)))
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
                 (data (plist-get packet :data))
                 (reason (plist-get packet :reason))
                 (first-call-frame (elt call-frames 0))
                 (location (plist-get first-call-frame :location))
                 (script-info
                  (gethash (plist-get location :scriptId)
                           (kite-session-script-infos kite-session)))
                 (line-number (1+ (plist-get location :lineNumber)))
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
              (stack-buffer (kite--get-buffer-create
                             (websocket-url
                              (kite-session-websocket kite-session))
                             'stack))
              (window (display-buffer
                       stack-buffer
                       (list (cons 'display-buffer-pop-up-window
                                   nil)))))
         (set-window-parameter window
                               'kite-stack-buffer
                               stack-buffer)
         (with-current-buffer stack-buffer
           (set (make-local-variable 'kite-session) kite-session)
           (set (make-local-variable 'widget-link-prefix) "")
           (set (make-local-variable 'widget-link-suffix) "")
           (let ((inhibit-read-only t))
             (erase-buffer)
             (save-excursion
               (case (intern reason)
                 (other
                  (insert "Execution paused"))
                 (EventListener
                  (insert (format "DOM event breakpoint: %s"
                                  (plist-get data :eventName))))
                 (exception
                  (overlay-put
                   (widget-get
                    (kite--insert-object-widget
                     (kite--get data :objectId)
                     (kite--get data :description)
                     0)
                    :button-overlay)
                   'face 'error)))
               (widget-insert "\n\n")
               (lexical-let (all-call-frame-widgets)
                 (mapc (lambda (call-frame)
                         (push
                          (kite--insert-call-frame-widget
                           call-frame
                           current-window
                           (lambda (widget)
                             (mapc (lambda (other-widget)
                                     (overlay-put
                                      (widget-get other-widget
                                                  :button-overlay)
                                      'face nil))
                                   all-call-frame-widgets)
                             (overlay-put
                              (widget-get widget :button-overlay)
                              'face 'kite-call-frame-face)))
                          all-call-frame-widgets)
                         (unless (cdr all-call-frame-widgets)
                           (overlay-put
                            (widget-get (car all-call-frame-widgets)
                                        :button-overlay)
                            'face 'kite-call-frame-face))
                         (mapc (lambda (scope)
                                 (kite--insert-object-widget
                                  (kite--get scope :object :objectId)
                                  (concat "  "
                                          (kite--get scope :type))
                                  2)
                                 (widget-insert "\n"))
                               (plist-get call-frame :scopeChain)))
                       call-frames))
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

(defun kite-set-breakpoint-at-point ()
  "Set a location breakpoint at point.

TODO: displaying the breakpoint in the buffer should not be done
here but in a separate function which can be reused to display
existing breakpoints in newly visited buffers."
  (interactive)
  (let ((kite-session (kite--select-session)))
    (lexical-let
        ((breakpoint
          (make-kite-location-breakpoint
           :location (list :scriptId kite-script-id
                           :lineNumber (save-restriction
                                         (widen)
                                         (1- (line-number-at-pos
                                              (point))))
                           :columnNumber (current-column))
           :condition nil)))
      (kite--session-add-breakpoint
       kite-session
       breakpoint
       (lambda (result)
         (let* ((location (kite-location-breakpoint-actual-location
                           breakpoint))
                (location-point
                 (save-excursion
                   (widen)
                   (goto-char 0)
                   (forward-line (plist-get location :lineNumber))
                   (forward-char (plist-get location :columnNumber))
                   (point)))
                (breakpoint-overlay
                 (make-overlay location-point location-point)))
           (overlay-put breakpoint-overlay
                        'face
                        'kite-node-highlight-face)
           (overlay-put breakpoint-overlay
                        'before-string
                        (propertize
                         "<BREAK>"
                         'face
                         'kite-script-breakpoint-face))))))))

(defun kite-set-script-source ()
  "Set the contents of the kite script overlay at point as the
new source for the corresponding script.  Causes
`Debugger.setScriptSource' to be sent to the remote debugger."
  (interactive)
  (if (kite-session-can-set-script-source kite-session)
      (mapc (lambda (overlay)
              (kite-send
               "Debugger.setScriptSource"
               :params
               (list :scriptId kite-script-id
                     :scriptSource (save-restriction
                                     (widen)
                                     (buffer-substring
                                      (overlay-start overlay)
                                      (overlay-end overlay)))
                     :preview :json-false)
               :success-function
               (lambda (result)
                 ;; FIXME: use :callFrames to update context
                 ;; information
                 (set (make-local-variable
                       'kite-set-script-source-tick)
                      (buffer-chars-modified-tick)))))
            (remove-if (lambda (overlay)
                         (null (overlay-get overlay 'kite-script-id)))
                       (overlays-in (point-min) (point-max))))
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

(defun kite--debug-stats-mode-line-indicator ()
  "Returns a string to be displayed in the mode line"
  (concat " (" (kite-session-debugger-state kite-session) ")"))

(defun kite-session-script-infos-for-url (url)
  "Return a list of the script-info entries for the given URL in
the session bound to `kite-session'."
  (let (result)
    (maphash (lambda (key value)
               (when (string= url (kite-script-info-url value))
                 (push value result)))
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
   (car
    (kite-session-script-infos-for-url
     (plist-get stack-frame-plist :url)))
   (plist-get stack-frame-plist :lineNumber)
   (plist-get stack-frame-plist :columnNumber)))

(provide 'kite-debug)

;;; kite-debug.el ends here
