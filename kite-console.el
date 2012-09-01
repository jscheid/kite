;;; kite-console.el --- Kite console module implementation

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

;; This package implements the WebKit inspector console.
;;
;; It is part of Kite, a WebKit inspector front-end.


;;; Code:

(require 'kite-global)
(require 'font-lock)

(defface kite-log-warning
  '((t :inherit warning))
  "Basic face used to highlight warnings."
  :version "24.1"
  :group 'kite-faces)

(defface kite-log-error
  '((t :inherit error))
  "Basic face used to highlight errors."
  :version "24.1"
  :group 'kite-faces)

(defface kite-log-debug
  '((t :inherit font-lock-comment))
  "Basic face used to highlight debug-level messages."
  :version "24.1"
  :group 'kite-faces)

(defface kite-log-log
  '((t :inherit default))
  "Basic face used to highlight regular messages."
  :version "24.1"
  :group 'kite-faces)

(defface kite-object
  '((t :inherit font-lock-variable-name))
  "Face used to highlight object references."
  :version "24.1"
  :group 'kite-faces)

(defface kite-number
  '((t :inherit nxml-char-ref-number))
  "Face used to highlight numbers."
  :version "24.1"
  :group 'kite-faces)

(defface kite-boolean
  '((t :inherit nxml-char-ref-number))
  "Face used to highlight boolean values."
  :version "24.1"
  :group 'kite-faces)

(defface kite-null
  '((t :inherit nxml-char-ref-number))
  "Face used to highlight null values."
  :version "24.1"
  :group 'kite-faces)

(defface kite-string
  '((t :inherit font-lock-string))
  "Face used to highlight strings."
  :version "24.1"
  :group 'kite-faces)

(defface kite-quote
  '((t :inherit font-lock-keyword))
  "Face used to highlight quotes around strings."
  :version "24.1"
  :group 'kite-faces)

(defface kite-loading
  '((t :inherit font-lock-comment))
  "Face used to highlight loading indicator."
  :version "24.1"
  :group 'kite-faces)

(defface kite-property-name
  '((t :inherit default))
  "Face used to highlight null values."
  :version "24.1"
  :group 'kite-faces)

(defface kite-proto-property-name
  `((t :inherit default
       :foreground ,(kite--dimmed-face-foreground 'default 0.5)))
  "Face used to highlight null values."
  :version "24.1"
  :group 'kite-faces)

(defcustom kite-console-log-max 1000
  "Maximum number of lines to keep in the kite console log buffer.
If nil, disable console logging.  If t, log messages but don't truncate
the buffer when it becomes large.")

(defvar kite-console-mode-map
  (let ((map (make-composed-keymap widget-keymap special-mode-map))
	(menu-map (make-sparse-keymap)))
    (suppress-keymap map t)
    (kite--define-global-mode-keys map)
    (define-key map "X" 'kite-clear-console)
    (define-key map "s" 'kite-console-visit-source)
    ;(define-key map (kbd "RET") 'kite-show-log-entry)
    map)
  "Local keymap for `kite-console-mode' buffers.")

(define-derived-mode kite-console-mode special-mode "kite-console"
  "Toggle kite console mode."
  :group 'kite
  (add-hook 'kill-buffer-hook 'kite--kill-console nil t)
  (set (make-local-variable 'kite-message-group-level) 0)
  (set (make-local-variable 'kite-console-line-count) 0)
  (hl-line-mode)
  (setq case-fold-search nil)
  (set (make-local-variable 'widget-link-prefix) "")
  (set (make-local-variable 'widget-link-suffix) "")
  (add-hook (make-local-variable 'kite-after-mode-hooks)
            (lambda ()
              (kite-send "Console.enable" nil
                         (lambda (response) (kite--log "Console enabled.")))))
  (run-mode-hooks 'kite-console-mode-hook))

(defun kite--kill-console ()
  "Called when a session's console buffer is closed.  Disables
console logging in the remote debugger by sending the
`Console.disable' message."
  (ignore-errors
    (kite-send "Console.disable" nil
               (lambda (response) (kite--log "Console disabled.")))))

(defun kite--message-repeat-text (repeat-count)
  "Return a string to be used as a suffix for messages with the
given REPEAT-COUNT.  Return nil for a repeat count less than or
equal to 1, a human-readable string otherwise.  The returned
string has the `kite-repeat-count' text property set so that it
can be updated later on."
  (and repeat-count
       (> repeat-count 1)
       (propertize
        (format " [message repeated %d times]" repeat-count)
        'kite-repeat-count t)))

(defun kite--console-replace-object-async
  (response object-plist buffer-point longp)
  "Replace a previously inserted simple object representation
with a more detailed representation after receiving additional
data from the server.  RESPONSE is the JSON-RPC response received
from the server.  OBJECT-PLIST is a plist describing the message
parameter for which the request was sent.  BUFFER-POINT is a
marker at which the temporary placeholder is located."
  (let* ((text-prop-start (text-property-any buffer-point
                           (point-max)
                           'kite-loading-object-id
                           (plist-get object-plist :objectId)))
         (text-prop-end (next-single-property-change
                         text-prop-start
                         'kite-loading-object-id))
         (log-message (get-text-property text-prop-start 'log-message)))

    (when (and text-prop-start text-prop-end)
      (let ((inhibit-read-only t))
        (save-excursion
          (delete-region text-prop-start text-prop-end)
          (goto-char text-prop-start)
          (cond
           ((string= (plist-get object-plist :subtype) "array")
            (insert (kite--format-array
                     (plist-get (plist-get response :result) :result))))
           ((string= (plist-get object-plist :subtype) "node")
            (insert (propertize
                     (plist-get object-plist :description)
                     'face 'kite-object)))
           ((null (plist-get object-plist :subtype))
            (widget-create
             'link
             :kite-object-id (plist-get object-plist :objectId)
             :kite-object-description (plist-get object-plist :description)
             :button-face 'kite-object
             :notify (lambda (widget &rest ignore)
                       (kite-inspect-object
                        (widget-get widget :kite-object-id)
                        (widget-get widget :kite-object-description)))
             (if longp
                 (kite--format-object-with-props
                  (plist-get (plist-get response :result) :result))
               (plist-get object-plist :description))))
           (t
            (insert "UNKNOWN")))
          (put-text-property text-prop-start
                             (point)
                             'log-message
                             log-message)))))
  (widget-setup))

(defun kite--console-format-object (object-plist &optional longp)
  "Return a propertized string representation of OBJECT-PLIST,
where OBJECT-PLIST is a raw short object description plist as
sent by the remote debugger, for example as part of a log message
record.

If LONGP is t, show a more detailed description of the object by
including its properties.

For JavaScript objects and arrays, additional data is fetched
from the remote debugger asynchronously and the returned
representation will eventually be replaced with a more detailed
one by calling `kite--console-replace-object-async'.

The returned string must be inserted into the current buffer so
that `kite--console-replace-object-async' can locate it for
replacement."
  (when (and (not (plist-member object-plist :result))
             (plist-member object-plist :objectId))
    (lexical-let ((object-plist object-plist)
                  (longp longp)
                  (buffer-point (point-marker)))
      ;; Optimization: receiving an array with thousands or even
      ;; millions of elements, or an object with thousands of
      ;; properties can take a very long time, most of all because of
      ;; JSON serialization.  Therefore, only fetch the
      ;; elements/properties necessary for printing a compact
      ;; representation.
      (kite-send
       "Runtime.callFunctionOn"
       `((objectId . ,(plist-get object-plist :objectId))
         (functionDeclaration
          . ,(format "\
function f() { \
  if (this instanceof Array) { \
    return this.slice(0, %d); \
  } \
  else if (this instanceof Object) {
    obj = {}; \
    count = 0; \
    for (key in this) { \
      obj[key] = this[key]; \
      if (++count >= %d) break; \
    }; \
    return obj; \
  } \
  else { \
    return this; \
  } \
}"
                    ;; Fetch one item more than necessary so that
                    ;; kite--format-array and
                    ;; kite--format-object-with-props know when to
                    ;; insert an ellipsis at the end.
                    (1+ kite-short-array-max-elements)
                    (1+ kite-short-object-max-properties)))
         (arguments . []))
       (lambda (response)
         (kite-send
          "Runtime.getProperties"
          `((objectId . ,(plist-get (plist-get (plist-get response
                                                          :result)
                                               :result)
                                    :objectId))
            (ownProperties . t))
          (lambda (response)
            (kite--console-replace-object-async response
                                                object-plist
                                                buffer-point
                                                longp)))))))
  (kite--format-object object-plist))

(defun kite--console-format-message (message)
  "Format the console message described by MESSAGE at point.
MESSAGE is the raw message plist as received from the remote
debugger."
  (propertize
   (let* ((parameters (plist-get message :parameters))
          (arg-index (if (string= (plist-get (elt parameters 0) :type) "string")
                         1 0)))
     (concat
      (make-string (* 2 kite-message-group-level) 32)
      (when (> arg-index 0)
        (replace-regexp-in-string
         "\\([^%]\\|^\\)\\(%[osd]\\)"
         (lambda (string)
           (prog1
               (let ((object (elt parameters arg-index)))
                 (if object
                     (kite--console-format-object object)
                   string))
             (setq arg-index (1+ arg-index))))
         (propertize
          (plist-get message :text)
          'face (intern (format "kite-log-%s" (plist-get message :level))))
         t   ; fixed-case
         t   ; literal
         2)) ; subexp
      (let (extra-args)
        (while (< arg-index (length parameters))
          (setq extra-args
                (concat extra-args
                        (when (> arg-index 0) " ")
                        (kite--console-format-object (elt parameters arg-index) t)))
          (setq arg-index (1+ arg-index)))
        extra-args)
      (when (plist-get message :repeatCount)
        (kite--message-repeat-text
         (plist-get message :repeatCount)))
      "\n"))
   'log-message message))

(defun kite--console-messageAdded (websocket-url packet)
  "Callback invoked for `Console.messageAdded' notifications
received from the remote debugger."
  (let* ((buf (kite--find-buffer websocket-url 'console))
         (message (plist-get packet :message))
         (message-type (plist-get message :type)))
    (cond
     ((string= message-type "startGroup")
      (setq kite-message-group-level
            (+ kite-message-group-level 1)))
     ((string= message-type "endGroup")
      (setq kite-message-group-level
            (- kite-message-group-level 1))))
    (when (and kite-console-log-max
               buf
               (> (length (plist-get message :text)) 0))
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (keep-at-end (and (eq (point) (point-max))
                                (not (eq (point) (point-min))))))
          (save-excursion
            (when (numberp kite-console-log-max)
              (while (>= kite-console-line-count kite-console-log-max)
                (goto-char (point-min))
                (forward-line)
                (delete-region (point-min) (point))
                (setq kite-console-line-count (- kite-console-line-count 1))))
            (goto-char (point-max))
            (insert (kite--console-format-message message))
            (setq kite-console-line-count (1+ kite-console-line-count)))
          (when keep-at-end
            (goto-char (point-max))))
        (kite--log "message added, url is %s, packet is %s" websocket-url packet)))))

(defun kite-clear-console ()
  "Clear the console locally and in the remote debugger, the
latter by sending a `Console.clearMessages' message."
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t))
      (erase-buffer))
    (kite-send "Console.clearMessages" nil
               (lambda (response) (kite--log "Console cleared.")))))

(defun kite-console-visit-source ()
  "Visit the JavaScript source where the console message at point
originated, or raise an error if the source is unknown or
unavailable."
  (interactive)
  (unless (eq kite-buffer-type 'console)
    (error "Not in a kite console buffer"))
  (save-excursion
    (beginning-of-line)
    (let ((stack-trace
           (plist-get
            (get-text-property (point) 'log-message)
            :stackTrace)))
      (if (and stack-trace
               (>= (length stack-trace) 1))
          (kite-visit-stack-frame (elt stack-trace 0))
        (error "No source location available for this log message")))))

(defun kite-show-log-entry ()
  "Show details about the console message under cursor in a
temporary buffer.

FIXME: this should print the message using the same code as
output into the main console buffer.

FIXME: this could use nicer formatting."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((log-message (get-text-property (point) 'log-message)))
      (with-output-to-temp-buffer "*kite log message*"
        (princ (format (concat
                        "Origin: %s:%s\n"
                        "Source: %s\n"
                        "Level: %s\n"
                        "Repeat Count: %s\n"
                        "Message:\n\n%s\nStack Trace:\n\n%s")
                       (plist-get log-message :url)
                       (plist-get log-message :line)
                       (plist-get log-message :source)
                       (plist-get log-message :level)
                       (plist-get log-message :repeatCount)
                       (kite--console-format-message log-message)
                       (kite--format-stacktrace (plist-get log-message :stackTrace))
                       ))))))

(defun kite--log (format-string &rest args)
  (with-current-buffer
      (get-buffer-create (format "*kite log*"))
    (save-excursion
      (goto-char (point-max))
      (insert (concat (apply 'format format-string args) "\n")))))

(defun kite-insert-page-break ()
  (kite--log "kite-insert-page-break called")
  (insert "\f\n"))

(defcustom kite-console-on-reload-function
  (function kite-insert-page-break)
  "A function called with no arguments when the page is reloaded,
  with the message buffer as the current buffer, point placed at
  the end of the buffer, and read-only-ness inhibited.  The
  default value `kite-insert-page-break' does just that, insert a
  page break.  To mimic the behaviour of the WebKit debugger
  frontend, set this function to `erase-buffer'." )

(defun kite--console-globalObjectCleared (websocket-url packet)
  "Callback invoked for `Console.globalObjectCleared'
notifications received from the remote debugger.

FIXME: this function is broken and needs tests."
  (let ((buf (get-buffer (format "*kite console %s*" websocket-url))))
    (when buf
      (save-excursion
        (with-current-buffer buf
          (goto-char (point-max))
          (let ((inhibit-read-only t))
            (kite--log "kite--console-Debugger-globalObjectCleared called")
            (funcall kite-console-on-reload-function)))))))

(defun kite--console-messageRepeatCountUpdated (websocket-url packet)
  "Callback invoked for `Console.messageRepeatCountUpdated'
notifications received from the remote debugger."
  (let ((buf (kite--find-buffer websocket-url 'console)))
    (when buf
      (save-excursion
        (with-current-buffer buf
          (goto-char (point-max))
          (forward-line -1)
          (let ((inhibit-read-only t)
                (text-prop-start (text-property-any
                                  (point)
                                  (point-max)
                                  'kite-repeat-count
                                  t)))
            (if text-prop-start
                (let ((text-prop-end (next-single-property-change
                                      text-prop-start
                                      'kite-repeat-count)))
                  (delete-region text-prop-start text-prop-end)
                  (goto-char text-prop-start))
              (end-of-line))
            (insert (kite--message-repeat-text
                     (plist-get packet :count)))))))))

(defun kite--format-stacktrace (stacktrace)
  "Return the given STACKTRACE as a formatted string.  STACKTRACE
should be the raw stack trace plist as received from the remote
debugger.

FIXME: this is incomplete.

FIXME: this should be consolidated with
`kite--insert-stack-line'."
  (let ((formatted "") (index 0))
    (while (< index (length stacktrace))
      (let ((stackframe (elt stacktrace index)))
        (setq formatted
              (concat formatted
                      (format "%s:%s:%s(%s)"
                              (plist-get stackframe :url)
                              (plist-get stackframe :lineNumber)
                              (plist-get stackframe :columnNumber)
                              (plist-get stackframe :functionName))))
        (setq index (1+ index))))
    formatted))

(add-hook 'kite-Console-messageAdded-hooks 'kite--console-messageAdded)
(add-hook 'kite-Console-messageRepeatCountUpdated-hooks 'kite--console-messageRepeatCountUpdated)
(add-hook 'kite-Debugger-globalObjectCleared-hooks 'kite--console-globalObjectCleared)

(provide 'kite-console)

;;; kite-console.el ends here
