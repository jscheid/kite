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

(require 'cl)
(require 'kite-debug)
(require 'kite-dom)
(require 'kite-global)
(require 'kite-object)
(require 'kite-util)
(require 'font-lock)
(require 'comint)
(require 'widget)
(require 'js) ; for syntax highlighting

(eval-when-compile
  (require 'rx))

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

(defface kite-undefined
  '((t :inherit nxml-char-ref-number))
  "Face used to highlight undefined values."
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

(defface kite-console-prompt-face
  '((t :inherit default))
  "Face used to highlight the prompt."
  :version "24.1"
  :group 'kite-faces)

(defface kite-stack-error-type
  '((t :inherit error))
  "Face used to highlight error types in stack traces."
  :version "24.1"
  :group 'kite-faces)

(defface kite-stack-error-message
  '((t :inherit default))
  "Face used to highlight error messages in stack traces."
  :version "24.1"
  :group 'kite-faces)

(defface kite-stack-function-name
  '((t :inherit font-lock-function-name-face))
  "Face used to highlight function names in stack traces."
  :version "24.1"
  :group 'kite-faces)

(defface kite-stack-pseudo-file-name
  '((t :inherit default))
  "Face used to highlight file names in stack traces."
  :version "24.1"
  :group 'kite-faces)

(defface kite-stack-file-name
  '((t :inherit link))
  "Face used to highlight file names in stack traces."
  :version "24.1"
  :group 'kite-faces)

(defface kite-stack-line-number
  '((t :inherit kite-number))
  "Face used to highlight line numbers in stack traces."
  :version "24.1"
  :group 'kite-faces)

(defface kite-stack-column-number
  '((t :inherit kite-number))
  "Face used to highlight column numbers in stack traces."
  :version "24.1"
  :group 'kite-faces)

(defconst kite-level-prefixes
  '(("warning" . "WARNING: ")
    ("error" . "ERROR: "))
  "Prefix strings by message log level")

(defcustom kite-console-log-max 1000
  "Maximum number of lines to keep in the kite console log buffer.
If nil, disable console logging.  If t, log messages but don't
truncate the buffer when it becomes large."
  :group 'kite)

(defcustom kite-console-prompt "JS> "
  "Prompt used in kite-console."
  :group 'kite)

(defcustom kite-console-on-reload-function
  (function kite-insert-page-break)
  "A function called with no arguments when the page is reloaded,
  with the message buffer as the current buffer, point placed at
  the end of the buffer, and read-only-ness inhibited.  The
  default value `kite-insert-page-break' does just that, insert a
  page break.  To mimic the behaviour of the WebKit debugger
  frontend, set this function to `erase-buffer'."
  :group 'kite)

(defvar kite-console-prompt-internal
  (propertize "JS> " 'font-lock-face 'kite-console-prompt-face)
  "Stored value of `kite-console-prompt' in the current kite-console
buffer.

This is an internal variable used by kite-console.  Its purpose
is to prevent a running kite-console process from being messed up
when the user customizes `kite-console-prompt'.")

(defvar kite-console-mode-map
  (let ((map (copy-keymap widget-keymap))
	(menu-map (make-sparse-keymap)))
    ;;(suppress-keymap map t)
    (kite--define-global-mode-keys map)
    (define-key map "\t" 'kite-async-completion-at-point)
    (define-key map "\C-cX" 'kite-clear-console)
    (define-key map "\C-cg" 'kite-console-visit-source)
    (define-key map "\C-ci" 'kite-show-log-entry)
    (define-key map "\C-j" 'kite-console-send-input)
    (define-key map (kbd "RET") 'kite-console-send-input-or-visit-object)
    map)
  "Local keymap for `kite-console-mode' buffers.")

(defvar kite-console-input)
(defvar kite-message-group-level)
(defvar kite-console-line-count)

(defvar kite-console-header
  "// Welcome to Kite Console. `M-x describe-mode' for help.\n"
  "Message to display when kite-console is started.")

(define-derived-mode kite-console-mode comint-mode "kite-console"
  "Toggle kite console mode."
  :group 'kite
  (add-hook 'kill-buffer-hook 'kite--kill-console nil t)
  (set (make-local-variable 'kite-message-group-level) 0)
  (set (make-local-variable 'kite-console-line-count) 0)
  (setq case-fold-search nil)
  (set (make-local-variable 'widget-link-prefix) "")
  (set (make-local-variable 'widget-link-suffix) "")

  ;; Below code is adapted from ielm.el
  (setq comint-prompt-regexp
        (concat "^" (regexp-quote kite-console-prompt)))
  (setq comint-input-sender 'kite-console-input-sender)
  (setq comint-get-old-input 'kite-console-get-old-input)
  (set (make-local-variable 'comint-prompt-read-only) t)

  ;; A dummy process to keep comint happy. It will never get any input
  (unless (comint-check-proc (current-buffer))
    (condition-case nil
        (start-process "kite-console"
                       (current-buffer)
                       "hexl")
      (file-error (start-process "kite-console"
                                 (current-buffer)
                                 "cat")))
    (set-process-query-on-exit-flag (kite-console-process) nil)
    (goto-char (point-max))

    ;; JavaScript output can include raw characters that confuse
    ;; comint's carriage control code.
    (set (make-local-variable 'comint-inhibit-carriage-motion) t)

    (set (make-local-variable 'font-lock-defaults)
         (list js--font-lock-keywords))

    ;; Add a silly header
    (insert (propertize kite-console-header
                        'face 'font-lock-comment
                        'font-lock-face 'font-lock-comment))
    (kite-console-set-pm (point-max))
    (unless comint-use-prompt-regexp
      (let ((inhibit-read-only t))
        (add-text-properties
         (point-min) (point-max)
         (list 'rear-nonsticky t
               'field 'output
               'inhibit-line-move-field-capture t))))
    (comint-output-filter (kite-console-process)
                          kite-console-prompt-internal)
    (set-marker comint-last-input-start (kite-console-pm))
    (set-process-filter (get-buffer-process (current-buffer))
                        'comint-output-filter))

  (kite--console-update-mode-line)
  (kite-send "Console.enable")

  (run-mode-hooks 'kite-console-mode-hook))

(defun kite--kill-console ()
  "Called when a session's console buffer is closed.  Disables
console logging in the remote debugger by sending the
`Console.disable' message."
  (ignore-errors
    (kite-send "Console.disable")))

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
  (result object-plist buffer-point longp)
  "Replace a previously inserted simple object representation
with a more detailed representation after receiving additional
data from the server.  RESULT is the JSON-RPC result received
from the server.  OBJECT-PLIST is a plist describing the message
parameter for which the request was sent.  BUFFER-POINT is a
marker at which the temporary placeholder is located."
  (let* ((text-prop-start (text-property-any
                           buffer-point
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
                     (plist-get result :result))))
           ((string= (plist-get object-plist :subtype) "node")
            (widget-create
             'link
             :kite-object-id (plist-get object-plist :objectId)
             :button-face 'kite-object
             :notify (lambda (widget &rest ignore)
                       (kite-visit-dom-node
                        (widget-get widget :kite-object-id)))
                     (plist-get object-plist :description)))
           ((null (plist-get object-plist :subtype))
            (widget-create
             'link
             :kite-object-id (plist-get object-plist :objectId)
             :kite-object-description (plist-get
                                       object-plist
                                       :description)
             :button-face 'kite-object
             :notify (lambda (widget &rest ignore)
                       (kite-inspect-object
                        (widget-get widget :kite-object-id)
                        (widget-get widget :kite-object-description)))
             (if longp
                 (kite--format-object-with-props
                  (plist-get result :result))
               (plist-get object-plist :description))))
           (t
            (insert "UNKNOWN")))
          (put-text-property text-prop-start
                             (point)
                             'log-message
                             log-message))))))

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
             (plist-member object-plist :objectId)
             (string= (plist-get object-plist :type) "object"))
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
       :params
       (list :objectId (plist-get object-plist :objectId)
             :functionDeclaration
             (format "\
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
                     (1+ kite-short-object-max-properties))
             :arguments [])
       :success-function
       (lambda (result)
         (kite-send
          "Runtime.getProperties"
          :params
          (list :objectId (kite--get result
                                     :result
                                     :objectId)
                :ownProperties t)
          :success-function
          (lambda (result)
            (kite--console-replace-object-async result
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
          (arg-index (if (string= (kite--get parameters 0 :type)
                                  "string")
                         1 0)))
     (concat
      (make-string (* 2 kite-message-group-level) 32)
      (let ((kite-level-prefix
             (cdr (assoc (plist-get message :level)
                         kite-level-prefixes))))
        (when kite-level-prefix
          (propertize
           kite-level-prefix
           'font-lock-face (intern
                            (format "kite-log-%s"
                                    (plist-get message :level))))))

      (when (or (> arg-index 0) (null parameters))
        (replace-regexp-in-string
         "\\([^%]\\|^\\)\\(%[osd]\\)"
         (lambda (string)
           (prog1
               (let ((object (elt parameters arg-index)))
                 (if object
                     (kite--console-format-object object)
                   string))
             (setq arg-index (1+ arg-index))))
         (plist-get message :text)
         t   ; fixed-case
         t   ; literal
         2)) ; subexp
      (let (extra-args)
        (while (< arg-index (length parameters))
          (setq extra-args
                (concat extra-args
                        (when (> arg-index 0) " ")
                        (kite--console-format-object
                         (elt parameters arg-index)
                         t)))
          (setq arg-index (1+ arg-index)))
        extra-args)
      (when (plist-get message :repeatCount)
        (kite--message-repeat-text
         (plist-get message :repeatCount)))))
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
                (setq kite-console-line-count
                      (- kite-console-line-count 1))))

            (kite--console-goto-last-message)
            (insert (concat "\n"
                            (kite--console-format-message message))))

          (setq kite-console-line-count (1+ kite-console-line-count)))
        (kite--log "message added, url is %s, packet is %s"
                   websocket-url
                   packet)))))

(defun kite-clear-console ()
  "Clear the console locally and in the remote debugger, the
latter by sending a `Console.clearMessages' message."
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t))
      (erase-buffer))
    (comint-output-filter (kite-console-process)
                          kite-console-prompt-internal))
  (goto-char (kite-console-pm))
  (kite-send "Console.clearMessages"))

(defun kite-console-visit-source ()
  "Visit the JavaScript source where the console message at point
originated, or raise an error if the source is unknown or
unavailable."
  (interactive)
  (unless (eq kite-buffer-type 'console)
    (error "Not in a kite console buffer"))
  (save-excursion
    (beginning-of-line)
    (let ((stack-frame
           (or (elt (plist-get
                     (get-text-property (point) 'log-message)
                     :stackTrace)
                    0)
               (get-text-property (point) 'kite-source))))
      (if stack-frame
          (kite-visit-stack-frame stack-frame)
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
                       (kite--format-stacktrace
                        (plist-get log-message :stackTrace))
                       ))))))

(defun kite-insert-page-break ()
  "Insert a newline and page break character.  Default for
`kite-console-on-reload-function'."
  (insert "\n\f"))

(defun kite--console-goto-last-message ()
  "Put point after the last message printed"
  (goto-char (kite-console-pm))
  (let ((inhibit-field-text-motion t))
    (beginning-of-line)
    (backward-char)))

(defun kite--console-globalObjectCleared (websocket-url packet)
  "Callback invoked for `Console.globalObjectCleared'
notifications received from the remote debugger."
  (let ((buf (kite--find-buffer websocket-url 'console)))
    (when buf
      (save-excursion
        (with-current-buffer buf
          (kite--console-goto-last-message)
          (let ((inhibit-read-only t))
            (funcall kite-console-on-reload-function)))))))

(defun kite--console-messageRepeatCountUpdated (websocket-url packet)
  "Callback invoked for `Console.messageRepeatCountUpdated'
notifications received from the remote debugger."
  (let ((buf (kite--find-buffer websocket-url 'console)))
    (when buf
      (save-excursion
        (with-current-buffer buf
          (goto-char (kite-console-pm))
          (let ((inhibit-field-text-motion t))
            (beginning-of-line)
            (while (and (null (get-text-property (point) 'log-message))
                        (> (point) (point-min)))
              (forward-line -1)))
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
`kite--format-stack-line'."
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

(defun kite-console-input-sender (_proc input)
  ;; Just sets the variable kite-console-input, which is in the scope
  ;; of `kite-console-send-input's call.
  (setq kite-console-input input))

(defun kite-console-send-input ()
  "Evaluate the current console prompt input."
  (interactive)
  (let (kite-console-input)	       	; set by
                                        ; kite-console-input-sender
    (comint-send-input)			; update history, markers etc.
    (kite-console-eval-input kite-console-input)))

(defun kite--is-whitespace-or-comment (string)
  "Return non-nil if STRING is all whitespace or a comment."
  (or (string= string "")
      (string-match-p "\\`[ \t\n]*\\(?:\/\/.*\\)*\\'" string)))

(defun kite-console-eval-input (input)
  "Evaluate console input: send it to the remote debugger and
insert the result or error message, along with a new prompt, into
the buffer."
  (unless (kite--is-whitespace-or-comment input)
    (kite--eval-in-current-context
     input
     (lambda (result)
       (if (eq :json-false (plist-get result :wasThrown))
           (comint-output-filter
            (kite-console-process)
            (concat (kite--console-format-object
                     (plist-get result :result)
                     t)
                    "\n"
                    kite-console-prompt-internal))
         (kite--get-formatted-stack-trace
          (kite--get result :result :objectId)
          (lambda (stack-trace)
            (comint-output-filter
             (kite-console-process)
             (concat stack-trace
                     kite-console-prompt-internal)))))
       (let ((object-id
              (kite--get result :result :objectId)))
         (when object-id
           (kite--release-object object-id)))))))

(defun kite--contexts-by-unique-name (context-and-frame-list)
  "Given CONTEXT-AND-FRAME-LIST, an alist of (CONTEXT-ID
. CONTEXT-PLIST FRAME-PLIST), return an alist
of (UNIQUE-CONTEXT-NAME . CONTEXT-ID).

FIXME: This does not yet ensure that the returned name is
unique."
  (mapcar
   (lambda (context-and-frame)
     (let* ((context-id (car context-and-frame))
            (context (nth 0 (cdr context-and-frame)))
            (frame (nth 1 (cdr context-and-frame)))
            (context-name (plist-get context :name)))
       (cons
        (cond
         ;; Use given context name if available
         ((not (string= context-name ""))
          context-name)
         ;; Otherwise, fall back to frame URL
         (t
          (plist-get frame :url)))
        context-id)))
   context-and-frame-list))

(defun kite--session-contexts-by-unique-name ()
  (kite--contexts-by-unique-name
   (mapcar
    (lambda (context)
      (cons context
            (list context (kite--frame-by-id
                           (plist-get context :frameId)))))
    (kite-session-execution-context-list kite-session))))

(defvar kite-context-history)

(defun kite-set-context ()
  "Interactively select a JavaScript execution context for the
console."
  (interactive)
  (let* ((contexts-by-unique-name
          (kite--session-contexts-by-unique-name))
         (selection
          (if contexts-by-unique-name
              (completing-read "Context: "
                               (mapcar (function car)
                                       contexts-by-unique-name)
                               nil
                               t
                               nil
                               'kite-context-history)
            (message "Only the default execution context is available")
            nil))
         (new-context (cdr (assoc selection
                                  contexts-by-unique-name))))
    (when new-context
      (setf (kite-session-current-context kite-session)
            new-context)
      (kite--console-update-mode-line))))

;;; Process and marker utilities

(defun kite-console-process ()
  ;; Return the current buffer's process.
  (get-buffer-process (current-buffer)))

(defun kite-console-pm ()
  ;; Return the process mark of the current buffer.
  (process-mark (get-buffer-process (current-buffer))))

(defun kite-console-set-pm (pos)
  ;; Set the process mark in the current buffer to POS.
  (set-marker (process-mark (get-buffer-process (current-buffer))) pos))

(defun kite-console-get-old-input nil
  ;; Return the previous input surrounding point
  (save-excursion
    (beginning-of-line)
    (unless (looking-at-p comint-prompt-regexp)
      (re-search-backward comint-prompt-regexp))
    (comint-skip-prompt)
    (buffer-substring (point) (progn (forward-sexp 1) (point)))))

(defun kite--console-update-mode-line ()
  "Update the console buffer mode line display.  Should be
invoked after execution context changes."
  (let ((buf (kite--find-buffer
              (websocket-url
               (kite-session-websocket kite-session))
              'console)))
    (when buf
      (save-excursion
        (with-current-buffer buf
          (setq mode-line-process nil)
          (setq
           mode-line-buffer-identification
           (let ((current-context (kite-session-current-context
                                   kite-session))
                 (unique-names (kite--session-contexts-by-unique-name)))
             (nconc
              (propertized-buffer-identification "%b")
              (when current-context
                (list
                 (replace-regexp-in-string
                  "%" "%%"
                  (format
                   " (%s)"
                   (car (rassq current-context
                               unique-names))))))))))))))

(defun kite--console-execution-context-created (websocket-url packet)
  (kite--console-update-mode-line)
  (force-mode-line-update))

(defun kite--get-properties-fast (object-expr js-regex callback)
  "Efficiently and asynchronously fetch matching property names
for the object resulting from evaluating OBJECT-EXPR, a
JavaScript expression.  Only properties matching JS-REGEX, a
regular expression using JavaScript syntax, are fetched.  The
resulting property names are passed as an unsorted list of
strings to CALLBACK, which should accept a single parameter.

FIXME: no error handling."

  (lexical-let ((lex-js-regex js-regex)
                (lex-callback callback))
    (kite--eval-in-current-context
     object-expr
     (lambda (result)
       (lexical-let ((object-id (kite--get result
                                           :result
                                           :objectId)))
         (kite-send
          "Runtime.callFunctionOn"
          :params
          (list :objectId object-id
                :functionDeclaration "\
function f(regex_str) {
    result = [];
    regex = new RegExp(regex_str);
    for (key in this) {
      if (regex.test(key)) {
        result.push(key);
      }
    }
    return result.join(\";\");
}"
                :arguments `[ ( :value ,lex-js-regex ) ])
          :success-function
          (lambda (result)
            (kite--release-object object-id)
            (funcall lex-callback
                     (split-string (kite--get result
                                              :result
                                              :value)
                                   ";")))))))))

(defun kite-async-completion-at-point ()
  "Asynchronously fetch completions for the JavaScript expression
at point and, once results have arrived, perform completion using
`completion-in-region'.

Note: we can't use the usual mechanism of hooking into the
completions API (`completion-at-point-functions') because it
doesn't support asynchronicity."
  (interactive)
  (let (completion-begin)

    ;; Find the dotted JavaScript expression (consisting of
    ;; identifiers only) before point.  Note that we can't use just a
    ;; single regex because greedy regexes don't work when searching
    ;; backwards.
    (save-excursion
      (save-match-data
        (while (re-search-backward kite--identifier-part-regexp nil t))
        (setq completion-begin (point))))

    ;; FIXME: the previous step is too broad, it will find identifiers
    ;; starting with a digit.  Could do a second pass here to make
    ;; sure that we're looking at a valid expression, or improve error
    ;; handling in `kite--get-properties-fast' to ensure that we do
    ;; the right thing when the JavaScript side gets back to us with a
    ;; complaint.

    (when (< completion-begin (point))
      (let* ((components (split-string (buffer-substring-no-properties
                                        completion-begin
                                        (point))
                                       "\\."))
             (last-component (car (last components))))

        (lexical-let ((lex-completion-begin (- (point)
                                               (length last-component)))
                      (lex-completion-end (point)))
          (kite--get-properties-fast
           (if (> (length components) 1)
               (mapconcat 'identity
                          (subseq components
                                  0
                                  (- (length components) 1))
                          ".")
             "window")
           (concat "^" (regexp-quote last-component))
           (lambda (completions)
             (let* (completion-extra-properties
                    completion-in-region-mode-predicate)
               (completion-in-region
                lex-completion-begin
                lex-completion-end
                completions)))))))))

(defun kite-console-send-input-or-visit-object ()
  "If point is on a widget, activate the widget.  Otherwise,
evaluate console input."
  (interactive)
  (let ((widget (widget-at (point))))
    (if widget
        (widget-apply-action widget nil)
      (kite-console-send-input))))

(defun kite-visit-dom-node (object-id)
  "Open the DOM buffer and move point to the node corresponding
to the given OBJECT-ID."
  (kite-send "DOM.requestNode"
             :params
             (list :objectId object-id)
             :success-function
             (lambda (result)
               (lexical-let ((lex-node-id (plist-get result :nodeId)))
                 (kite--get-buffer-create
                  (websocket-url (kite-session-websocket kite-session))
                  'dom
                  (lambda ()
                    (kite-dom-goto-node
                     lex-node-id)))))))

(add-hook 'kite-Console-messageAdded-hooks
          'kite--console-messageAdded)
(add-hook 'kite-Console-messageRepeatCountUpdated-hooks
          'kite--console-messageRepeatCountUpdated)
(add-hook 'kite-Debugger-globalObjectCleared-hooks
          'kite--console-globalObjectCleared)
(add-hook 'kite-Runtime-executionContextCreated-hooks
          'kite--console-execution-context-created)

(provide 'kite-console)

;;; kite-console.el ends here
