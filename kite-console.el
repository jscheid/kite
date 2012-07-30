(require 'font-lock)

(defface kite-log-warning
  '((t :inherit warning))
  "Basic face used to highlight warnings."
  :version "24.1"
  :group 'kite-faces)

(defface kite-log-error
  '((t :inherit error))
  "Basic face used to highlight warnings."
  :version "24.1"
  :group 'kite-faces)

(defface kite-log-debug
  '((t :inherit font-lock-comment))
  "Basic face used to highlight warnings."
  :version "24.1"
  :group 'kite-faces)

(defface kite-log-log
  '((t :inherit default))
  "Basic face used to highlight warnings."
  :version "24.1"
  :group 'kite-faces)

(defface kite-log-tip
  '((t :inherit underline))
  "Basic face used to highlight warnings."
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

(defcustom kite-console-log-max 1000
  "Maximum number of lines to keep in the kite console log buffer.
If nil, disable console logging.  If t, log messages but don't truncate
the buffer when it becomes large.")

(defvar kite-console-mode-map
  (let ((map (make-keymap))
	(menu-map (make-sparse-keymap)))
    (suppress-keymap map t)
    (kite--define-global-mode-keys map)
    (define-key map "X" 'kite-clear-console)
    (define-key map (kbd "RET") 'kite-show-log-entry)
    map)
  "Local keymap for `kite-console-mode' buffers.")

(define-derived-mode kite-console-mode special-mode "kite-console"
  "Toggle kite console mode."
  (set (make-local-variable 'kill-buffer-hook) 'kite--kill-console)
  (set (make-local-variable 'kite-message-group-level) 0)
  (set (make-local-variable 'kite-console-line-count) 0)
  (hl-line-mode)
  (setq case-fold-search nil))

(defun kite--kill-console ()
  (ignore-errors
    (kite-send "Console.disable" nil
               (lambda (response) (kite--log "Console disabled.")))))

(defun kite--console-buffer (websocket-url)
  (get-buffer (format "*kite console %s*" websocket-url)))

(defun kite--message-repeat-text (repeat-count)
  (and repeat-count
       (> repeat-count 1)
       (propertize
        (format " [message repeated %d times]" repeat-count)
        'kite-repeat-count t)))

(defun kite--insert-object-async (response object buffer-point)
  (let* ((text-prop-start (text-property-any
                           buffer-point
                           (point-max)
                           'kite-loading-object-id
                           (plist-get object :objectId)))
         (text-prop-end (next-single-property-change
                         text-prop-start
                         'kite-loading-object-id)))

    (kite--log "Got runtime props: %s start %s end %s" response text-prop-start text-prop-end)
    (when (and text-prop-start text-prop-end)
      (let ((inhibit-read-only t))
        (save-excursion
          (delete-region text-prop-start text-prop-end)
          (goto-char text-prop-start)
          (cond
           ((string= (plist-get object :subtype) "array")
            (insert "[")
            (let ((array-index 0)
                  (is-first t)
                  (array-elements (plist-get (plist-get response :result) :result)))
              (while (< array-index (length array-elements))
                (let ((array-element (elt array-elements array-index)))
                  (when (eq t (plist-get array-element :enumerable))
                    (if is-first
                        (setq is-first nil)
                      (insert ", "))
                    (insert (kite--format-object (plist-get array-element :value)))))
                (setq array-index (1+ array-index))))
            (insert "]"))
           (t
            (insert "LOADED"))))))))

(defun kite--format-object (object)
  (let ((type (plist-get object :type)))
    (cond
     ((string= type "number")
      (propertize
       (plist-get object :description)
       'face 'kite-number))
     ((string= type "string")
      (concat
       (propertize
        "\""
        'face 'kite-quote)
       (propertize
        (plist-get object :value)
        'face 'kite-string)
       (propertize
        "\""
        'face 'kite-quote)))
     ((and (not (plist-member object :result))
           (plist-member object :objectId))
      (lexical-let ((object object)
                    (buffer-point (point-marker)))
        (kite-send "Runtime.getProperties"
                   (list (cons 'objectId (plist-get object :objectId))
                         (cons 'ownProperties t))
                   (lambda (response)
                     (kite--insert-object-async response object buffer-point))))
      (propertize
       (format "(Loading...)" (plist-get object :value))
       'kite-loading-object-id (plist-get object :objectId)
       'face 'kite-loading))
     (t
      (propertize "(unknown)" 'face 'error)))))

(defun kite--console-insert-message (message)
  (insert
   (propertize
    (concat
     (make-string (* 2 kite-message-group-level) 32)
     (let ((arg-index 1)
           (parameters (plist-get message :parameters)))
       (replace-regexp-in-string
        "\\([^%]\\|^\\)\\(%[osd]\\)"
        (lambda (string)
          (prog1
              (let ((object (elt parameters arg-index)))
                (if object
                    (kite--format-object object)
                  string))
            (setq arg-index (1+ arg-index))))
        (propertize
         (plist-get message :text)
         'face (intern (format "kite-log-%s" (plist-get message :level))))
        t   ; fixed-case
        t   ; literal
        2)) ; subexp
     (when (plist-get message :repeatCount)
       (kite--message-repeat-text
        (plist-get message :repeatCount)))
     "\n")
    'log-message message)))

(defun kite--console-messageAdded (websocket-url packet)
  (let* ((buf (kite--console-buffer websocket-url))
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
            (kite--console-insert-message message)
            (setq kite-console-line-count (1+ kite-console-line-count)))
          (when keep-at-end
            (goto-char (point-max))))
        (kite--log "message added, url is %s, packet is %s" websocket-url packet)))))

(defun kite-clear-console ()
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t))
      (erase-buffer))
    (kite-send "Console.clearMessages" nil
               (lambda (response) (kite--log "Console cleared.")))))

(defun kite-show-log-entry ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((log-message (get-text-property (point) 'log-message)))
      (with-output-to-temp-buffer "*kite log message*"
        (princ (format (concat
                        "Origin: %s:%s\n"
                        "Source: %s\n"
                        "Type: %s\n"
                        "Level: %s\n"
                        "Repeat Count: %s\n"
                        "Message:\n\n%s\n\nStack Trace:\n\n%s")
                       (plist-get log-message :url)
                       (plist-get log-message :line)
                       (plist-get log-message :source)
                       (plist-get log-message :type)
                       (plist-get log-message :level)
                       (plist-get log-message :repeatCount)
                       (plist-get log-message :text)
                       (kite--format-stacktrace (plist-get log-message :stackTrace))
                       ))))))

(defun kite--log (format-string &rest args)
  (with-current-buffer
      (get-buffer-create (format "*kite log*"))
    (save-excursion
      (goto-char (point-max))
      (insert (concat (apply 'format format-string args) "\n")))))

(defun kite-console ()
  (interactive)
  (kite--log "opening console")
  (lexical-let*
      ((kite-session kite-session))
    (switch-to-buffer
     (get-buffer-create (format "*kite console %s*"
                                (websocket-url (kite-session-websocket kite-session)))))
    (kite-console-mode)
    (set (make-local-variable 'kite-session) kite-session)
    (erase-buffer)
    (kite-send "Console.enable" nil
               (lambda (response) (kite--log "Console enabled.")))))

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
  (let ((buf (get-buffer (format "*kite console %s*" websocket-url))))
    (when buf
      (save-excursion
        (with-current-buffer buf
          (goto-char (point-max))
          (let ((inhibit-read-only t))
            (kite--log "kite--console-Debugger-globalObjectCleared called")
            (funcall kite-console-on-reload-function)))))))

(defun kite--console-messageRepeatCountUpdated (websocket-url packet)
  (let ((buf (kite--console-buffer websocket-url)))
    (when buf
      (save-excursion
        (with-current-buffer buf
          (goto-char (point-max))
          (previous-line)
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

(add-hook 'kite-Console-messageAdded-hooks 'kite--console-messageAdded)
(add-hook 'kite-Console-messageRepeatCountUpdated-hooks 'kite--console-messageRepeatCountUpdated)
(add-hook 'kite-Debugger-globalObjectCleared-hooks 'kite--console-globalObjectCleared)

(provide 'kite-console)
