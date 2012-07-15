
(defface kite-log-warning
  '((t :inherit warning))
  "Basic face used to highlight warnings."
  :version "24.1"
  :group 'ekwd-faces)

(defface kite-log-error
  '((t :inherit error))
  "Basic face used to highlight warnings."
  :version "24.1"
  :group 'ekwd-faces)

(defface kite-log-debug
  '((t :inherit font-lock-comment))
  "Basic face used to highlight warnings."
  :version "24.1"
  :group 'ekwd-faces)

(defface kite-log-log
  '((t :inherit default))
  "Basic face used to highlight warnings."
  :version "24.1"
  :group 'ekwd-faces)

(defface kite-log-tip
  '((t :inherit underline))
  "Basic face used to highlight warnings."
  :version "24.1"
  :group 'ekwd-faces)

(defvar kite-console-mode-map
  (let ((map (make-keymap))
	(menu-map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "X" 'kite-clear-console)
    (define-key map (kbd "RET") 'kite-show-log-entry)
    map)
  "Local keymap for `kite-console-mode' buffers.")

(define-derived-mode kite-console-mode special-mode "kite-console"
  "Toggle kite console mode."
  (set (make-local-variable 'kill-buffer-hook) 'kite--kill-console)
  (set (make-local-variable 'kite-message-group-level) 0)
  (hl-line-mode)
  (setq case-fold-search nil))

(defun kite--kill-console ()
  (ignore-errors
    (kite-send "Console.disable" nil
               (lambda (response) (kite--log "Console disabled.")))))

(defun kite--console-Console-messageAdded (websocket-url packet)
  (let* ((buf (get-buffer (format "*kite console %s*" websocket-url)))
         (message (plist-get packet :message))
         (message-type (plist-get message :type)))
    (cond
     ((string= message-type "startGroup")
      (setq kite-message-group-level
            (+ kite-message-group-level 1)))
     ((string= message-type "endGroup")
      (setq kite-message-group-level
            (- kite-message-group-level 1))))
    (when (and buf
               (> (length (plist-get message :text)) 0))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-max))
            (insert (propertize (concat
                                 (make-string (* 2 kite-message-group-level) 32)
                                 (plist-get message :text)
                                 "\n")
                                'log-message message
                                'face (intern (format "kite-log-%s" (plist-get message :level))))))
          (kite--log "message added, url is %s, packet is %s" websocket-url packet))))))

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

(add-hook 'kite-Console-messageAdded-hooks 'kite--console-Console-messageAdded)

(provide 'kite-console)
