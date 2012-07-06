
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
  (set (make-local-variable 'kill-buffer-hook) '--kite-kill-console)
  (hl-line-mode)
  (setq case-fold-search nil))

(defun --kite-kill-console ()
  (ignore-errors
    (with-current-buffer kite-connection
      (--kite-send "Console.disable" nil
                   (lambda (response) (--kite-log "Console disabled."))))))

(defun --kite-Console-messageAdded (websocket-url packet)
  (let ((buf (get-buffer (format "*kite console %s*" websocket-url))))
    (when buf
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (propertize (concat (cdr (assq 'text packet)) "\n")
                              'log-message packet
                              'face (intern (format "kite-log-%s" (cdr (assq 'level packet))))))
          (goto-char (point-max))
          (--kite-log "message added, url is %s, packet is %s" websocket-url packet))))))

(defun kite-clear-console ()
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t))
      (erase-buffer))
    (with-current-buffer kite-connection
      (--kite-send "Console.clearMessages" nil
                   (lambda (response) (--kite-log "Console cleared."))))))

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
                       (cdr (assq 'url log-message))
                       (cdr (assq 'line log-message))
                       (cdr (assq 'source log-message))
                       (cdr (assq 'type log-message))
                       (cdr (assq 'level log-message))
                       (cdr (assq 'repeatCount log-message))
                       (cdr (assq 'text log-message))
                       (--kite-format-stacktrace (cdr (assq 'stackTrace log-message)))
                       ))))))

(defun --kite-log (format-string &rest args)
  (with-current-buffer
      (get-buffer-create (format "*kite log*"))
    (insert (concat (apply 'format format-string args) "\n"))))

(defun kite-console ()
  (interactive)
  (--kite-log "opening console")
  (lexical-let*
      ((kite-connection (current-buffer))
       (buf (get-buffer-create (format "*kite console %s*" (cdr (assq 'webSocketDebuggerUrl kite-tab-alist))))))
    (with-current-buffer buf
      (kite-console-mode)
      (set (make-local-variable 'kite-connection) kite-connection))
    (switch-to-buffer buf)
    (erase-buffer)
    (save-excursion
      (with-current-buffer kite-connection
        (--kite-log "sending in buffer %s" (current-buffer))
        (--kite-send "Console.enable" nil
                     (lambda (response) (--kite-log "Console enabled.")))))))

(provide 'kite-console)
