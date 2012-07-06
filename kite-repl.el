
(defun kite-repl ()
  (interactive)
  (--kite-log "opening repl")
  (lexical-let*
      ((kite-connection (current-buffer))
       (buf (get-buffer-create (format "*kite repl %s*" (cdr (assq 'webSocketDebuggerUrl kite-tab-alist))))))
    (with-current-buffer buf
      (kite-repl-mode)
      (set (make-local-variable 'kite-connection) kite-connection))
    (switch-to-buffer buf)))

(defvar kite-repl-mode-map
  (let ((map (make-keymap))
	(menu-map (make-sparse-keymap)))
    (define-key map (kbd "C-M-x") 'kite-eval-defun)
    (define-key map (kbd "C-c C-c") 'kite-repl-eval)
    map)
  "Local keymap for `kite-repl-mode' buffers.")

(define-derived-mode kite-repl-mode javascript-mode "kite-repl"
  "Toggle kite repl mode."
  t)


(defun kite-eval-defun ()
  (save-excursion
    (let (begin end pstate defun-info temp-name defun-body)
      (js-end-of-defun)
      (setq end (point))
      (js--ensure-cache)
      (js-beginning-of-defun)
      (re-search-forward "\\_<function\\_>")
      (setq begin (match-beginning 0))
      (setq pstate (js--forward-pstate))

      (when (or (null pstate)
                (> (point) end))
        (error "Could not locate function definition"))

      (setq defun-info (js--guess-eval-defun-info pstate))
      (setq defun-body (buffer-substring-no-properties begin end))

      (message "defun-info=%s   defun-body: %s"))))


(defun kite-repl-eval ()
  (interactive)
  (save-excursion

    (lexical-let* ((buf (current-buffer))
                   (begin
                    (progn
                      (if (re-search-backward "^///" nil t)
                          (progn
                            (next-line)
                            (beginning-of-line))
                        (goto-char (point-min)))
                      (point)))

                   (end
                    (progn
                      (if (re-search-forward "^///" nil t)
                          (beginning-of-line)
                        (goto-char (point-max)))
                      (point)))

                   (code (buffer-substring-no-properties begin end)))

      (with-current-buffer kite-connection
        (--kite-send "Runtime.evaluate" (list (cons 'expression code))
                     (lambda (response)
                       (let ((result (cdr (assq 'result response))))
                         (message "result %s" result)
                         (if (eq :json-false (cdr (assq 'wasThrown result)))
                             (with-current-buffer buf
                               (save-excursion
                                 (goto-char end)
                                 (insert (format "/// -> %S\n" (or (cdr (assq 'value (cdr (assq 'result result))))
                                                                   (intern (cdr (assq 'type (cdr (assq 'result result))))))))))

                           (--kite-send "Runtime.getProperties" (list (cons 'objectId (cdr (assq 'objectId (cdr (assq 'result result))))))
                                        (lambda (response)
                                          (mapcar (lambda (x)
                                                    (when (string= "stack" (cdr (assq 'name x)))
                                                      (--kite-send "Runtime.getProperties" (list (cons 'objectId (cdr (assq 'objectId (cdr (assq 'get x))))))
                                                                   (lambda (response)
                                                                     (message "got stack %s" response)))))
                                                  (cdr (assq 'result (cdr (assq 'result response)))))
                                          ))))))))))

                                            ;(insert (format "///! %s\n" (cdr (assq 'description (cdr (assq 'result result))))))))))))))))

(provide 'kite-repl)
