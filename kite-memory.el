
(defvar kite-memory-mode-map
  (let ((map (make-keymap))
	(ctl-c-b-map (make-keymap))
	(menu-map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "g" 'kite--memory-refresh)
    map))

(define-derived-mode kite-memory-mode special-mode "kite-memory"
  "Toggle kite memory mode."
  (set (make-local-variable 'kill-buffer-hook) 'kite--kill-buffer)
  (setq case-fold-search nil))

(defun kite--memory-render-graph (node indent)
  (insert (concat (make-string (* 2 indent) 32)
                  (plist-get node :name)
                  ": "
                  (number-to-string (plist-get node :size))
                  "\n"))
  (let ((count (length (plist-get node :children)))
        (index 0))
    (while (< index count)
      (kite--memory-render-graph (elt (plist-get node :children) index) (1+ indent))
      (setq index (1+ index)))))

(defun kite--memory-refresh ()
  (interactive)
  (kite-send "Memory.getProcessMemoryDistribution" nil
             (lambda (response)
               (let ((inhibit-read-only t))
                 (erase-buffer)
                 (kite--memory-render-graph
                  (plist-get (plist-get response :result) :distribution) 0))))
  (kite-send "Memory.getDOMNodeCount" nil
             (lambda (response)
               (kite--log "Memory.getDOMNodeCount got response %s" response))))



(defun kite-memory ()
  (interactive)

  (lexical-let*
      ((kite-session kite-session)
       (buf (get-buffer-create (format "*kite memory %s*" (websocket-url (kite-session-websocket kite-session))))))
    (with-current-buffer buf
      (kite-memory-mode)
      (set (make-local-variable 'kite-session) kite-session))
    (switch-to-buffer buf)
    (kite--memory-refresh)))

(provide 'kite-memory)
