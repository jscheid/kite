;;; kite-repl.el --- Kite REPL implementation

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

;; This package implements a simple JavaScript REPL.  It could
;; probably use a better name and a better description since the
;; functionality it implements is more akin to the Emacs *scratch*
;; buffer than to a true read-eval-print loop.
;;
;; It is part of Kite, a WebKit inspector front-end.


;;; Code:

(defface kite-link-face
  '((t (:inherit change-log-file)))
  "Face used for links to source code locations."
  :group 'kite-highlighting-faces)

(defun kite-repl ()
  (interactive)
  (kite--log "opening repl")
  (kite--get-buffer-create 'repl 'kite-repl-mode))

(defvar kite-repl-mode-map
  (let ((map (make-keymap))
	(menu-map (make-sparse-keymap)))
    (define-key map (kbd "C-M-x") 'kite-eval-defun)
    (define-key map (kbd "C-c C-c") 'kite-repl-eval)
    map)
  "Local keymap for `kite-repl-mode' buffers.")

(defvar kite-repl-mode-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'kite-goto-link)
    (define-key map (kbd "RET") 'kite-goto-link)
    map))

(defun kite-goto-link ()
  (interactive)
  (message "kite-goto-link"))

(define-derived-mode kite-repl-mode javascript-mode "kite-repl"
  "Toggle kite repl mode."
  :group 'kite
  (set (make-local-variable 'font-lock-extra-managed-props) '(keymap))
  (run-mode-hooks 'kite-repl-mode-hook))

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

(defun kite--insert-stack-line (line)
  (insert (format "/// %s" line))
  (when (string-match "(\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\))$" line)
    (let* ((error-offset (match-beginning 0))
           (error-url-string (match-string 1 line))
           (error-line (match-string 2 line))
           (error-column (match-string 3 line))
           (error-url (url-generic-parse-url error-url-string)))
      (when (url-type error-url)
        (add-text-properties
         (save-excursion
           (beginning-of-line)
           (forward-char error-offset)
           (point))
         (save-excursion
           (end-of-line)
           (point))
         '(face error))
        )))
  (insert "\n"))

(defun kite-repl-eval ()
  (interactive)
  (save-excursion

    (lexical-let* ((begin
                    (progn
                      (if (re-search-backward "^///" nil t)
                          (progn
                            (forward-line)
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

      (kite-send "Runtime.evaluate" (list (cons 'expression code))
                 (lambda (response)
                   (let ((result (plist-get response :result)))
                     (message "result %s" result)
                     (if (eq :json-false (plist-get result :wasThrown))
                         (save-excursion
                           (goto-char end)
                           (insert (format "\n/// -> %S\n" (or (plist-get (plist-get result :result) :value)
                                                               (intern (plist-get (plist-get result :result) :type))))))
                       (kite--log "got thrown exception response: %s" (pp-to-string response))
                       (lexical-let ((error-object-id (plist-get (plist-get result :result) :objectId)))

                         (kite-send "Runtime.callFunctionOn"
                                    (list
                                     (cons 'objectId error-object-id)
                                     (cons 'functionDeclaration "function foo() { return this.stack; }") ; (plist-get (plist-get x :get) :objectId))
                                     (cons 'arguments '[]))
                                    (lambda (response)
                                      (kite--log "got stack %s"
                                                 (save-excursion
                                                   (goto-char end)
                                                   (when (> (current-column) 0)
                                                     (insert "\n"))
                                                   (mapcar
                                                    (function kite--insert-stack-line)
                                                    (split-string (plist-get (plist-get (plist-get response :result) :result) :value) "\n"))))))))
                     (plist-get (plist-get response :result) :result)))))))

(font-lock-add-keywords 'kite-repl-mode '(("(\\([a-zA-Z]+:.*?:[0-9]+:[0-9]+\\))$" 1 `(face kite-link-face keymap ,kite-repl-mode-link-map) t)))


(provide 'kite-repl)

;;; kite-repl.el ends here
