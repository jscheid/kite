;;; kite-scratch.el --- Kite scratch buffer implementation

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

;; This package implements a buffer suitable for evaluating JavaScript
;; code, akin to the Emacs *scratch* buffer for evaluating Lisp code.
;;
;; It is part of Kite, a WebKit inspector front-end.


;;; Code:

(require 'js)

(require 'kite-global)
(require 'kite-object)
(require 'kite-util)

(defface kite-link-face
  '((t (:inherit change-log-file)))
  "Face used for links to source code locations."
  :group 'kite-highlighting-faces)

(defvar kite-scratch-mode-map
  (let ((map (make-keymap))
	(menu-map (make-sparse-keymap)))
    (define-key map (kbd "C-M-x") 'kite-eval-defun)
    (define-key map (kbd "C-c C-c") 'kite-scratch-eval)
    map)
  "Local keymap for `kite-scratch-mode' buffers.")

(defvar kite-scratch-mode-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'kite-goto-link)
    (define-key map (kbd "RET") 'kite-goto-link)
    map))

(defun kite-goto-link ()
  (interactive)
  (message "kite-goto-link"))

(define-derived-mode kite-scratch-mode javascript-mode "kite-scratch"
  "Toggle kite scratch mode."
  :group 'kite
  (set (make-local-variable 'font-lock-extra-managed-props) '(keymap))

  (set (make-local-variable 'font-lock-fontify-region-function)
       (lambda (beginning end &optional verbose)
         "Override to ensure our preset font face isn't changed
by font locking"
         (prog1
             (font-lock-default-fontify-region beginning end verbose)
           (loop for i from beginning to end do
                 (let ((face (get-text-property i 'font-lock-face)))
                   (when face
                     (put-text-property i (1+ i) 'face face)))))))

  (run-mode-hooks 'kite-scratch-mode-hook))

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
      (setq defun-body (buffer-substring-no-properties begin end)))))

(defun kite-scratch-eval ()
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

      (kite-send "Runtime.evaluate"
                 :params
                 (list :expression code)
                 :success-function
                 (lambda (result)
                   (if (eq :json-false (plist-get result :wasThrown))
                       (save-excursion
                         (goto-char end)
                         (insert
                          (concat "\n/// -> "
                                  (kite--format-object (plist-get result :result))
                                  "\n")))
                     (kite--get-formatted-stack-trace
                      (kite--get result :result :objectId)
                      (lambda (stack-trace)
                       (save-excursion
                         (goto-char end)
                         (insert (concat
                                  "\n"
                                  (mapconcat
                                   (lambda (line)
                                     (concat "/// " line))
                                   (split-string stack-trace "\n")
                                   "\n")
                                  "\n")))))))))))

(font-lock-add-keywords 'kite-scratch-mode '(("(\\([a-zA-Z]+:.*?:[0-9]+:[0-9]+\\))$" 1 `(face kite-link-face keymap ,kite-scratch-mode-link-map) t)))


(provide 'kite-scratch)

;;; kite-scratch.el ends here
