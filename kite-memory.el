;;; kite-memory.el --- Kite memory inspector implementation

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

;; This package implements the WebKit memory usage inspector.
;;
;; It is part of Kite, a WebKit inspector front-end.


;;; Code:

(require 'kite-global)

(defvar kite-memory-mode-map
  (let ((map (make-keymap))
	(ctl-c-b-map (make-keymap))
	(menu-map (make-sparse-keymap)))
    (suppress-keymap map t)
    (kite--define-global-mode-keys map)
    (define-key map "g" 'kite--memory-refresh)
    map))

(define-derived-mode kite-memory-mode special-mode "kite-memory"
  "Toggle kite memory mode."
  :group 'kite
  (set (make-local-variable 'kill-buffer-hook) 'kite--kill-buffer)
  (setq case-fold-search nil)
  (run-mode-hooks 'kite-memory-mode-hook))

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
  (kite-send "Memory.getProcessMemoryDistribution"
             :success-function
             (lambda (result)
               (let ((inhibit-read-only t))
                 (erase-buffer)
                 (kite--memory-render-graph
                  (plist-get result :distribution) 0)))))

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

;;; kite-memory.el ends here
