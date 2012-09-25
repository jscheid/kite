;;; kite-load-path.el --- WebKit inspector front-end

;; Copyright (C) 2012 Julian Scheid

;; Author: Julian Scheid <julians37@gmail.com>
;; Author: T. V. Raman <tv.raman.tv@gmail.com> (cloned from emacspeak-load-path.el)
;; Keywords: tools
;; Version: 0.1
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
;;; Set up load-path so we can compile cleanly.
;;; Code:



(when (or (< emacs-major-version 24)
          (and (eq emacs-major-version 24)
               (< emacs-minor-version 1)))
  (error "Kite requires Emacs 24.1 or newer.  This is %s" emacs-version))

(require 'package)

(defvar kite--directory
  (expand-file-name (file-name-directory load-file-name))
  "Directory where Kite is installed. ")
(let ((package-load-list '((websocket t))))
  (package-initialize))

(unless (member kite--directory load-path)
  (setq load-path (cons kite--directory load-path)))

(setq byte-compile-warnings '(not cl-functions))

(provide 'kite-load-path)
