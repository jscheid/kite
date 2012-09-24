;;; kite-net.el --- Temporary Kite Common Lisp adaptor

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

;; This package provides aliases to smooth out the differences between
;; cl and cl-lib.  It is intended to be temporary, to be removed once
;; cl-lib is available widely.
;;
;; It is part of Kite, a WebKit inspector front-end.


;;; Code:

(if (require 'cl-lib nil t)
    (progn
      (defalias 'kite--defstruct 'cl-defstruct)
      (defalias 'kite--defun 'cl-defun)
      (defalias 'kite--every 'cl-every)
      (defalias 'kite--find-if 'cl-find-if)
      (defalias 'kite--flet 'cl-flet)
      (defalias 'kite--incf 'cl-incf)
      (defalias 'kite--mapcar 'cl-mapcar)
      (defalias 'kite--position 'cl-position)
      (defalias 'kite--remove-if 'cl-remove-if)
      (defalias 'kite--subseq 'cl-subseq))
  (require 'cl)
  (defalias 'kite--defstruct 'defstruct)
  (defalias 'kite--defun 'defun*)
  (defalias 'kite--every 'every)
  (defalias 'kite--find-if 'find-if)
  (defalias 'kite--flet 'flet)
  (defalias 'kite--incf 'incf)
  (defalias 'kite--mapcar 'mapcar*)
  (defalias 'kite--position 'position)
  (defalias 'kite--remove-if 'remove-if)
  (defalias 'kite--subseq 'subseq))

(provide 'kite-cl)

;;; kite-cl.el ends here
