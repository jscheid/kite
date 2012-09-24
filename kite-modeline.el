;;; kite-modeline.el --- Kite mode line display

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

;; This package provides `kite-mode-line-mode' which displays
;; information about global Kite state in the mode line.
;;
;; It is part of Kite, a WebKit inspector front-end.


;;; Code:

(require 'kite-global)

(defvar kite-mode-line-string nil)
(defvar kite-mode-line-info nil)

(define-minor-mode kite-mode-line-mode
  "Toggle display of kite information in the mode line.
With a prefix argument ARG, enable Kite Modeline mode if ARG is
positive, and disable it otherwise.

If called from Lisp, enable the mode if ARG is omitted or nil."
  :global t :group 'kite
  (setq kite-mode-line-string "")
  (or global-mode-string (setq global-mode-string '("")))
  (when kite-mode-line-mode
    (add-to-list 'global-mode-string 'kite-mode-line-string t)
    (setq kite-mode-line-string
          (list '(:eval (lambda () kite-mode-line-info))))
    (put 'kite-mode-line-string 'risky-local-variable t)
    (put 'kite-mode-line-info 'risky-local-variable t)
    (kite--mode-line-update)))

(defun kite--mode-line-session-info (kite-session)
  "Make a short string describing the status of the session.
Currently shows whether or not the session is running or paused,
and how many errors occurred in the session."
  (concat
   (propertize "r" 'face 'success)
   (when (> (kite-session-error-count kite-session) 0)
     (concat
      ":"
      (propertize (number-to-string
                   (kite-session-error-count kite-session))
                  'face 'error)
      ))))

(defun kite--mode-line-update ()
  "Update information in mode line.  Should be called after list
of active session changes and after any session state displayed
in the mode line changes."
  (setq kite-mode-line-info
        (concat
         " Kite["
         (if kite-active-session-list
             (mapconcat (function kite--mode-line-session-info)
                        kite-active-session-list
                        "/")
           "offline")
         "]")))

(provide 'kite-modeline)

;;; kite-modeline.el ends here
