;;; kite-util.el --- Kite utility functions

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

;; This package provides various utility functions.
;;
;; It is part of Kite, a WebKit inspector front-end.


;;; Code:

(defun kite--dimmed-face-foreground (face darkness)
  "Return a color value string suitable for passing as the value
  for the :foreground face property that represents a
  'dimmed' (darker) variant of the foreground color of the given
  FACE.  The DARKNESS parameter should be a numeric value in the
  range 0..1; 0 means not to darken the foregroud color, 1 means
  to darken it fully (so that it is the same as the background
  color).  A darkness value of 0.5 would cause a foreground color
  to be returned that is halfway between the foreground and
  background color of FACE.  (If FACE doesn't have a background
  color set, the current frame's background color will be used
  instead.)"
  (flet ((lerp (a b w)
               (+ (* a w)
                  (* b (- 1 w)))))
    (let ((fg (color-name-to-rgb (face-foreground face nil t)))
          (bg (color-name-to-rgb (or (face-background face nil t)
                                     (cdr (assq 'background-color (frame-parameters)))))))
      (or (and
           fg
           bg
           (color-rgb-to-hex
            (lerp (nth 0 bg) (nth 0 fg) darkness)
            (lerp (nth 1 bg) (nth 1 fg) darkness)
            (lerp (nth 2 bg) (nth 2 fg) darkness)))
          "#888888"))))

(defun kite--longest-prefix (strings)
  "Return the longest prefix common to all the given STRINGS,
which should be a sequence of strings.  Naive implementation."
  (if (null strings)
      ""
    (let ((max-length (length (car strings))))
      (while (let ((prefix-candidate (substring (car strings) 0 max-length)))
               (not (every (apply-partially 'string-prefix-p prefix-candidate) strings)))
        (setq max-length (- max-length 1)))
      (substring (car strings) 0 max-length))))

(defun* kite--fill-overflow (string width &key (align 'left) (trim 'right))
  (let ((string-length (length string)))
    (if (> string-length width)
        (if (eq 'right trim)
            (concat (substring string 0 (- width 3)) "...")
          (concat "..." (substring string (- string-length (- width 3)))))
      (let ((fill (- width string-length)))
        (cond
         ((eq 'left align)
          (concat string (make-string fill 32)))
         ((eq 'right align)
          (concat (make-string fill 32) string))
         (t
          (let* ((left-fill (/ fill 2))
                 (right-fill (- fill left-fill)))
            (concat (make-string left-fill 32)
                    string
                    (make-string left-fill 32)))))))))

(defun kite--log (format-string &rest args)
  "Print a message to the kite debug logging buffer"
  (with-current-buffer
      (get-buffer-create (format "*kite log*"))
    (save-excursion
      (goto-char (point-max))
      (insert (concat (apply 'format format-string args) "\n")))))

(defun kite--visit-remote-file (url)
  "Synchronously fetch the given URL, create a new read-only
buffer for its contents and switch to the buffer.  Invokes
`normal-mode' to guess the correct major mode for the new buffer.

If there is a buffer open visiting URL, just switch to that
buffer instead.

FIXME: Should use the HTTP Content-Type header to determine the
major mode more reliably."
  (switch-to-buffer
   (or
    (get-buffer url)
    (let ((new-buffer (generate-new-buffer url))
          (url-parts (url-generic-parse-url url)))
      (with-current-buffer new-buffer
        (setq buffer-file-name (url-filename url-parts))
        (save-excursion
          (insert
           (with-current-buffer
               (url-retrieve-synchronously url)
             (goto-char (point-min))
             (save-match-data
               (re-search-forward "\n\n" nil t))
             (buffer-substring-no-properties (point) (point-max)))))
        (setq buffer-read-only t)
        (set-buffer-modified-p nil)
        (normal-mode))
      new-buffer))))

(provide 'kite-util)

;;; kite-util.el ends here
