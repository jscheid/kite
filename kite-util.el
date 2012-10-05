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

(require 'kite-global)
(require 'cl)
(require 'color)
(require 'url-parse)
(require 'wid-edit)

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
  (flet
   ((lerp (a b w)
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
               (not (every
                     (apply-partially 'string-prefix-p prefix-candidate)
                     strings)))
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

(defun kite--widget-field-activate (pos &optional event)
  "Invoke the editable field at point."
  (interactive "@d")
  (let ((field (or (widget-field-at pos)
                   (widget-field-at (- pos 1)))))
    (if field
        (widget-apply-action field event)
      (call-interactively
       (lookup-key widget-global-map (this-command-keys))))))
(defconst kite--identifier-part-regexp
  (rx
   word-boundary
   (1+ (or alnum
           ?.
           (: "\\x" (repeat 2 xdigit))
           (: "\\u" (repeat 4 xdigit))))
   point)
  "Used by `kite-async-completion-at-point' to find a part of a
JavaScript identifier.")

(defconst kite--stack-line-regexp
  (eval-when-compile
    (let ((rx-constituents
           (append rx-constituents
                   (list
                    (cons 'file-line-column
                          (rx
                           (or
                            (submatch     ; <file>
                             "<"
                             (minimal-match (1+ anything))
                             ">")
                            (submatch     ; file
                             (minimal-match (1+ anything))))
                           ":"
                           (submatch      ; line
                            (1+ (in digit)))
                           ":"
                           (submatch      ; column
                            (1+ (in digit)))))))))

      (rx-to-string
       '(: string-start
           (or
            (: (submatch                  ; error type
                (1+ (in alnum)))
               ":"
               (0+ (in space))
               (submatch                  ; error message
                (0+ anything)))
            (: (1+ (in space))
               "at"
               (1+ (in space))
               (or
                (: file-line-column)
                (: (submatch              ; function
                    (1+ (not (in space))))
                   (1+ (in space))
                   "("
                   file-line-column
                   ")"))))
           (0+ (in space))
           string-end)))))

(defun kite--format-stack-line (stack-line)
  "Return a prettified version of a line of a WebKit stack trace.
  adds face and font-lock-face properties, and the kite-source
  property for lines that describe a source location."
  (when (string-match kite--stack-line-regexp stack-line)
    (let* ((matches (match-data t))
           (faces
            '(nil
              kite-stack-error-type
              kite-stack-error-message
              kite-stack-pseudo-file-name
              kite-stack-file-name
              kite-stack-line-number
              kite-stack-column-number
              kite-stack-function-name
              kite-stack-pseudo-file-name
              kite-stack-file-name
              kite-stack-line-number
              kite-stack-column-number)))
      (loop for i from 1 to (- (/ (length matches) 2) 1) do
            (dolist (property '(face font-lock-face))
              (when (nth (* 2 i) matches)
                (put-text-property (nth (* 2 i) matches)
                                   (nth (1+ (* 2 i)) matches)
                                   property
                                   (nth i faces)
                                   stack-line))))
      (let ((file-name
             (or (match-string-no-properties 4 stack-line)
                 (match-string-no-properties 9 stack-line)))
            (line-number-str
             (or (match-string-no-properties 5 stack-line)
                 (match-string-no-properties 10 stack-line)))
            (column-number-str
             (or (match-string-no-properties 6 stack-line)
                 (match-string-no-properties 11 stack-line)))
            (function-name
             (match-string-no-properties 7 stack-line)))
        (when file-name
          (put-text-property
           0
           (length stack-line)
           'kite-source
           (list :url file-name
                 :lineNumber (string-to-number line-number-str)
                 :columnNumber (string-to-number column-number-str)
                 :functionName function-name)
           stack-line)))))
  stack-line)

(defun kite--get-formatted-stack-trace (error-object-id
                                        callback-function)
  "Invoke CALLBACK-FUNCTION asynchronously with one argument, the
full prettified stack trace for the error object with the given
ERROR-OBJECT-ID."
  (lexical-let ((lex-callback-function callback-function))
    (kite-send
     "Runtime.callFunctionOn"
     :params
     (list :objectId error-object-id
           :functionDeclaration "function f() { return this.stack; }"
           :arguments [])
     :success-function
     (lambda (result)
       (funcall lex-callback-function
                (concat
                 (mapconcat
                  (function kite--format-stack-line)
                  (split-string (kite--get result
                                           :result
                                           :value)
                                "\n")
                  "\n")
                 "\n"))))))

(defun kite--eval-in-current-context (input success-function)
  "Evaluate INPUT in the remote remote debugger in the current
execution context and asynchronously invoke SUCCESS-FUNCTION with
the results in case of success."
  (let ((eval-params (list :expression input))
        (context-id (plist-get (kite-session-current-context
                                kite-session)
                               :id)))
    (when context-id
      (setq eval-params (plist-put eval-params :contextId context-id)))
    (kite-send
     "Runtime.evaluate"
     :params
     eval-params
     :success-function
     success-function)))

(defun kite--rgba (r g b a)
  "Return the given RGBA color value in the WebKit remote
debugger API `RGBA' structure format."
  `((r . ,r)
    (g . ,g)
    (b . ,b)
    (a . ,a)))

(provide 'kite-util)

;;; kite-util.el ends here
