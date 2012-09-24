;;; kite-sourcemap.el --- Kite helpers for source map decoding

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

;; This package providers helper functions for decoding source maps
;; and looking up mappings in them.
;;
;; It is mostly a transliteration of Mozilla's code found at
;; https://github.com/mozilla/source-map/
;;
;; It is part of Kite, a WebKit inspector front-end.
;;
;; See also:
;; * http://www.html5rocks.com/en/tutorials/developertools/sourcemaps/
;; * https://github.com/mozilla/source-map/
;; * https://docs.google.com/document/d/1U1RGAehQwRypUTovF1KRlpiOFze0b-_2gc6fAH0KY0k/edit?pli=1#


;;; Code:

(require 'cl)

(defconst kite--vlq-base-shift 5)

(defconst kite--vlq-base (lsh 1 kite--vlq-base-shift))

(defconst kite--vlq-base-mask (- kite--vlq-base 1))

(defconst kite--vlq-continuation-bit kite--vlq-base)

(defconst kite--supported-source-map-version 3)

(defconst kite--base64-char-to-int-map
  (let* ((index 0)
         (base64-chars "\
ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
         (map (make-hash-table :size 64)))
    (dolist (char
             (string-to-list base64-chars))
      (puthash char index map)
      (incf index))
    map))

(defstruct (kite-source-mapping)
  "Holds the parsed mapping coordinates from the source map's
  `mappings' attribute."
  generated-line
  generated-column
  source
  original-line
  original-column
  name)

(defstruct (kite-source-map)
  "Representation of a parsed source map suitable for fast
lookup."
  names
  sources
  generated-mappings)

(defun kite--base64-decode (char)
  "Decode a single base64 character into its corresponding
integer value, or raise an error if the character is invalid."
  (or (gethash char kite--base64-char-to-int-map)
      (error "Invalid base 64 characters: %c" char)))

(defun kite--from-vlq-signed (value)
"Converts to a two-complement value from a value where the sign
bit is is placed in the least significant bit.  For example, as
decimals:
2 (10 binary) becomes 1,
3 (11 binary) becomes -1,
4 (100 binary) becomes 2,
5 (101 binary) becomes -2"
  (let ((shifted (lsh value -1)))
    (if (eq 1 (logand value 1))
        (- shifted)
      shifted)))

(defun kite--base64-vlq-decode (string-as-list)
  "Decode the next base 64 VLQ value from the given
STRING-AS-LIST and return the value and the rest of the string as
values, that is a list (VALUE STRING-REST)."
  (let ((result 0)
        (continuation t)
        (shift 0))
    (while continuation
      (when (null string-as-list)
        (error "Expected more digits in base 64 VLQ value"))
      (let ((digit (kite--base64-decode (car string-as-list))))
        (setq continuation
              (not (eq 0 (logand digit kite--vlq-continuation-bit))))
        (setq digit (logand digit kite--vlq-base-mask))
        (incf result (lsh digit shift)))
      (incf shift kite--vlq-base-shift)
      (setq string-as-list (cdr string-as-list)))
    (list :value (kite--from-vlq-signed result)
          :rest string-as-list)))

(defun kite--source-map-decode (source-map)
  "Decode SOURCE-MAP, which should be a deserialized JSON
object, and return a `kite-source-map' struct."

  (when (not (eq (plist-get source-map :version)
                 kite--supported-source-map-version))
    (error "Unsupported source map version %s"
           (plist-get source-map :version)))

  (let* ((source-root (plist-get source-map :sourceRoot))
         (names (plist-get source-map :names))
         (sources (plist-get source-map :sources))
         (string (string-to-list (plist-get source-map :mappings)))
         (result (make-kite-source-map
                  :names names
                  :sources sources))
         (generated-mappings-list)
         (generated-line 1)
         (previous-generated-column 0)
         (previous-original-line 0)
         (previous-original-column 0)
         (previous-source 0)
         (previous-name 0))
    (flet
     ((starts-with-mapping-separator (string)
                                     (or (null string)
                                         (eq (car string) ?,)
                                         (eq (car string) ?\;))))
     (while string
       (cond
        ((eq (car string) ?\;)
         (incf generated-line)
         (setq string (cdr string))
         (setq previous-generated-column 0))
        ((eq (car string) ?,)
         (setq string (cdr string)))
        (t
         (let ((mapping (make-kite-source-mapping
                         :generated-line generated-line)))

           ;; Generated column.
           (let ((temp (kite--base64-vlq-decode string)))
             (setf (kite-source-mapping-generated-column mapping)
                   (+ previous-generated-column
                      (plist-get temp :value)))
             (setq previous-generated-column
                   (kite-source-mapping-generated-column mapping))
             (setq string (plist-get temp :rest)))

           (when (not (starts-with-mapping-separator string))

             ;; Original source.
             (let ((temp (kite--base64-vlq-decode string)))
               (setf (kite-source-mapping-source mapping)
                     (concat source-root
                             (elt sources
                                  (+ previous-source
                                     (plist-get temp :value)))))
               (incf previous-source (plist-get temp :value))
               (setq string (plist-get temp :rest)))

             (when (starts-with-mapping-separator string)
               (error "Found a source, but no line and column"))

             ;; Original line.
             (let ((temp (kite--base64-vlq-decode string)))
               (setf (kite-source-mapping-original-line mapping)
                     (+ previous-original-line
                        (plist-get temp :value)))
               (setq previous-original-line
                     (kite-source-mapping-original-line mapping))

               ;; Lines are stored 0-based
               (incf (kite-source-mapping-original-line mapping))

               (setq string (plist-get temp :rest)))

             (when (starts-with-mapping-separator string)
               (error "Found a source and line, but no column"))

             ;; Original column
             (let ((temp (kite--base64-vlq-decode string)))
               (setf (kite-source-mapping-original-column mapping)
                     (+ previous-original-column
                        (plist-get temp :value)))
               (setq previous-original-column
                     (kite-source-mapping-original-column mapping))

               (setq string (plist-get temp :rest)))

             (when (not (starts-with-mapping-separator string))

               ;; Original name
               (let ((temp (kite--base64-vlq-decode string)))
                 (setf (kite-source-mapping-name mapping)
                       (elt names (+ previous-name
                                     (plist-get temp :value))))
                 (incf previous-name (plist-get temp :value))

                 (setq string (plist-get temp :rest)))))

           (push mapping generated-mappings-list)))))

     (setf (kite-source-map-generated-mappings result)
           (vconcat (nreverse generated-mappings-list))))
    result))

(defun kite-source-map-original-position-for (source-map
                                              line column)
  "Given SOURCE-MAP, which should be a `kite-source-map' struct
  as returned by `kite--source-map-decode', find the original
  position corresponding to LINE and COLUMN.  Return a plist with
  `:source', `:line', `:column', and `:name', or nil if not
  found."

  (when (<= line 0)
    (error "Line must be greater than or equal to 1"))
  (when (< column 0)
    (error "Column must be greater than or equal to 0."))

  ;; This is an implementation of binary search which will always try
  ;; and return the next lowest value checked if there is no exact
  ;; hit. This is because mappings between original and generated
  ;; line/col pairs are single points, and there is an implicit region
  ;; between each of them, so a miss just means that you aren't on the
  ;; very start of a region.

  (let* ((haystack (kite-source-map-generated-mappings source-map))
         (low -1)
         (high (length haystack))
         terminate
         found)

    (when (> (length source-map) 0)

      ;; This terminates when one of the following is true:
      ;;
      ;;   1. We find the exact element we are looking for.
      ;;
      ;;   2. We did not find the exact element, but we can return the
      ;;      next closest element that is less than that element.
      ;;
      ;;   3. We did not find the exact element, and there is no
      ;;      next-closest element which is less than the one we are
      ;;      searching for, so we return null.
      (while (not terminate)
        (let* ((mid (floor (+ (/ (- high low) 2) low)))
               (cur (elt haystack mid)))
          (cond
           ((and (eq (kite-source-mapping-generated-line cur) line)
                 (eq (kite-source-mapping-generated-column cur) column))
            ;; Found the element we are looking for.
            (setq found cur)
            (setq terminate t))

           ((or (< (kite-source-mapping-generated-line cur) line)
                (and (eq (kite-source-mapping-generated-line cur) line)
                     (< (kite-source-mapping-generated-column cur)
                        column)))
            ;; haystack[mid] is greater than our needle.
            (if (> (- high mid) 1)
                ;; The element is in the upper half.
                (setq low mid)
              ;; We did not find an exact match, return the next
              ;; closest one (termination case 2).
              (setq found cur)
              (setq terminate t)))

           (t
            ;; haystack[mid] is less than our needle.
            (if (> (- mid low) 1)
                ;; The element is in the lower half.
                (setq high mid)
              ;; The exact needle element was not found in this
              ;; haystack. Determine if we are in termination case (2)
              ;; or (3) and return the appropriate thing.
              (unless (< low 0)
                (setq found (elt haystack low)))
              (setq terminate t))))))

      (when found
        (list :source (kite-source-mapping-source found)
              :line (kite-source-mapping-original-line found)
              :column (kite-source-mapping-original-column found)
              :name (kite-source-mapping-name found))))))

(provide 'kite-sourcemap)

;;; kite-sourcemap.el ends here
