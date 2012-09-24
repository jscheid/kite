;;; kite-color.el --- Color-related functions for Kite

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

;; This package provides functions for parsing and generating color
;; representations according to the CSS3 Color Module, as well as
;; visualizing colors with and without alpha channel.
;;
;; It is part of Kite, a WebKit inspector front-end.


;;; Code:

(require 'cl)
(require 'color)
(eval-when-compile
  (require 'rx))

(defun* kite--make-color-image
    (rgba-float-color
     &key
     (width 16)
     (height 16)
     (checker-size 8)
     (bg-color-1 (color-hsl-to-rgb 0 0 1))
     (bg-color-2 (color-hsl-to-rgb 0 0 0.5)))
  "Return an image that visualizes the given RGBA-FLOAT-COLOR,
which should be a list containing four float values in the range
0..1.

WIDTH and HEIGHT determine the image dimensions in pixels.
Colors with an alpha value less than one (i.e. semi-transparent
colors) will be shown superimposed on top of a checkerboard.
CHECKER-SIZE gives the size of each checkerboard tile in pixels,
and BG-COLOR-1 and BG-COLOR-2 are lists consisting of three
floating point values each, giving the RGB color values to use
for the checkerboard tiles."
  (create-image
   (with-output-to-string
     (let ((scale 255)
           (row 0)
           (bg-color-list (list bg-color-1 bg-color-2))
           (alpha (nth 3 rgba-float-color)))
       (princ (format "P3\n%d %d\n%d\n" width height scale))
       (while (< row height)
         (let ((column 0))
           (while (< column width)
             (let ((bg-color (nth
                              (logxor (mod (/ column checker-size) 2)
                                      (mod (/ row checker-size) 2))
                              bg-color-list)))
               (dolist (component
                        (mapcar* (lambda (fg bg)
                                   (+ (* fg alpha)
                                      (* bg (- 1 alpha))))
                                 rgba-float-color bg-color))
                 (princ (format "%d " (round (* scale component)))))
               (setq column (1+ column)))))
         (princ "\n")
         (setq row (1+ row)))))
   'pbm t))

(eval-when-compile
  (defconst kite--color-keywords
    (let ((table (make-hash-table :test 'equal)))

      ;; 4.1. HTML4 color keywords
      (puthash "black" (color-name-to-rgb "#000000") table)
      (puthash "green" (color-name-to-rgb "#008000") table)
      (puthash "silver" (color-name-to-rgb "#C0C0C0") table)
      (puthash "lime" (color-name-to-rgb "#00FF00") table)
      (puthash "gray" (color-name-to-rgb "#808080") table)
      (puthash "olive" (color-name-to-rgb "#808000") table)
      (puthash "white" (color-name-to-rgb "#FFFFFF") table)
      (puthash "yellow" (color-name-to-rgb "#FFFF00") table)
      (puthash "maroon" (color-name-to-rgb "#800000") table)
      (puthash "navy" (color-name-to-rgb "#000080") table)
      (puthash "red" (color-name-to-rgb "#FF0000") table)
      (puthash "blue" (color-name-to-rgb "#0000FF") table)
      (puthash "purple" (color-name-to-rgb "#800080") table)
      (puthash "teal" (color-name-to-rgb "#008080") table)
      (puthash "fuchsia" (color-name-to-rgb "#FF00FF") table)
      (puthash "aqua" (color-name-to-rgb "#00FFFF") table)

      ;; 4.2.3. 'transparent' color keyword
      (puthash "transparent" '(0 0 0 0) table)

      ;; 4.3. SVG color keywords
      (puthash "aliceblue" (color-name-to-rgb "#F0F8FF") table)
      (puthash "antiquewhite" (color-name-to-rgb "#FAEBD7") table)
      (puthash "aqua" (color-name-to-rgb "#00FFFF") table)
      (puthash "aquamarine" (color-name-to-rgb "#7FFFD4") table)
      (puthash "azure" (color-name-to-rgb "#F0FFFF") table)
      (puthash "beige" (color-name-to-rgb "#F5F5DC") table)
      (puthash "bisque" (color-name-to-rgb "#FFE4C4") table)
      (puthash "black" (color-name-to-rgb "#000000") table)
      (puthash "blanchedalmond" (color-name-to-rgb "#FFEBCD") table)
      (puthash "blue" (color-name-to-rgb "#0000FF") table)
      (puthash "blueviolet" (color-name-to-rgb "#8A2BE2") table)
      (puthash "brown" (color-name-to-rgb "#A52A2A") table)
      (puthash "burlywood" (color-name-to-rgb "#DEB887") table)
      (puthash "cadetblue" (color-name-to-rgb "#5F9EA0") table)
      (puthash "chartreuse" (color-name-to-rgb "#7FFF00") table)
      (puthash "chocolate" (color-name-to-rgb "#D2691E") table)
      (puthash "coral" (color-name-to-rgb "#FF7F50") table)
      (puthash "cornflowerblue" (color-name-to-rgb "#6495ED") table)
      (puthash "cornsilk" (color-name-to-rgb "#FFF8DC") table)
      (puthash "crimson" (color-name-to-rgb "#DC143C") table)
      (puthash "cyan" (color-name-to-rgb "#00FFFF") table)
      (puthash "darkblue" (color-name-to-rgb "#00008B") table)
      (puthash "darkcyan" (color-name-to-rgb "#008B8B") table)
      (puthash "darkgoldenrod" (color-name-to-rgb "#B8860B") table)
      (puthash "darkgray" (color-name-to-rgb "#A9A9A9") table)
      (puthash "darkgreen" (color-name-to-rgb "#006400") table)
      (puthash "darkgrey" (color-name-to-rgb "#A9A9A9") table)
      (puthash "darkkhaki" (color-name-to-rgb "#BDB76B") table)
      (puthash "darkmagenta" (color-name-to-rgb "#8B008B") table)
      (puthash "darkolivegreen" (color-name-to-rgb "#556B2F") table)
      (puthash "darkorange" (color-name-to-rgb "#FF8C00") table)
      (puthash "darkorchid" (color-name-to-rgb "#9932CC") table)
      (puthash "darkred" (color-name-to-rgb "#8B0000") table)
      (puthash "darksalmon" (color-name-to-rgb "#E9967A") table)
      (puthash "darkseagreen" (color-name-to-rgb "#8FBC8F") table)
      (puthash "darkslateblue" (color-name-to-rgb "#483D8B") table)
      (puthash "darkslategray" (color-name-to-rgb "#2F4F4F") table)
      (puthash "darkslategrey" (color-name-to-rgb "#2F4F4F") table)
      (puthash "darkturquoise" (color-name-to-rgb "#00CED1") table)
      (puthash "darkviolet" (color-name-to-rgb "#9400D3") table)
      (puthash "deeppink" (color-name-to-rgb "#FF1493") table)
      (puthash "deepskyblue" (color-name-to-rgb "#00BFFF") table)
      (puthash "dimgray" (color-name-to-rgb "#696969") table)
      (puthash "dimgrey" (color-name-to-rgb "#696969") table)
      (puthash "dodgerblue" (color-name-to-rgb "#1E90FF") table)
      (puthash "firebrick" (color-name-to-rgb "#B22222") table)
      (puthash "floralwhite" (color-name-to-rgb "#FFFAF0") table)
      (puthash "forestgreen" (color-name-to-rgb "#228B22") table)
      (puthash "fuchsia" (color-name-to-rgb "#FF00FF") table)
      (puthash "gainsboro" (color-name-to-rgb "#DCDCDC") table)
      (puthash "ghostwhite" (color-name-to-rgb "#F8F8FF") table)
      (puthash "gold" (color-name-to-rgb "#FFD700") table)
      (puthash "goldenrod" (color-name-to-rgb "#DAA520") table)
      (puthash "gray" (color-name-to-rgb "#808080") table)
      (puthash "green" (color-name-to-rgb "#008000") table)
      (puthash "greenyellow" (color-name-to-rgb "#ADFF2F") table)
      (puthash "grey" (color-name-to-rgb "#808080") table)
      (puthash "honeydew" (color-name-to-rgb "#F0FFF0") table)
      (puthash "hotpink" (color-name-to-rgb "#FF69B4") table)
      (puthash "indianred" (color-name-to-rgb "#CD5C5C") table)
      (puthash "indigo" (color-name-to-rgb "#4B0082") table)
      (puthash "ivory" (color-name-to-rgb "#FFFFF0") table)
      (puthash "khaki" (color-name-to-rgb "#F0E68C") table)
      (puthash "lavender" (color-name-to-rgb "#E6E6FA") table)
      (puthash "lavenderblush" (color-name-to-rgb "#FFF0F5") table)
      (puthash "lawngreen" (color-name-to-rgb "#7CFC00") table)
      (puthash "lemonchiffon" (color-name-to-rgb "#FFFACD") table)
      (puthash "lightblue" (color-name-to-rgb "#ADD8E6") table)
      (puthash "lightcoral" (color-name-to-rgb "#F08080") table)
      (puthash "lightcyan" (color-name-to-rgb "#E0FFFF") table)
      (puthash "lightgoldenrodyellow" (color-name-to-rgb "#FAFAD2") table)
      (puthash "lightgray" (color-name-to-rgb "#D3D3D3") table)
      (puthash "lightgreen" (color-name-to-rgb "#90EE90") table)
      (puthash "lightgrey" (color-name-to-rgb "#D3D3D3") table)
      (puthash "lightpink" (color-name-to-rgb "#FFB6C1") table)
      (puthash "lightsalmon" (color-name-to-rgb "#FFA07A") table)
      (puthash "lightseagreen" (color-name-to-rgb "#20B2AA") table)
      (puthash "lightskyblue" (color-name-to-rgb "#87CEFA") table)
      (puthash "lightslategray" (color-name-to-rgb "#778899") table)
      (puthash "lightslategrey" (color-name-to-rgb "#778899") table)
      (puthash "lightsteelblue" (color-name-to-rgb "#B0C4DE") table)
      (puthash "lightyellow" (color-name-to-rgb "#FFFFE0") table)
      (puthash "lime" (color-name-to-rgb "#00FF00") table)
      (puthash "limegreen" (color-name-to-rgb "#32CD32") table)
      (puthash "linen" (color-name-to-rgb "#FAF0E6") table)
      (puthash "magenta" (color-name-to-rgb "#FF00FF") table)
      (puthash "maroon" (color-name-to-rgb "#800000") table)
      (puthash "mediumaquamarine" (color-name-to-rgb "#66CDAA") table)
      (puthash "mediumblue" (color-name-to-rgb "#0000CD") table)
      (puthash "mediumorchid" (color-name-to-rgb "#BA55D3") table)
      (puthash "mediumpurple" (color-name-to-rgb "#9370DB") table)
      (puthash "mediumseagreen" (color-name-to-rgb "#3CB371") table)
      (puthash "mediumslateblue" (color-name-to-rgb "#7B68EE") table)
      (puthash "mediumspringgreen" (color-name-to-rgb "#00FA9A") table)
      (puthash "mediumturquoise" (color-name-to-rgb "#48D1CC") table)
      (puthash "mediumvioletred" (color-name-to-rgb "#C71585") table)
      (puthash "midnightblue" (color-name-to-rgb "#191970") table)
      (puthash "mintcream" (color-name-to-rgb "#F5FFFA") table)
      (puthash "mistyrose" (color-name-to-rgb "#FFE4E1") table)
      (puthash "moccasin" (color-name-to-rgb "#FFE4B5") table)
      (puthash "navajowhite" (color-name-to-rgb "#FFDEAD") table)
      (puthash "navy" (color-name-to-rgb "#000080") table)
      (puthash "oldlace" (color-name-to-rgb "#FDF5E6") table)
      (puthash "olive" (color-name-to-rgb "#808000") table)
      (puthash "olivedrab" (color-name-to-rgb "#6B8E23") table)
      (puthash "orange" (color-name-to-rgb "#FFA500") table)
      (puthash "orangered" (color-name-to-rgb "#FF4500") table)
      (puthash "orchid" (color-name-to-rgb "#DA70D6") table)
      (puthash "palegoldenrod" (color-name-to-rgb "#EEE8AA") table)
      (puthash "palegreen" (color-name-to-rgb "#98FB98") table)
      (puthash "paleturquoise" (color-name-to-rgb "#AFEEEE") table)
      (puthash "palevioletred" (color-name-to-rgb "#DB7093") table)
      (puthash "papayawhip" (color-name-to-rgb "#FFEFD5") table)
      (puthash "peachpuff" (color-name-to-rgb "#FFDAB9") table)
      (puthash "peru" (color-name-to-rgb "#CD853F") table)
      (puthash "pink" (color-name-to-rgb "#FFC0CB") table)
      (puthash "plum" (color-name-to-rgb "#DDA0DD") table)
      (puthash "powderblue" (color-name-to-rgb "#B0E0E6") table)
      (puthash "purple" (color-name-to-rgb "#800080") table)
      (puthash "red" (color-name-to-rgb "#FF0000") table)
      (puthash "rosybrown" (color-name-to-rgb "#BC8F8F") table)
      (puthash "royalblue" (color-name-to-rgb "#4169E1") table)
      (puthash "saddlebrown" (color-name-to-rgb "#8B4513") table)
      (puthash "salmon" (color-name-to-rgb "#FA8072") table)
      (puthash "sandybrown" (color-name-to-rgb "#F4A460") table)
      (puthash "seagreen" (color-name-to-rgb "#2E8B57") table)
      (puthash "seashell" (color-name-to-rgb "#FFF5EE") table)
      (puthash "sienna" (color-name-to-rgb "#A0522D") table)
      (puthash "silver" (color-name-to-rgb "#C0C0C0") table)
      (puthash "skyblue" (color-name-to-rgb "#87CEEB") table)
      (puthash "slateblue" (color-name-to-rgb "#6A5ACD") table)
      (puthash "slategray" (color-name-to-rgb "#708090") table)
      (puthash "slategrey" (color-name-to-rgb "#708090") table)
      (puthash "snow" (color-name-to-rgb "#FFFAFA") table)
      (puthash "springgreen" (color-name-to-rgb "#00FF7F") table)
      (puthash "steelblue" (color-name-to-rgb "#4682B4") table)
      (puthash "tan" (color-name-to-rgb "#D2B48C") table)
      (puthash "teal" (color-name-to-rgb "#008080") table)
      (puthash "thistle" (color-name-to-rgb "#D8BFD8") table)
      (puthash "tomato" (color-name-to-rgb "#FF6347") table)
      (puthash "turquoise" (color-name-to-rgb "#40E0D0") table)
      (puthash "violet" (color-name-to-rgb "#EE82EE") table)
      (puthash "wheat" (color-name-to-rgb "#F5DEB3") table)
      (puthash "white" (color-name-to-rgb "#FFFFFF") table)
      (puthash "whitesmoke" (color-name-to-rgb "#F5F5F5") table)
      (puthash "yellow" (color-name-to-rgb "#FFFF00") table)
      (puthash "yellowgreen" (color-name-to-rgb "#9ACD32") table)
      table)
    "Hash map from lower-case CSS3 color keywords to the
corresponding RGB or (in the case of `transparent') RGBA values,
with each color component a float value in the range 0..1."))

(defconst kite-color-regexp
  ;; Shortcuts for use in the regex
  (eval-when-compile
    (let ((rx-constituents
           (append rx-constituents
                   (list
                    (cons 'integer-ws
                          (rx (0+ (syntax whitespace))
                              (group
                               (opt "-")
                               (or (sequence (1+ digit)
                                             (opt "." (0+ digit)))
                                   (sequence "." (1+ digit))))
                              (0+ (syntax whitespace))))
                    (cons 'float-ws
                          (rx (0+ (syntax whitespace))
                              (group
                               (opt "-")
                               (or (sequence (1+ digit)
                                             (opt "." (0+ digit)))
                                   (sequence "." (1+ digit))))
                              (0+ (syntax whitespace))))
                    (cons 'percent-ws
                          (rx (0+ (syntax whitespace))
                              (group
                               (opt "-")
                               (1+ digit))
                              (0+ (syntax whitespace))
                              "%"
                              (0+ (syntax whitespace))))))))
      (rx-to-string
       `(: string-start
           (or
            ;; 4.1. HTML4 color keywords
            ;; 4.2.3. 'transparent' color keywords
            ;; 4.3 SVG color keywords
            (group                      ; 1
             ,(append
               '(or)
               (let ((keys))
                 (maphash (lambda (key value)
                            (setq keys
                                  (cons
                                   (append
                                    '(sequence)
                                    (mapcar
                                     (lambda (chr) (list 'char
                                                         (downcase chr)
                                                         (upcase chr)))
                                     (string-to-list key)))
                                   keys)))
                          (eval-when-compile kite--color-keywords))
                 keys)))

            ;; 4.2.1. RGB color values

            ;; #FFFFFF notation
            (group                      ; 2
             "#"
             (= 6 hex-digit))

            ;; #FFF notation
            (group                      ; 3
             "#"
             (= 3 hex-digit))

            ;; rgb notation, 0-255 values
            (: "rgb("
               integer-ws               ; 4
               ","
               integer-ws               ; 5
               ","
               integer-ws               ; 6
               ")")

            ;; rgb notation, 0%-100% values
            (: "rgb("
               percent-ws               ; 7
               ","
               percent-ws               ; 8
               ","
               percent-ws               ; 9
               ")")

            ;; 4.2.2. RGBA color values

            ;; rgba notation, 0-255 values
            (: "rgba("
               integer-ws               ; 10
               ","
               integer-ws               ; 11
               ","
               integer-ws               ; 12
               ","
               float-ws                 ; 13
               ")")

            ;; rgba notation, 0%-100% values
            (: "rgba("
               percent-ws               ; 14
               ","
               percent-ws               ; 15
               ","
               percent-ws               ; 16
               ","
               float-ws                 ; 17
               ")")

            ;; 4.2.4 HSL color values
            (: "hsl("
               integer-ws               ; 18
               ","
               percent-ws               ; 19
               ","
               percent-ws               ; 20
               ")")

            ;; 4.2.5 HSLA color values
            (: "hsla("
               integer-ws               ; 21
               ","
               percent-ws               ; 22
               ","
               percent-ws               ; 23
               ","
               float-ws                 ; 24
               ")"))
           string-end))))
  "Regular expression matching any of the color representations
described in the CSS3 Color Module")

(defun kite-parse-color-match (string)
  "Return the RGB or RGBA color values corresponding to the color
last matched by `kite-color-regexp', as per the CSS3 Color
Module (W3C Candidate Recommendation 14 May 2003) section 4.  See
http://www.w3.org/TR/2003/CR-css3-color-20030514 .

STRING should be given if the last search was by `string-match'
on STRING.  If STRING is nil, the current buffer should be the
same buffer the search/match was performed in.

HSL values will be converted to RGB, and all components will be
normalized and clamped to the range 0..1 as per the spec.

The returned value is either a list of three float values in the
range 0..1 corresponding to red, green, and blue components (if
the matched color did not have an alpha component) or a list of
four float values in the range 0..1 corresponding to red, green,
blue, and alpha otherwise.  This function does not deal in color
spaces or color profiles and thus its result should be treated as
a 'raw' color value."
  (flet
   ((clamp (n)
           (max 0.0 (min 1.0 n)))
    (color-hue-to-rgb (v1 v2 h)
                      (cond
                       ((< h 0) (setq h (+ h 1)))
                       ((> h 1) (setq h (- h 1))))
                      (cond
                       ((< h (/ 1.0 6))
                        (+ v1 (* (- v2 v1) h 6.0)))
                       ((< h 0.5)
                        v2)
                       ((< h (/ 2.0 3))
                        (+ v1 (* (- v2 v1) (- (/ 2.0 3) h) 6.0)))
                       (t
                        v1))))
   (cond

    ;; color keyword
    ((match-string 1 string)
     (gethash (match-string 1 string) (eval-when-compile kite--color-keywords)))

    ;; #FFFFFF
    ((match-string 2 string)
     (color-name-to-rgb (match-string 2 string)))

    ;; #FFF
    ((match-string 3 string)
     (let ((match (match-string 3 string)))
       (list (/ (string-to-number (substring match 1 2) 16) 15.0)
             (/ (string-to-number (substring match 2 3) 16) 15.0)
             (/ (string-to-number (substring match 3 4) 16) 15.0))))

    ;; rgb(n,n,n)
    ((match-string 4 string)
     (list (clamp (/ (string-to-number (match-string 4 string)) 255.0))
           (clamp (/ (string-to-number (match-string 5 string)) 255.0))
           (clamp (/ (string-to-number (match-string 6 string)) 255.0))))

    ;; rgb(n%,n%,n%)
    ((match-string 7 string)
     (list (clamp (/ (string-to-number (match-string 7 string)) 100.0))
           (clamp (/ (string-to-number (match-string 8 string)) 100.0))
           (clamp (/ (string-to-number (match-string 9 string)) 100.0))))

    ;; rgba(n,n,n,a)
    ((match-string 10 string)
     (list (clamp (/ (string-to-number (match-string 10 string)) 255.0))
           (clamp (/ (string-to-number (match-string 11 string)) 255.0))
           (clamp (/ (string-to-number (match-string 12 string)) 255.0))
           (clamp (string-to-number (match-string 13 string)))))

    ;; rgba(n%,n%,n%,a)
    ((match-string 14 string)
     (list (clamp (/ (string-to-number (match-string 14 string)) 100.0))
           (clamp (/ (string-to-number (match-string 15 string)) 100.0))
           (clamp (/ (string-to-number (match-string 16 string)) 100.0))
           (clamp (string-to-number (match-string 17 string)))))

    ;; hsl(n,n%,n%)
    ((match-string 18 string)
     (mapcar #'clamp
             (color-hsl-to-rgb
              (mod (/ (string-to-number (match-string 18 string)) 360.0) 1.0)
              (clamp (/ (string-to-number (match-string 19 string)) 100.0))
              (clamp (/ (string-to-number (match-string 20 string)) 100.0)))))

    ;; hsla(n,n%,n%,a)
    ((match-string 21 string)
     (append
      (mapcar #'clamp
              (color-hsl-to-rgb
               (mod (/ (string-to-number (match-string 21 string)) 360.0) 1.0)
               (clamp (/ (string-to-number (match-string 22 string)) 100.0))
               (clamp (/ (string-to-number (match-string 23 string)) 100.0))))
      (list
       (clamp (string-to-number (match-string 24 string)))))))))

(defun kite-parse-color (string)
  (when (let ((case-fold-search nil))
          (string-match kite-color-regexp string))
    (kite-parse-color-match string)))

(defun kite--rgba-value (color)
  "If COLOR is an RGB triplet, turn it into an RGBA quadruplet by
appending an alpha value of 1.  Otherwise, return the value
as-is"
  (if (eq 3 (length color))
      (append color (list 1))
    color))

(provide 'kite-color)

;;; kite-color.el ends here
