;;; kite-dom-css.el --- CSS helpers for DOM module implementation

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

;; This package provides CSS helper functions used by the DOM module.
;;
;; It is part of Kite, a WebKit inspector front-end.


;;; Code:

(require 'kite-color)
(require 'kite-global)
(require 'kite-util)

(require 'widget)
(require 'wid-edit)
;; Try loading css-mode so we can steal their faces
(require 'css-mode nil t)

(defface kite-css-selector
  (if (facep 'css-selector)
      '((t (:inherit css-selector)))
    '((t :inherit font-lock-function-name-face)))
  "Face to use for selectors."
  :group 'kite)

(defface kite-css-property
  (if (facep 'css-property)
      '((t (:inherit css-property)))
    '((t :inherit font-lock-variable-name-face)))
  "Face to use for properties."
  :group 'kite)

(defface kite-css-computed-unused-property
  `((t :inherit kite-css-property
       :foreground ,(kite--dimmed-face-foreground 'kite-css-property 0.5)))
  "Face to use for computed properties that are unused."
  :group 'kite)

(defface kite-css-proprietary-property
  (if (facep 'css-proprietary-property)
      '((t (:inherit css-proprietary-property)))
    '((t :inherit (css-property italic))))
  "Face to use for vendor-specific properties."
  :group 'kite)

(defface kite-css-computed-proprietary-unused-property
  `((t :inherit kite-css-proprietary-property
       :foreground ,(kite--dimmed-face-foreground 'kite-css-property 0.5)))
  "Face to use for computed vendor-specific properties that are unused."
  :group 'kite)

(defface kite-css-value-widget-modified
  `((t :foreground ,(face-foreground 'warning nil t)
       :background ,(face-background 'widget-field nil t)))
  "Face to use for CSS value widget with unparseable input."
  :group 'kite)

(defface kite-css-value-widget-error
  `((t :foreground ,(face-foreground 'error nil t)
       :background ,(face-background 'widget-field nil t)))
  "Face to use for CSS value widget with unparseable input."
  :group 'kite)

(defface kite-css-selected-overlay
  `((t :inherit secondary-selection))
  "Face to use for highlighting selected CSS property."
  :group 'kite)

(defconst kite-css-completions
  '((azimuth . [far-left left center-left center center-right right
                         far-right leftwards rightwards inherit])
    (background . [inherit])
    (background-attachment . [scroll fixed inherit])
    (background-color . [transparent inherit])
    (background-image . [none inherit])
    (background-position . [left center top center center center
                                 inherit])
    (background-repeat . [repeat repeat-x repeat-y no-repeat inherit])
    (border . [inherit])
    (border-collapse . [collapse separate inherit])
    (border-color . [inherit])
    (border-left . [inherit])
    (border-left-color . [transparent inherit])
    (border-left-style . [inherit])
    (border-left-width . [inherit])
    (border-spacing . [inherit])
    (border-style . [inherit])
    (border-width . [inherit])
    (bottom . [auto inherit])
    (caption-side . [top bottom inherit])
    (clear . [none left right both inherit])
    (clip . [auto inherit])
    (color . [inherit])
    (content . [normal none open-quote close-quote no-open-quote
                       inherit])
    (counter-increment . [none inherit])
    (counter-reset . [none inherit])
    (cue . [inherit])
    (cue-after . [none inherit])
    (cue-before . [none inherit])
    (cursor . [crosshair default pointer move e-resize ne-resize
                         nw-resize n-resize se-resize sw-resize s-resize
                         w-resize text wait help inherit])
    (direction . [ltr rtl inherit])
    (display . [inline block list-item inline-block table inline-table
                       table-row-group table-header-group
                       table-footer-group table-row table-column-group
                       table-column table-cell table-caption none
                       inherit])
    (elevation . [below level above higher lower inherit])
    (empty-cells . [show hide inherit])
    (float . [left right none inherit])
    (font . [caption icon menu message-box small-caption status-bar
                     inherit])
    (font-family . [inherit])
    (font-size . [inherit])
    (font-style . [normal italic oblique inherit])
    (font-variant . [normal small-caps inherit])
    (font-weight . [normal bold bolder lighter 100 200 300 400 500 600
                           700 800 900 inherit])
    (height . [auto inherit])
    (left . [auto inherit])
    (letter-spacing . [normal inherit])
    (line-height . [normal inherit])
    (list-style . [inherit])
    (list-style-image . [none inherit])
    (list-style-position . [inside outside inherit])
    (list-style-type . [disc circle square decimal decimal-leading-zero
                             lower-roman upper-roman lower-greek
                             lower-latin upper-latin armenian georgian
                             lower-alpha upper-alpha none inherit])
    (margin . [inherit])
    (margin-bottom . [inherit])
    (margin-left . [inherit])
    (max-height . [none inherit])
    (max-width . [none inherit])
    (min-height . [inherit])
    (min-width . [inherit])
    (orphans . [inherit])
    (outline . [inherit])
    (outline-color . [invert inherit])
    (outline-style . [inherit])
    (outline-width . [inherit])
    (overflow . [visible hidden scroll auto inherit])
    (padding . [inherit])
    (padding-left . [inherit])
    (page-break-after . [auto always avoid left right inherit])
    (page-break-before . [auto always avoid left right inherit])
    (page-break-inside . [avoid auto inherit])
    (pause . [inherit])
    (pause-after . [inherit])
    (pause-before . [inherit])
    (pitch . [x-low low medium high x-high inherit])
    (pitch-range . [inherit])
    (play-during . [auto none inherit])
    (position . [static relative absolute fixed inherit])
    (quotes . [none inherit])
    (richness . [inherit])
    (right . [auto inherit])
    (speak . [normal none spell-out inherit])
    (speak-header . [once always inherit])
    (speak-numeral . [digits continuous inherit])
    (speak-punctuation . [code none inherit])
    (speech-rate . [x-slow slow medium fast x-fast faster slower
                           inherit])
    (stress . [inherit])
    (table-layout . [auto fixed inherit])
    (text-align . [left right center justify inherit])
    (text-decoration . [none overline line-through inherit])
    (text-indent . [inherit])
    (text-transform . [capitalize uppercase lowercase none inherit])
    (top . [auto inherit])
    (unicode-bidi . [normal embed bidi-override inherit])
    (vertical-align . [baseline sub super top text-top middle bottom
                                text-bottom inherit])
    (visibility . [visible hidden collapse inherit])
    (voice-family . [inherit])
    (volume . [silent x-soft soft medium loud x-loud inherit])
    (white-space . [normal pre nowrap pre-wrap pre-line inherit])
    (widows . [inherit])
    (width . [auto inherit])
    (word-spacing . [normal inherit])
    (z-index . [auto inherit]))
  "CSS 2.1 property completions.  Automatically extracted by
extract-css2.py")

(defvar kite-css-widget-field-keymap
  (let ((map (copy-keymap widget-field-keymap)))
    (define-key map "\C-m" 'kite--widget-field-activate)
    map)
  "Custom keymap for editing CSS property values.  Overrides RET
so that it works also at the very end of the field.")

(defun kite-dom--render-property (css-rule property-index property indent)
  (widget-insert
   (make-string (* 2 indent) 32))

  (when (not (null (plist-get property :text)))
    (widget-create 'checkbox
                   :notify (lambda (widget &rest ignore)
                             (kite-send "CSS.toggleProperty"
                                        :params
                                        (list :styleId (widget-get widget :kite-css-style-id)
                                              :propertyIndex (widget-get widget :kite-css-property-index)
                                              :disable (if (widget-value widget) :json-false t))))
                   :help-echo "Enable/disable this CSS property"
                   :kite-css-style-id (kite--get css-rule :style :styleId)
                   :kite-css-property-index property-index
                   t))
  (widget-insert
   (concat
    " "
    (propertize
     (plist-get property :name)
     'face 'kite-css-property
     'font-lock-face 'kite-css-property)
    ":"))

  (let ((color (kite-parse-color
                (plist-get property :value))))
    (when color
        (let ((inhibit-read-only t))
          (widget-insert " ")
          (insert-image (kite--make-color-image (kite--rgba-value color))))))

  (widget-create 'editable-field
                 :format " %v;"
                 :size 1
                 :action (lambda (widget changed-widget &optional event)
                           (kite-send
                            "CSS.setPropertyText"
                            :params
                            (list :styleId (widget-get widget :kite-css-style-id)
                                  :propertyIndex (widget-get widget :kite-css-property-index)
                                  :text (concat (widget-get widget :kite-css-property-name)
                                                ": "
                                                (widget-value widget)
                                                ";")
                                  :overwrite t)
                            :success-function
                            (lexical-let ((lex-property-name (widget-get widget :kite-css-property-name))
                                          (lex-widget widget))
                              (lambda (result)
                                (mapcar (lambda (property)
                                          (when (string= (plist-get property :name)
                                                         lex-property-name)
                                            (overlay-put (widget-get lex-widget :field-overlay)
                                                         'face
                                                         (if (eq (plist-get property :parsedOk) :json-false)
                                                             'kite-css-value-widget-error
                                                           'widget-field))))
                                        (kite--get result :style :cssProperties))))))
                 :notify (lambda (widget &rest ignore)
                           (overlay-put (widget-get widget :field-overlay)
                                        'face
                                        'kite-css-value-widget-modified))
                 :kite-css-style-id (kite--get css-rule :style :styleId)
                 :kite-css-property-index property-index
                 :kite-css-property-name (plist-get property :name)
                 :completions (cdr (assoc (intern (plist-get property :name))
                                          kite-css-completions))
                 :keymap kite-css-widget-field-keymap
                 (plist-get property :value))

  (put-text-property (save-excursion (beginning-of-line) (point))
                     (point)
                     'kite-css-property-index property-index)
  (widget-insert "\n"))

(defun kite-dom--render-css-rule (css-rule)
  (let ((begin (point)))
    (widget-insert (concat
                    (propertize
                     (plist-get css-rule :selectorText)
                     'face 'kite-css-selector
                     'font-lock-face 'kite-css-selector)
                    " {\n" ))
    (let* ((style (plist-get css-rule :style))
           (shorthand-entries
            (let* ((hashtable (make-hash-table :test 'equal))
                   (entries (plist-get style :shorthandEntries))
                   (index 0)
                   (count (length entries)))
              (while (< index count)
                (let ((element (elt entries index)))
                  (puthash (plist-get element :name)
                           element
                           hashtable))
                (setq index (1+ index)))
              hashtable))
           (properties (plist-get style :cssProperties))
           (property-index 0)
           (property-count (length properties)))

      (kite--log "shorthand-entries %s" shorthand-entries)
      (while (< property-index property-count)
        (let* ((property (elt properties property-index))
               (shorthand-name (plist-get property :shorthandName))
               (shorthand-property
                (and shorthand-name
                     (gethash shorthand-name
                              shorthand-entries))))
          (when shorthand-property
            (kite-dom--render-property css-rule
                                       property-index
                                       shorthand-property
                                       1)
            (remhash shorthand-name shorthand-entries))
          (kite-dom--render-property css-rule
                                     property-index
                                     property
                                     (if shorthand-name 2 1)))
        (setq property-index (1+ property-index))))
    (widget-insert "}\n\n")
    (put-text-property begin (point)
                       'kite-css-rule css-rule)))

(defvar kite-css-mode-map
  (let ((map (make-composed-keymap
              (copy-keymap widget-keymap)
              (copy-keymap special-mode-map)))
        (menu-map (make-sparse-keymap)))
    (suppress-keymap map t)
    (kite--define-global-mode-keys map)
    (define-key map "\C-cg" 'kite--css-goto-source)
    map)
  "Local keymap for `kite-css-mode' buffers.")

(define-derived-mode kite-css-mode special-mode "kite-css"
  "Toggle Kite CSS mode."
  :group 'kite
  (setq kite-buffer-type 'css)
  (setq buffer-read-only nil))

(defun kite--dom-create-css-buffer (matched-css-rules)
  (kite--log
   (concat "kite--dom-create-css-buffer called with rules:\n"
           (pp-to-string matched-css-rules)))
  (let* ((-kite-session kite-session)
         (node-region-buffer (get-buffer-create "*kite css*"))
         (max-width 0)
         (window (display-buffer
                  node-region-buffer
                  '((display-buffer-pop-up-window . nil)))))
    (with-current-buffer node-region-buffer
      (kite-css-mode)
      (set (make-local-variable 'kite-session) -kite-session)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (remove-overlays)
        (save-excursion
          (widget-insert
           (concat
            (propertize "element.style"
                        'face 'kite-css-selector
                        'font-lock-face 'kite-css-selector)
            " {\n"
            "}\n"
            "\n"))

          (let ((rule-index (- (length matched-css-rules) 1)))
            (while (>= rule-index 0)
              (kite-dom--render-css-rule
               (elt matched-css-rules rule-index))
              (setq rule-index (- rule-index 1)))))
        (widget-setup)))))

(defun kite-dom-show-matched-css ()
  (interactive)

  (kite-send
   "CSS.getMatchedStylesForNode"
   :params
   (list :nodeId (get-char-property (point) 'kite-node-id))
   :success-function
   (lambda (result)
     (kite--log (pp-to-string result))
     (kite--dom-create-css-buffer
      (plist-get result :matchedCSSRules)))))

(defun kite--dom-make-style-to-rule-map (matched-styles-result)

  (let ((style-to-rule-map (make-hash-table)))
    (let* ((matched-css-rules (plist-get matched-styles-result :matchedCSSRules))
           (num-matched-rules (length matched-css-rules))
           (matched-rule-index 0))
      (while (< matched-rule-index num-matched-rules)
        (let* ((matched-rule (elt matched-css-rules matched-rule-index))
               (rule-info (list (plist-get matched-rule :selectorText)
                                (plist-get matched-rule :origin)
                                (plist-get matched-rule :sourceLine)))
               (style-arr (plist-get (plist-get matched-rule :style) :cssProperties))
               (num-styles (length style-arr))
               (style-index 0))
          (while (< style-index num-styles)
            (let ((style-name (plist-get (elt style-arr style-index) :name))
                  (style-value (plist-get (elt style-arr style-index) :value)))
              (puthash (intern style-name)
                       (cons (cons rule-info
                                   style-value)
                             (gethash (intern style-name)
                                      style-to-rule-map)) style-to-rule-map))
            (setq style-index (1+ style-index))))
        (setq matched-rule-index (1+ matched-rule-index))))
    style-to-rule-map))

(defun kite--dom-insert-computed-style-value (value)
  (let ((color (kite-parse-color value)))
    (if color
        (let ((inhibit-read-only t))
          (insert-image (kite--make-color-image (kite--rgba-value color)))
          (insert " ")
          (insert value))
      (insert value))))

(defun kite--dom-render-computed-css (computed-styles-result matched-styles-result)
  (let* ((style-to-rule-map (kite--dom-make-style-to-rule-map matched-styles-result))
         (arr (plist-get computed-styles-result :computedStyle))
         (index 0)
         (arr-len (length arr))
         as-list)
    (while (< index arr-len)
      (let ((item (elt arr index)))
        (setq as-list (cons (cons (plist-get item :name)
                                  (plist-get item :value))
                            as-list)))
      (setq index (1+ index)))
    (let ((sorted-styles
           (sort as-list
                 (lambda (x y) (string< (car x) (car y))))))

      (let* ((node-region-buffer (get-buffer-create "*kite dom node details*"))
             (max-width 0)
             (window (display-buffer node-region-buffer (list (cons 'display-buffer-pop-up-window nil)))))
        (with-current-buffer node-region-buffer
          (special-mode)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (save-excursion
              (while sorted-styles
                (let* ((style (car sorted-styles))
                       (matched-styles (gethash (intern (car style)) style-to-rule-map)))
                  (insert (propertize (car style)
                                      'face
                                      (if (null (string-match "^-" (car style)))
                                          (if matched-styles
                                              'kite-css-property
                                            'kite-css-computed-unused-property)
                                        (if matched-styles
                                            'kite-css-proprietary-property
                                          'kite-css-computed-proprietary-unused-property))))
                  (insert ": ")
                  (kite--dom-insert-computed-style-value (cdr style))
                  (setq max-width (max max-width (current-column)))
                  (insert "\n")

                  (while matched-styles
                    (let ((match-info (car matched-styles)))
                      (insert (concat "  "
                                      (propertize
                                       (caar match-info)
                                       'face 'kite-css-selector)
                                      " - "
                                      (cdr match-info)
                                      "\n"))
                      (when (string= "user-agent" (nth 1 (car match-info)))
                        (insert "  user agent stylesheet\n")))
                    (setq matched-styles (cdr matched-styles))))

                (setq sorted-styles (cdr sorted-styles))))))
        (when (< max-width (window-width window))
          (with-selected-window window
            (shrink-window-horizontally (- (window-width window) max-width))))))))

(defun kite-dom-show-computed-css ()
  (interactive)
  (lexical-let ((barrier (cons nil nil))
                (node-id (get-char-property (point) 'kite-node-id)))
    (kite-send "CSS.getComputedStyleForNode"
               :params
               (list :nodeId node-id)
               :success-function
               (lambda (result)
                 (setcar barrier result)
                 (when (and (not (null (car barrier)))
                            (not (null (cdr barrier))))
                   (kite--dom-render-computed-css
                    (car barrier) (cdr barrier)))))
    (kite-send "CSS.getMatchedStylesForNode"
               :params
               (list :nodeId node-id)
               :success-function
               (lambda (result)
                 (setcdr barrier result)
                 (when (and (not (null (car barrier)))
                            (not (null (cdr barrier))))
                   (kite--dom-render-computed-css
                    (car barrier) (cdr barrier)))))))

(defun kite--dom-timing-function-image (width height args)
  "Create an image that visualizes the timing function as a cubic
bezier curve.  WIDTH and HEIGHT are the image dimensions in
pixels.  ARGS are four floating point numbers as per CSS
Transitions Module Level 3 section 2.3"
  (create-image
   (format
    (concat
     "<?xml version='1.0'?>\n"
     "<!DOCTYPE svg PUBLIC '-//W3C//DTD SVG 1.1//EN'\n"
     "  'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'>\n"
     "<svg xmlns='http://www.w3.org/2000/svg'\n"
     "     width='%spx'\n"
     "     height='%spx'>\n"
     " <g transform='translate(0, %s)'>\n"
     "  <g transform='scale(1, -1)'>\n"
     "   <path d='M0,0 C%s,%s %s,%s %s,%s' style='fill: none; stroke: red; stroke-width: 1px'/>\n"
     "  </g>"
     " </g>"
     "</svg>")
    width height height
    (* width (nth 0 args))
    (* height (nth 1 args))
    (* width (nth 2 args))
    (* height (nth 3 args))
    width height
    )
   'svg
   t))

(defun kite--css-goto-source ()
  "Open the CSS source stylesheet in a buffer, highlight the
region corresponding to the rule or property under point, and
place point at region end.

FIXME: the range information seems to change when updating
property values, causing a mismatch with the source CSS text if
that wasn't updated as well.  Need to investigate whether we need
to store original range information before any property updates."
  (interactive)
  (let* ((css-rule (get-text-property (point) 'kite-css-rule))
         (url (kite--get css-rule :sourceURL))
         (rule-range (kite--get css-rule
                               :style
                               :range))
         (property-index (get-text-property
                          (point)
                          'kite-css-property-index)))

    (if url
        (progn
          (kite--visit-remote-file url)
          (remove-overlays)
          (when rule-range
            (let* ((property-range (when property-index
                                     (kite--get css-rule
                                                :style
                                                :cssProperties
                                                property-index
                                                :range)))
                   (start (+ (plist-get rule-range :start)
                             (or (plist-get property-range :start) 0)
                             1))
                   (end (+ (if property-range
                               (+ (plist-get rule-range :start)
                                  (plist-get property-range :end))
                             (plist-get rule-range :end))
                           1)))
              (when (and start end)
                (overlay-put (make-overlay start end)
                             'face 'kite-css-selected-overlay))
              (when end (goto-char end)))))
      (error "Source location not available"))))

(provide 'kite-dom-css)

;;; kite-dom-css.el ends here
