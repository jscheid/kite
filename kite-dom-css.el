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

(require 'kite-util)

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
  "Face to use for computed vendore-specific properties that are unused."
  :group 'kite)

(defun kite-dom--render-property (css-rule property-index property indent)
  (widget-insert
   (propertize
    (make-string (* 2 indent) 32)
    'kite-css-property (list css-rule property-index)))
  (when (not (null (plist-get property :text)))
    (widget-create 'checkbox
                   :notify (lambda (widget &rest ignore)
                             (kite-send "CSS.toggleProperty"
                                        (list :styleId (widget-get widget :kite-css-style-id)
                                              :propertyIndex  (widget-get widget :kite-css-property-index)
                                              :disable (if (widget-value widget) :json-false t))))
                   :help-echo "Enable/disable this CSS property"
                   :kite-css-style-id (kite--get css-rule :style :styleId)
                   :kite-css-property-index property-index
                   t))
  (widget-insert
   (propertize
    (concat
     " "
     (propertize
      (plist-get property :name)
      'face 'kite-css-property
      'font-lock-face 'kite-css-property)
     ": ")
    'kite-css-property (list css-rule property-index)))

  (let ((color (kite-parse-color
                (plist-get property :value))))
    (when color
        (let ((inhibit-read-only t))
          (insert-image (kite--make-color-image (kite--rgba-value color)))
          (widget-insert " "))))

  (widget-insert
   (propertize
    (concat
     (plist-get property :value)
     ";\n")
    'kite-css-property (list css-rule property-index))))

(defun kite-dom--render-css-rule (css-rule)
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
  (widget-insert "}\n\n"))

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
      (special-mode)
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
        (use-local-map widget-keymap)
        (widget-setup)))))

(defun kite-dom-show-matched-css ()
  (interactive)

  (kite-send
   "CSS.getMatchedStylesForNode"
   (list (cons 'nodeId
               (get-char-property (point) 'kite-node-id)))
   (lambda (response)
     (kite--log (pp-to-string response))
     (kite--dom-create-css-buffer
      (plist-get (plist-get response :result)
                 :matchedCSSRules)))))

(defun kite--dom-make-style-to-rule-map (matched-styles-response)

  (let ((style-to-rule-map (make-hash-table)))
    (let* ((matched-css-rules (plist-get (plist-get matched-styles-response :result) :matchedCSSRules))
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
    (message "style-to-rule-map is %s" style-to-rule-map)
    style-to-rule-map))

(defun kite--dom-insert-computed-style-value (value)
  (let ((color (kite-parse-color value)))
    (if color
        (let ((inhibit-read-only t))
          (insert-image (kite--make-color-image (kite--rgba-value color)))
          (insert " ")
          (insert value))
      (insert value))))

(defun kite--dom-render-computed-css (computed-styles-response matched-styles-response)
  (message "kite--dom-render-computed-css have matched styles %s" matched-styles-response)

  (let* ((style-to-rule-map (kite--dom-make-style-to-rule-map matched-styles-response))
         (arr (plist-get (plist-get computed-styles-response :result) :computedStyle))
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
               (list (cons 'nodeId node-id))
               (lambda (response)
                 (setcar barrier response)
                 (when (and (not (null (car barrier)))
                            (not (null (cdr barrier))))
                   (kite--dom-render-computed-css
                    (car barrier) (cdr barrier)))))
    (kite-send "CSS.getMatchedStylesForNode"
               (list (cons 'nodeId node-id))
               (lambda (response)
                 (setcdr barrier response)
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

(provide 'kite-dom-css)

;;; kite-dom-css.el ends here
