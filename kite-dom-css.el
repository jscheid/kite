
(defun kite-dom--render-property (property indent)
  (insert (make-string (* 2 indent) 32))
  (insert (plist-get property :name))
  (insert ": ")
  (insert (plist-get property :value))
  (insert ";\n"))

(defun kite-dom--render-css-rule (css-rule)
  (insert (format "%s {\n" (plist-get css-rule :selectorText)))
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
          (kite-dom--render-property shorthand-property 1)
          (remhash shorthand-name shorthand-entries))
        (kite-dom--render-property property (if shorthand-name 2 1)))
      (setq property-index (1+ property-index))))
  (insert "}\n\n"))

(defun kite--dom-create-css-buffer (matched-css-rules)
  (kite--log
   (concat "kite--dom-create-css-buffer called with rules:\n"
           (pp-to-string matched-css-rules)))
  (let* ((node-region-buffer (get-buffer-create "*kite css*"))
         (max-width 0)
         (window (display-buffer
                  node-region-buffer
                  '((display-buffer-pop-up-window . nil)))))
    (with-current-buffer node-region-buffer
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (save-excursion
          (insert "element.style {\n}\n\n")
          (let ((rule-index (- (length matched-css-rules) 1)))
            (while (>= rule-index 0)
              (kite-dom--render-css-rule
               (elt matched-css-rules rule-index))
              (setq rule-index (- rule-index 1)))))))))

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

(provide 'kite-dom-css)
