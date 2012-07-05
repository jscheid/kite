(eval-when-compile (require 'cl))

(defstruct (node-region)
  line-begin
  line-end
  outer-begin
  outer-end
  inner-begin
  inner-end
  indent
  attributes)

(defstruct (attr-region)
  outer-begin
  outer-end
  value-begin
  value-end)

(defun --kite-dimmed-face-foreground (face w)
  (flet ((lerp (a b w)
               (+ (* a w)
                  (* b (- 1 w)))))
    (let ((fg (color-name-to-rgb (face-foreground face nil t)))
          (bg (color-name-to-rgb (or (face-background face nil t)
                                     (cdr (assq 'background-color (frame-parameters)))))))
      (color-rgb-to-hex
       (lerp (nth 0 bg) (nth 0 fg) w)
       (lerp (nth 1 bg) (nth 1 fg) w)
       (lerp (nth 2 bg) (nth 2 fg) w)))))

(defface kite-css-selector '((t :inherit font-lock-function-name-face))
  "Face to use for selectors."
  :group 'kite)
(defface kite-css-property '((t :inherit font-lock-variable-name-face))
  "Face to use for properties."
  :group 'kite)
(defface kite-css-computed-unused-property
  `((t :inherit kite-css-property-computed-unused
       :foreground ,(--kite-dimmed-face-foreground 'kite-css-property 0.5)))
  "Face to use for computed properties that are unused."
  :group 'kite)

(defface kite-css-proprietary-property '((t :inherit (css-property italic)))
  "Face to use for vendor-specific properties."
  :group 'kite)

(defface kite-css-computed-proprietary-unused-property
  `((t :inherit kite-css-proprietary-property
       :foreground ,(--kite-dimmed-face-foreground 'kite-css-property 0.5)))
  "Face to use for computed vendore-specific properties that are unused."
  :group 'kite)

;; The following are the colors we use with a light background.
;; The two blues have the same hue but contrasting saturation/value.
;; The hue of the green is 120 degrees different from that of the
;; blue.  The red used for highlighting errors is 120 degrees
;; different again.  We use the light blue only for refs and
;; delimiters, since these are short (long stretches in a light color
;; would be too hard to read).  The dark blue is closest to black
;; (which we use by default for text), so we use it for attribute
;; values, which are similar to text.

(defconst kite-light-blue-color "#9292C9") ; hue 240
(defconst kite-dark-blue-color "#3A3A7B") ; hue 240
(defconst kite-green-color "#257A25") ; hue 120

;; Similar principles apply with a dark background.  However,
;; we switch green and blue, because darker blues are very hard to
;; read (for me anyway) on a dark background.

(defconst kite-sky-blue-color "#ACACFC") ; hue 240
(defconst kite-dark-green-color "#00AD00") ; hue 120
(defconst kite-light-green-color "#70F170") ; hue 120

(defface kite-delimited-data-face
  `((((class color) (background light)) (:foreground ,kite-dark-blue-color))
    (((class color) (background dark)) (:foreground ,kite-light-green-color)))
  "Face used to highlight data enclosed between delimiters.
By default, this is inherited by `kite-attribute-value-face'
and `kite-processing-instruction-content-face'."
  :group 'kite-highlighting-faces)

(defface kite-name-face
  `((((class color) (background light)) (:foreground ,kite-green-color))
    (((class color) (background dark)) (:foreground ,kite-sky-blue-color)))
  "Face used to highlight various names.
This includes element and attribute names, processing
instruction targets and the CDATA keyword in a CDATA section.
This is not used directly, but only via inheritance by other faces."
  :group 'kite-highlighting-faces)

(defface kite-ref-face
  `((((class color) (background light)) (:foreground ,kite-light-blue-color))
    (((class color) (background dark)) (:foreground ,kite-dark-green-color)))
  "Face used to highlight character and entity references.
This is not used directly, but only via inheritance by other faces."
  :group 'kite-highlighting-faces)

(defface kite-delimiter-face
  `((((class color) (background light)) (:foreground ,kite-light-blue-color))
    (((class color) (background dark)) (:foreground ,kite-dark-green-color))
    (t (:bold t)))
  "Face used to highlight delimiters.
This is not used directly, but only via inheritance by other faces."
  :group 'kite-highlighting-faces)

(defface kite-text-face
  nil
  "Face used to highlight text."
  :group 'kite-highlighting-faces)

(defface kite-comment-content-face
  '((t (:italic t)))
  "Face used to highlight the content of comments."
  :group 'kite-highlighting-faces)

(defface kite-comment-delimiter-face
  '((t (:inherit kite-delimiter-face)))
  "Face used for the delimiters of comments, i.e <!-- and -->."
  :group 'kite-highlighting-faces)

(defface kite-processing-instruction-delimiter-face
  '((t (:inherit kite-delimiter-face)))
  "Face used for the delimiters of processing instructions, i.e <? and ?>."
  :group 'kite-highlighting-faces)

(defface kite-processing-instruction-target-face
  '((t (:inherit kite-name-face)))
  "Face used for the target of processing instructions."
  :group 'kite-highlighting-faces)

(defface kite-processing-instruction-content-face
  '((t (:inherit kite-delimited-data-face)))
  "Face used for the content of processing instructions."
  :group 'kite-highlighting-faces)

(defface kite-cdata-section-delimiter-face
  '((t (:inherit kite-delimiter-face)))
  "Face used for the delimiters of CDATA sections, i.e <![, [, and ]]>."
  :group 'kite-highlighting-faces)

(defface kite-cdata-section-CDATA-face
  '((t (:inherit kite-name-face)))
  "Face used for the CDATA keyword in CDATA sections."
  :group 'kite-highlighting-faces)

(defface kite-cdata-section-content-face
  '((t (:inherit kite-text-face)))
  "Face used for the content of CDATA sections."
  :group 'kite-highlighting-faces)

(defface kite-char-ref-number-face
  '((t (:inherit kite-ref-face)))
  "Face used for the number in character references.
This includes ths `x' in hex references."
  :group 'kite-highlighting-faces)

(defface kite-char-ref-delimiter-face
  '((t (:inherit kite-ref-face)))
  "Face used for the delimiters of character references, i.e &# and ;."
  :group 'kite-highlighting-faces)

(defface kite-entity-ref-name-face
  '((t (:inherit kite-ref-face)))
  "Face used for the entity name in general entity references."
  :group 'kite-highlighting-faces)

(defface kite-entity-ref-delimiter-face
  '((t (:inherit kite-ref-face)))
  "Face used for the delimiters of entity references, i.e & and ;."
  :group 'kite-highlighting-faces)

(defface kite-tag-delimiter-face
  '((t (:inherit kite-delimiter-face)))
  "Face used for the angle brackets delimiting tags.
`kite-tag-slash-face' is used for slashes."
  :group 'kite-highlighting-faces)

(defface kite-tag-slash-face
  '((t (:inherit kite-name-face)))
  "Face used for slashes in tags, both in end-tags and empty-elements."
  :group 'kite-highlighting-faces)

(defface kite-element-prefix-face
  '((t (:inherit kite-name-face)))
  "Face used for the prefix of elements."
  :group 'kite-highlighting-faces)

(defface kite-element-colon-face
  '((t (:inherit kite-name-face)))
  "Face used for the colon in element names."
  :group 'kite-highlighting-faces)

(defface kite-element-local-name-face
  '((t (:inherit kite-name-face)))
  "Face used for the local name of elements."
  :group 'kite-highlighting-faces)

(defface kite-attribute-prefix-face
  '((t (:inherit kite-name-face)))
  "Face used for the prefix of attributes."
  :group 'kite-highlighting-faces)

(defface kite-attribute-colon-face
  '((t (:inherit kite-name-face)))
  "Face used for the colon in attribute names."
  :group 'kite-highlighting-faces)
  
(defface kite-attribute-local-name-face
  '((t (:inherit kite-name-face)))
  "Face used for the local name of attributes."
  :group 'kite-highlighting-faces)

(defface kite-namespace-attribute-xmlns-face
  '((t (:inherit kite-name-face)))
  "Face used for `xmlns' in namespace attributes."
  :group 'kite-highlighting-faces)

(defface kite-namespace-attribute-colon-face
  '((t (:inherit kite-name-face)))
  "Face used for the colon in namespace attributes."
  :group 'kite-highlighting-faces)

(defface kite-namespace-attribute-prefix-face
  '((t (:inherit kite-name-face)))
  "Face used for the prefix declared in namespace attributes."
  :group 'kite-highlighting-faces)

(defface kite-attribute-value-face
  '((t (:inherit kite-delimited-data-face)))
  "Face used for the value of attributes."
  :group 'kite-highlighting-faces)

(defface kite-attribute-value-delimiter-face
  '((t (:inherit kite-delimiter-face)))
  "Face used for the delimiters of attribute values."
  :group 'kite-highlighting-faces)

(defface kite-namespace-attribute-value-face
  '((t (:inherit kite-attribute-value-face)))
  "Face used for the value of namespace attributes."
  :group 'kite-highlighting-faces)

(defface kite-namespace-attribute-value-delimiter-face
  '((t (:inherit kite-attribute-value-delimiter-face)))
  "Face used for the delimiters of namespace attribute values."
  :group 'kite-highlighting-faces)

(defface kite-prolog-literal-delimiter-face
  '((t (:inherit kite-delimiter-face)))
  "Face used for the delimiters of literals in the prolog."
  :group 'kite-highlighting-faces)

(defface kite-prolog-literal-content-face
  '((t (:inherit kite-delimited-data-face)))
  "Face used for the content of literals in the prolog."
  :group 'kite-highlighting-faces)

(defface kite-prolog-keyword-face
  '((t (:inherit kite-name-face)))
  "Face used for keywords in the prolog."
  :group 'kite-highlighting-faces)

(defface kite-markup-declaration-delimiter-face
  '((t (:inherit kite-delimiter-face)))
  "Face used for the delimiters of markup declarations in the prolog.
The delimiters are <! and >."
  :group 'kite-highlighting-faces)

(defface kite-hash-face
  '((t (:inherit kite-name-face)))
  "Face used for # before a name in the prolog."
  :group 'kite-highlighting-faces)

(defface kite-glyph-face
  '((((type x))
     (:family
      "misc-fixed"
      :background
      "light grey"
      :foreground
      "black"
      :weight
      normal 
      :slant
      normal))
    (t
     (:background
      "light grey"
      :foreground
      "black"
      :weight
      normal 
      :slant
      normal)))
  "Face used for glyph for char references."
  :group 'kite-highlighting-faces)

(defvar kite-dom-mode-map nil
  "Local keymap for `kite-dom-mode' buffers.")

(setq kite-dom-mode-map
  (let ((map (make-sparse-keymap))
	(menu-map (make-sparse-keymap)))
    ;(define-key map [remap self-insert-command] 'kite-dom-no-edit)
    (suppress-keymap map t)
    (define-key map "p" 'kite-dom-pick-node)
    (define-key map "h" 'kite-dom-highlight-node)
    (define-key map "H" 'kite-dom-hide-highlight)
    (define-key map "c" 'kite-dom-show-matched-css)
    (define-key map "C" 'kite-dom-show-computed-css)
    map))

(defvar kite-dom-attr-value-keymap nil
  "Keymap used inside editable fields in customization buffers.")

(setq kite-dom-attr-value-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] 'kite-dom-insert-attr-value)
    (substitute-key-definition 'self-insert-command 'kite-dom-insert-attr-value map (current-global-map))
    (define-key map "\C-c\C-c" 'kite-dom-attr-value-set)
    (define-key map "\C-x\C-s" 'kite-dom-attr-value-save)

    (define-key map "\M-\C-u" 'kite-backward-up-element)
    (define-key map "\M-\C-d" 'kite-down-element)
    (define-key map "\M-\C-n" 'kite-forward-element)
    (define-key map "\M-\C-p" 'kite-backward-element)
    (define-key map "\M-{" 'kite-backward-paragraph)
    (define-key map "\M-}" 'kite-forward-paragraph)
    (define-key map "\M-h" 'kite-mark-paragraph)

    map))

(define-derived-mode kite-dom-mode special-mode "kite-dom"
  "Toggle kite dom mode."
  (set (make-local-variable 'kill-buffer-hook) '--kite-kill-dom)
  (setq buffer-read-only nil)
  (set (make-local-variable 'kite-dom-nodes) (make-hash-table)))

(defconst kite-dom-offset 2)

(defun --kite-dom-attr-value-left (old-point new-point)
  (when (not (eq (get-text-property new-point 'point-left)
                 '--kite-dom-attr-value-left))
    (message "--kite-dom-attr-value-left %s %s" old-point new-point)))

(defun --kite-insert-attribute (node-id attr-name attr-value)
  (let ((attr-begin (point-marker))
        (attr-region (make-attr-region)))

    (setf (attr-region-outer-begin attr-region) (point-marker))
    (insert (concat " "
                    (propertize attr-name
                                'read-only t
                                'face 'kite-attribute-local-name-face)
                    "="))
    (setf (attr-region-value-begin attr-region) (point-marker))
    (insert (propertize "\""
                        'kite-node-id node-id
                        'read-only t
                        'face 'kite-attribute-value-delimiter-face
                        'rear-nonsticky '(read-only point-left)))
    (insert (propertize attr-value
                        'kite-node-id node-id
                        'face 'kite-attribute-value-face
                        'point-left '--kite-dom-attr-value-left
                        'keymap kite-dom-attr-value-keymap
                        'field 'kite-dom-attribute))
    (insert (propertize "\""
                        'kite-node-id node-id
                        'read-only t
                        'keymap kite-dom-attr-value-keymap
                        'face 'kite-attribute-value-delimiter-face
                        'front-sticky '(keymap)
                        'rear-nonsticky '(keymap)))
    (setf (attr-region-value-end attr-region) (point-marker))
    (setf (attr-region-outer-end attr-region) (point-marker))

    (cons (intern attr-name) attr-region)))

(defun --kite-dom-insert-element (element indent loadp)
  (flet ((indent-prefix (indent)
                        (propertize
                         (make-string (* kite-dom-offset indent) 32)
                         'read-only t))
         (render-attributes (element)
                            (let* (attribute-info-list
                                   (node-id (cdr (assq 'nodeId element)))
                                   (attributes (cdr (assq 'attributes element)))
                                   (attr-index 0)
                                   (num-attrs (length attributes))
                                   (inhibit-read-only t))
                              (while (< attr-index num-attrs)
                                (setq attribute-info-list
                                      (cons
                                       (--kite-insert-attribute node-id
                                                                (elt attributes attr-index)
                                                                (elt attributes (1+ attr-index)))
                                       attribute-info-list))
                                (setq attr-index (+ 2 attr-index)))
                              attribute-info-list)))

    (let ((nodeType (cdr (assq 'nodeType element)))
          (node-id (cdr (assq 'nodeId element)))
          (localName (cdr (assq 'localName element)))
          (inhibit-read-only t)
          (node-region (make-node-region))
          attributes)

      (setf (node-region-line-begin node-region) (point-marker))

      (cond

       ((and (eq nodeType 1)
             (or (eq 0 (cdr (assq 'childNodeCount element)))
                 (assq 'children element)))
        (insert (concat (indent-prefix indent)
                        (propertize "<"
                                    'kite-node-id node-id
                                    'read-only t
                                    'face 'kite-tag-delimiter-face)
                        (propertize localName
                                    'kite-node-id node-id
                                    'face 'kite-element-local-name-face)))
        (setq attributes (render-attributes element))
        (insert (concat (propertize ">"
                                    'kite-node-id node-id
                                    'read-only t
                                    'face 'kite-tag-delimiter-face)
                        "\n"))
        (setf (node-region-inner-begin node-region) (point-marker))

        (mapcar (lambda (child) (--kite-dom-insert-element child (1+ indent) loadp))
                (cdr (assq 'children element)))
        (setf (node-region-inner-end node-region) (point-marker))

        (insert (concat (indent-prefix indent)
                        (propertize "<"
                                    'kite-node-id node-id
                                    'read-only t
                                    'face 'kite-tag-delimiter-face)
                        (propertize "/"
                                    'kite-node-id node-id
                                    'read-only t
                                    'face 'kite-tag-slash-face)
                        (propertize localName
                                    'kite-node-id node-id
                                    'face 'kite-element-local-name-face)
                        (propertize ">"
                                    'kite-node-id node-id
                                    'read-only t
                                    'face 'kite-tag-delimiter-face)
                        "\n")))

       ((eq nodeType 1)
        (insert (concat (indent-prefix indent)
                        (propertize "<"
                                    'kite-node-id node-id
                                    'read-only t
                                    'face 'kite-tag-delimiter-face)
                        (propertize localName
                                    'kite-node-id node-id
                                    'face 'kite-element-local-name-face)))
        (setq attributes (render-attributes element))
        (insert (propertize ">"
                            'face 'kite-tag-delimiter-face))
        (setf (node-region-inner-begin node-region) (point-marker))
        (insert "...")
        (setf (node-region-inner-end node-region) (point-marker))
        (insert (concat (propertize "<"
                                    'kite-node-id node-id
                                    'read-only t
                                    'face 'kite-tag-delimiter-face)
                        (propertize "/"
                                    'kite-node-id node-id
                                    'read-only t
                                    'face 'kite-tag-slash-face)
                        (propertize localName
                                    'kite-node-id node-id
                                    'face 'kite-element-local-name-face)
                        (propertize ">"
                                    'kite-node-id node-id
                                    'read-only t
                                    'face 'kite-tag-delimiter-face)
                        "\n"))
        (when loadp
          (--kite-send "DOM.requestChildNodes" (list (cons 'nodeId (cdr (assq 'nodeId element))))
                       (lambda (response) nil))))

       ((eq nodeType 3)
        (insert (concat (indent-prefix indent)
                        (propertize (replace-regexp-in-string "\\(^\\(\\s \\|\n\\)+\\|\\(\\s \\|\n\\)+$\\)" ""
                                                              (cdr (assq 'nodeValue element)))
                                    'kite-node-id node-id
                                    'face 'kite-text-face)
                        "\n"))))
      (setf (node-region-line-end node-region) (point-marker))
      (setf (node-region-indent node-region) indent)
      (setf (node-region-attributes node-region) attributes)
      (puthash (cdr (assq 'nodeId element)) node-region kite-dom-nodes))))

(defun --kite-kill-dom ()
  (ignore-errors
    (with-current-buffer kite-connection
      (--kite-send "CSS.disable" nil
                   (lambda (response) (message "CSS disabled."))))))

(defun --kite-websocket-url ()
  (cdr (assq 'webSocketDebuggerUrl kite-tab-alist))  )

(defun kite-dom-inspect ()
  (interactive)
  (--kite-log "opening dom")
  (lexical-let*
      ((kite-connection (current-buffer))
       (buf (get-buffer-create
             (--kite-dom-buffer (--kite-websocket-url)))))
    (with-current-buffer buf
      (kite-dom-mode)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (set (make-local-variable 'kite-connection) kite-connection))
    (switch-to-buffer buf)
    (--kite-send "CSS.enable" nil
                 (lambda (response)
                   (message "CSS.enable got response %s" response)
                   (--kite-send "CSS.getAllStyleSheets" nil
                                (lambda (response)
                                  (message "CSS.getAllStyleSheets got response %s" response)))))
    (--kite-send "DOM.getDocument" nil
                 (lambda (response)
                   (--kite-log "DOM.getDocument got response %s" response)
                   (with-current-buffer buf
                     (save-excursion
                       (--kite-dom-insert-element (elt (cdr (assq 'children (assq 'root (assq 'result response)))) 0)
                                                  0 t)))))))

(defun --kite-DOM-documentUpdated (websocket-url packet)
  t)

(defun --kite-dom-buffer (websocket-url)
  (format "*kite dom %s*" websocket-url))

(defun --kite-DOM-setChildNodes (websocket-url packet)
  (--kite-log "--kite-DOM-setChildNodes got packet %s" packet)
  (with-current-buffer (--kite-dom-buffer websocket-url)
    (save-excursion
      (let ((inhibit-read-only t)
            (node-info
             (gethash (cdr (assq 'parentId packet)) kite-dom-nodes)))
        (delete-region (node-region-inner-begin node-info)
                       (node-region-inner-end node-info))
        (goto-char (node-region-inner-begin node-info))
        (atomic-change-group
          (insert "\n")
          (mapcar (lambda (node)
                    (--kite-dom-insert-element node (1+ (node-region-indent node-info)) t))
                  (cdr (assq 'nodes packet)))
          (insert (make-string (* kite-dom-offset (node-region-indent node-info)) 32)))))))

(defun --kite-DOM-childNodeInserted (websocket-url packet)
  (--kite-log "--kite-DOM-childNodeInserted got packet %s" packet)
  (with-current-buffer (--kite-dom-buffer websocket-url)
    (save-excursion
      (let ((inhibit-read-only t)
            (previous-node-id (cdr (assq 'previousNodeId packet)))
            (parent-node-id (cdr (assq 'parentNodeId packet))))
        (if (eq previous-node-id 0)
            (let ((node-info (gethash parent-node-id kite-dom-nodes)))
              (goto-char (node-region-inner-begin node-info))
              (--kite-dom-insert-element (cdr (assq 'node packet))
                                         (1+ (node-region-indent node-info))
                                         t))
          (let ((node-info (gethash previous-node-id kite-dom-nodes)))
            (goto-char (node-region-line-end node-info))
            (--kite-dom-insert-element (cdr (assq 'node packet))
                                       (node-region-indent node-info)
                                       t)))))))

(defun --kite-DOM-childNodeCountUpdated (websocket-url packet)
  (--kite-log "--kite-DOM-childNodeCountUpdated got packet %s" packet))

(defun --kite-DOM-childNodeRemoved (websocket-url packet)
  (--kite-log "--kite-DOM-childNodeRemoved got packet %s" packet)
  (with-current-buffer (--kite-dom-buffer websocket-url)
    (save-excursion
      (let ((inhibit-read-only t)
            (node-info (gethash (cdr (assq 'nodeId packet)) kite-dom-nodes)))
        (delete-region
         (node-region-line-begin node-info)
         (node-region-line-end node-info))))))

(defun --kite-DOM-attributeModified (websocket-url packet)
  (--kite-log "--kite-DOM-attributeModified got packet %s" packet)
  (with-current-buffer (--kite-dom-buffer websocket-url)
    (save-excursion
      (let* ((inhibit-read-only t)
             (node-id (cdr (assq 'nodeId packet)))
             (attr-name (intern (cdr (assq 'name packet))))
             (node-info (gethash node-id kite-dom-nodes))
             (attr-info (cdr (assq attr-name (node-region-attributes node-info)))))
        (if attr-info
            ;; Modify existing attribute
            (progn
              (goto-char (1+ (attr-region-value-begin attr-info)))
              (delete-region (1+ (attr-region-value-begin attr-info))
                             (- (attr-region-value-end attr-info) 1))
              (insert (propertize (cdr (assq 'value packet))
                                  'keymap kite-dom-attr-value-keymap
                                  'field 'kite-dom-attribute
                                  'point-left '--kite-dom-attr-value-left
                                  'face 'kite-attribute-value-face)))
          ;; Insert new attribute
          (goto-char (attr-region-value-end (cdar (node-region-attributes node-info))))
          (setf (node-region-attributes node-info)
                (cons (--kite-insert-attribute node-id (cdr (assq 'name packet)) (cdr (assq 'value packet)))
                      (node-region-attributes node-info))))))))

(defun --kite-DOM-attributeRemoved (websocket-url packet)
  (--kite-log "--kite-DOM-attributeRemoved got packet %s" packet)
  (with-current-buffer (--kite-dom-buffer websocket-url)
    (save-excursion
      (let* ((inhibit-read-only t)
             (attr-name (intern (cdr (assq 'name packet))))
             (node-info (gethash (cdr (assq 'nodeId packet)) kite-dom-nodes))
             (attr-info (cdr (assq attr-name (node-region-attributes node-info)))))
        (delete-region (attr-region-outer-begin attr-info)
                       (attr-region-outer-end attr-info))
        (setf (node-region-attributes node-info)
              (assq-delete-all attr-name (node-region-attributes node-info)))))))

(defun kite-dom-insert-attr-value (arg)
  (interactive "p")
  (let ((begin (point)))
    (self-insert-command arg)
    (add-text-properties begin (point)
                         (list 'face 'kite-attribute-value-face
                               'keymap kite-dom-attr-value-keymap
                               'field 'kite-dom-attribute))))


(defun kite-backward-up-element (&optional arg)
  (interactive "p")
  t)

(defun kite-down-element (&optional arg)
  (interactive "p")
  t)

(defun kite-forward-element (&optional arg)
  (interactive "p")
  t)

(defun kite-backward-element (&optional arg)
  (interactive "p")
  t)

(defun kite-backward-paragraph (&optional arg)
  (interactive "p")
  t)

(defun kite-forward-paragraph (&optional arg)
  (interactive "p")
  t)

(defun kite-mark-paragraph (&optional arg)
  (interactive "p")
  t)

(defun --kite-rgba (r g b a)
  `((r . ,r)
    (g . ,g)
    (b . ,b)
    (a . ,a)))

(defun kite-dom-highlight-node ()
  (interactive)

  (--kite-send "DOM.highlightNode"
               (list (cons 'nodeId
                           (get-char-property (point) 'kite-node-id))
                     (cons 'highlightConfig
                           `((showInfo . nil)
                             (contentColor . ,(--kite-rgba 255 0 0 0.5))
                             (paddingColor . ,(--kite-rgba 0 255 0 0.5))
                             (borderColor . ,(--kite-rgba 0 0 255 0.5))
                             (marginColor . ,(--kite-rgba 255 255 0 0.5)))))
               (lambda (response)
                 (--kite-log "DOM.highlightNode got response %s" response))))


(defun kite-dom-hide-highlight ()
  (interactive)
  (--kite-send "DOM.hideHighlight"))

(defun kite-dom-show-matched-css ()
  (interactive)

  (--kite-send "CSS.getMatchedStylesForNode"
               (list (cons 'nodeId
                           (get-char-property (point) 'kite-node-id)))
               (lambda (response)
                 (message "CSS.getMatchedStylesForNode got response %s" response))))

(defun --kite-dom-make-style-to-rule-map (matched-styles-response)

  (let ((style-to-rule-map (make-hash-table)))
    (let* ((matched-css-rules (cdr (assq 'matchedCSSRules (cdr (assq 'result matched-styles-response)))))
           (num-matched-rules (length matched-css-rules))
           (matched-rule-index 0))
      (while (< matched-rule-index num-matched-rules)
        (let* ((matched-rule (elt matched-css-rules matched-rule-index))
               (rule-info (list (cdr (assq 'selectorText matched-rule))
                                (cdr (assq 'origin matched-rule))
                                (cdr (assq 'sourceLine matched-rule))))
               (style-arr (cdr (assq 'cssProperties (cdr (assq 'style matched-rule)))))
               (num-styles (length style-arr))
               (style-index 0))
          (while (< style-index num-styles)
            (let ((style-name (cdr (assq 'name (elt style-arr style-index))))
                  (style-value (cdr (assq 'value (elt style-arr style-index)))))
              (puthash (intern style-name)
                       (cons (cons rule-info
                                   style-value)
                             (gethash (intern style-name)
                                      style-to-rule-map)) style-to-rule-map))
            (setq style-index (1+ style-index))))
        (setq matched-rule-index (1+ matched-rule-index))))
    (message "style-to-rule-map is %s" style-to-rule-map)
    style-to-rule-map))

(defun --kite-dom-render-computed-css (computed-styles-response matched-styles-response)
  (message "--kite-dom-render-computed-css have matched styles %s" matched-styles-response)

  (let* ((style-to-rule-map (--kite-dom-make-style-to-rule-map matched-styles-response))
         (arr (cdr (assq 'computedStyle (cdr (assq 'result computed-styles-response)))))
         (index 0)
         (arr-len (length arr))
         as-list)
    (while (< index arr-len)
      (let ((item (elt arr index)))
        (setq as-list (cons (cons (cdr (assq 'name item))
                                  (cdr (assq 'value item)))
                            as-list)))
      (setq index (1+ index)))
    (let ((sorted-styles
           (sort as-list
                 (lambda (x y) (string< (car x) (car y))))))

      (let* ((node-info-buffer (get-buffer-create "*kite dom node details*"))
             (max-width 0)
             (window (display-buffer node-info-buffer (list (cons 'display-buffer-pop-up-window nil)))))
        (with-current-buffer node-info-buffer
          (special-mode)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (save-excursion
              (while sorted-styles
                (let* ((style (car sorted-styles))
                       (matched-styles (gethash (intern (car style)) style-to-rule-map))
                       (line (concat (propertize (car style)
                                                 'face
                                                 (if (null (string-match "^-" (car style)))
                                                     (if matched-styles
                                                         'kite-css-property
                                                       'kite-css-computed-unused-property)
                                                   (if matched-styles
                                                       'kite-css-proprietary-property
                                                     'kite-css-computed-proprietary-unused-property)))
                                     ": "
                                     (cdr style))))
                  (setq max-width (max max-width (length line)))
                  (insert (concat line "\n"))
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
    (--kite-send "CSS.getComputedStyleForNode"
                 (list (cons 'nodeId node-id))
                 (lambda (response)
                   (setcar barrier response)
                   (when (and (not (null (car barrier)))
                              (not (null (cdr barrier))))
                     (--kite-dom-render-computed-css
                      (car barrier) (cdr barrier)))))
    (--kite-send "CSS.getMatchedStylesForNode"
                 (list (cons 'nodeId node-id))
                 (lambda (response)
                   (setcdr barrier response)
                   (when (and (not (null (car barrier)))
                              (not (null (cdr barrier))))
                     (--kite-dom-render-computed-css
                      (car barrier) (cdr barrier)))))))

(defconst --kite-dom-pick-node-message
  "Now switch to your browser and select a DOM node")

(defun kite-dom-pick-node ()
  (interactive)
  (--kite-send "DOM.setInspectModeEnabled"
               `((enabled . t)
                 (highlightConfig
                  . ((showInfo . nil)
                     (contentColor . ,(--kite-rgba 255 0 0 0.5))
                     (paddingColor . ,(--kite-rgba 0 255 0 0.5))
                     (borderColor . ,(--kite-rgba 0 0 255 0.5))
                     (marginColor . ,(--kite-rgba 255 255 0 0.5)))))
               (lambda (response)
                 (message --kite-dom-pick-node-message))))


(defun kite-dom-goto-node (node-id)
  (interactive)
  (let ((node-info (gethash node-id kite-dom-nodes)))
    (goto-char (node-region-line-begin node-info))
    (when (string= (current-message)
                   --kite-dom-pick-node-message)
      (message nil))))

(provide 'kite-dom)
