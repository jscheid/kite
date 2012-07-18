(require 'kite-color)
(require 'widget)

(eval-when-compile
  (require 'cl)
  (require 'wid-edit))

(defstruct (node-region)
  line-begin
  line-end
  outer-begin
  outer-end
  inner-begin
  inner-end
  indent
  attribute-regions)

(defstruct (attr-region)
  outer-begin
  outer-end
  value-begin
  value-end)

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
      (color-rgb-to-hex
       (lerp (nth 0 bg) (nth 0 fg) darkness)
       (lerp (nth 1 bg) (nth 1 fg) darkness)
       (lerp (nth 2 bg) (nth 2 fg) darkness)))))


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

;; Try loading nxml-mode so we can steal their faces
(require 'nxml-mode nil t)

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
  (if (facep 'nxml-delimited-data-face)
      '((t (:inherit nxml-delimited-data-face)))
    `((((class color) (background light)) (:foreground ,kite-dark-blue-color))
      (((class color) (background dark)) (:foreground ,kite-light-green-color))))
  "Face used to highlight data enclosed between delimiters.
By default, this is inherited by `kite-attribute-value-face'
and `kite-processing-instruction-content-face'."
  :group 'kite-highlighting-faces)

(defface kite-name-face
  (if (facep 'nxml-name-face)
      '((t (:inherit nxml-name-face)))
    `((((class color) (background light)) (:foreground ,kite-green-color))
      (((class color) (background dark)) (:foreground ,kite-sky-blue-color))))
  "Face used to highlight various names.
This includes element and attribute names, processing
instruction targets and the CDATA keyword in a CDATA section.
This is not used directly, but only via inheritance by other faces."
  :group 'kite-highlighting-faces)

(defface kite-ref-face
  (if (facep 'nxml-ref-face)
      '((t (:inherit nxml-ref-face)))
    `((((class color) (background light)) (:foreground ,kite-light-blue-color))
      (((class color) (background dark)) (:foreground ,kite-dark-green-color))))
  "Face used to highlight character and entity references.
This is not used directly, but only via inheritance by other faces."
  :group 'kite-highlighting-faces)

(defface kite-delimiter-face
  (if (facep 'nxml-delimiter-face)
      '((t (:inherit nxml-delimiter-face)))
    `((((class color) (background light)) (:foreground ,kite-light-blue-color))
      (((class color) (background dark)) (:foreground ,kite-dark-green-color))
      (t (:bold t))))
  "Face used to highlight delimiters.
This is not used directly, but only via inheritance by other faces."
  :group 'kite-highlighting-faces)

(defface kite-text-face
  (if (facep 'nxml-text-face)
      '((t (:inherit nxml-text-face)))
    nil)
  "Face used to highlight text."
  :group 'kite-highlighting-faces)

(defface kite-comment-content-face
  (if (facep 'nxml-comment-content-face)
      '((t (:inherit nxml-comment-content-face)))
    '((t (:italic t))))
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

(defface kite-modified-element-local-name-face
  '((t :inherit kite-element-local-name-face :background "#555"))
  "Face used for the local name of attributes."
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

(defface kite-modified-attribute-local-name-face
  '((t :inherit kite-attribute-local-name-face :background "#555"))
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

(defface kite-modified-attribute-value-face
  '((t :inherit kite-attribute-value-face :background "#555"))
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
        ;;(define-key map [remap self-insert-command] 'kite-dom-no-edit)
        (suppress-keymap map t)
        (kite--define-global-mode-keys map)
        (define-key map (kbd "RET") 'undefined)
        (define-key map (kbd "DEL") 'kite-dom-delete-node-or-attribute)
        (define-key map "p" 'kite-dom-pick-node)
        (define-key map "h" 'kite-dom-highlight-node)
        (define-key map "H" 'kite-dom-hide-highlight)
        (define-key map "c" 'kite-dom-show-matched-css)
        (define-key map "C" 'kite-dom-show-computed-css)
        (define-key map "\C-xnd" 'kite-dom-narrow-to-node)
        map))

(defvar kite-dom-attr-value-keymap nil
  "Keymap used inside editable fields in customization buffers.")

(setq kite-dom-attr-value-keymap
      (let ((map (make-sparse-keymap)))
        (define-key map [remap self-insert-command] 'kite-dom-insert-attr-value)
        (substitute-key-definition 'self-insert-command 'kite-dom-insert-attr-value map (current-global-map))
        (define-key map (kbd "DEL") 'backward-delete-char-untabify)
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
  (set (make-local-variable 'kill-buffer-hook) 'kite--kill-dom)
  (setq buffer-read-only nil)
  (set (make-local-variable 'kite-dom-nodes) (make-hash-table)))

(defconst kite-dom-offset 2)

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

(defun kite--dom-attr-value-left (old-point new-point)
  (when (not (eq (get-text-property new-point 'point-left)
                 'kite--dom-attr-value-left))
    (message "kite--dom-attr-value-left %s %s" old-point new-point)))

(defun kite--notify-widget (widget &rest ignore)
  (let ((modified-face (widget-get widget :modified-value-face)))
    (unless (eq (widget-get widget :value-face)
                modified-face)
      (widget-put widget :value-face modified-face)
      (overlay-put (widget-get widget :field-overlay)
                   'face modified-face))))

(defun kite--validate-widget (widget)
  (let ((val (widget-apply widget :value-get)))
    (message "validate, widget value %s" val)
    (unless (> (length val) 0)
      (widget-put widget :error "too short!")
      widget)))

(defun kite--insert-attribute (node-id attr-name attr-value)
  (let ((attr-begin (point-marker))
        (attr-region (make-attr-region)))

    (setf (attr-region-outer-begin attr-region) (point-marker))

    (widget-insert " ")

    (widget-create 'editable-field
                   :size 1
                   :value-face 'kite-attribute-local-name-face
                   :modified-value-face 'kite-modified-attribute-local-name-face
                   :notify (function kite--notify-widget)
                   :validate (function kite--validate-widget)
                   :match (lambda (x) (> (length (widget-value x)) 0))
                   attr-name)

    (widget-insert "=")

    (setf (attr-region-value-begin attr-region) (point-marker))
    (widget-insert (propertize "\""
                        'kite-node-id node-id
                        'face 'kite-attribute-value-delimiter-face))
    (widget-create 'editable-field
                   :size 0
                   :value-face 'kite-attribute-value-face
                   :modified-value-face 'kite-modified-attribute-value-face
                   :notify (function kite--notify-widget)
                   attr-value)
    (widget-insert (propertize "\""
                        'kite-node-id node-id
                        'face 'kite-attribute-value-delimiter-face))
    (setf (attr-region-value-end attr-region) (point-marker))
    (setf (attr-region-outer-end attr-region) (point-marker))

    (cons (intern attr-name) attr-region)))

(defun kite--dom-insert-element (element indent loadp)
  (flet ((indent-prefix (indent node-id)
                        (propertize
                         (make-string (* kite-dom-offset indent) 32)
                         'kite-node-id node-id
                         'read-only t))
         (render-attribute-regions (element)
                                   (let* (attribute-info-list
                                          (node-id (plist-get element :nodeId))
                                          (attributes (plist-get element :attributes))
                                          (attr-index 0)
                                          (num-attrs (length attributes))
                                          (inhibit-read-only t))
                                     (while (< attr-index num-attrs)
                                       (setq attribute-info-list
                                             (cons
                                              (kite--insert-attribute node-id
                                                                       (elt attributes attr-index)
                                                                       (elt attributes (1+ attr-index)))
                                              attribute-info-list))
                                       (setq attr-index (+ 2 attr-index)))
                                     attribute-info-list)))

    (let ((nodeType (plist-get element :nodeType))
          (node-id (plist-get element :nodeId))
          (localName (plist-get element :localName))
          (inhibit-read-only t)
          (node-region (make-node-region))
          attributes)

      (setf (node-region-line-begin node-region) (point-marker))

      (cond

       ((and (eq nodeType 1)
             (or (eq 0 (plist-get element :childNodeCount))
                 (plist-member element :children)))
        (widget-insert (concat (indent-prefix indent node-id)
                        (propertize "<"
                                    'kite-node-id node-id
                                    'face 'kite-tag-delimiter-face)))

        (widget-create 'editable-field
                       :size 1
                       :value-face 'kite-element-local-name-face
                       :modified-value-face 'kite-modified-element-local-name-face
                       :notify (function kite--notify-widget)
                       :validate (function kite--validate-widget)
                       :match (lambda (x) (> (length (widget-value x)) 0))
                       localName)

        (setq attributes (render-attribute-regions element))
        (widget-insert (concat (propertize ">"
                                    'kite-node-id node-id
                                    'read-only t
                                    'face 'kite-tag-delimiter-face)
                        "\n"))
        (setf (node-region-inner-begin node-region) (point-marker))

        (put-text-property (node-region-line-begin node-region)
                           (node-region-inner-begin node-region)
                           'kite-node-id
                           node-id)

        (mapcar (lambda (child) (kite--dom-insert-element child (1+ indent) loadp))
                (plist-get element :children))
        (setf (node-region-inner-end node-region) (point-marker))

        (widget-insert (concat (indent-prefix indent node-id)
                        (propertize "<"
                                    'face 'kite-tag-delimiter-face)
                        (propertize "/"
                                    'face 'kite-tag-slash-face)
                        (propertize localName
                                    'face 'kite-element-local-name-face)
                        (propertize ">\n"
                                    'face 'kite-tag-delimiter-face)))

        (put-text-property (node-region-inner-end node-region)
                           (point)
                           'kite-node-id
                           node-id))


       ((eq nodeType 1)
        (widget-insert (concat (indent-prefix indent node-id)
                        (propertize "<"
                                    'face 'kite-tag-delimiter-face)))

        (widget-create 'editable-field
                       :size 1
                       :value-face 'kite-element-local-name-face
                       :modified-value-face 'kite-modified-element-local-name-face
                       :notify (function kite--notify-widget)
                       :validate (function kite--validate-widget)
                       :match (lambda (x) (> (length (widget-value x)) 0))
                       localName)
        (setq attributes (render-attribute-regions element))
        (widget-insert (propertize ">"
                            'face 'kite-tag-delimiter-face))
        (setf (node-region-inner-begin node-region) (point-marker))
        (widget-insert "...")
        (setf (node-region-inner-end node-region) (point-marker))
        (widget-insert (concat (propertize "<"
                                    'face 'kite-tag-delimiter-face)
                        (propertize "/"
                                    'face 'kite-tag-slash-face)
                        (propertize localName
                                    'face 'kite-element-local-name-face)
                        (propertize ">"
                                    'face 'kite-tag-delimiter-face)
                        "\n"))

        (put-text-property (node-region-line-begin node-region)
                           (point)
                           'kite-node-id
                           node-id)

        (when loadp
          (kite-send "DOM.requestChildNodes" (list (cons 'nodeId (plist-get element :nodeId)))
                     (lambda (response) nil))))

       ((eq nodeType 3)
        (widget-insert (concat (indent-prefix indent node-id)
                        (propertize (replace-regexp-in-string "\\(^\\(\\s \\|\n\\)+\\|\\(\\s \\|\n\\)+$\\)" ""
                                                              (plist-get element :nodeValue))
                                    'face 'kite-text-face)
                        "\n"))

        (put-text-property (node-region-line-begin node-region)
                           (point)
                           'kite-node-id
                           node-id)))

      (setf (node-region-line-end node-region) (point-marker))
      (setf (node-region-indent node-region) indent)
      (setf (node-region-attribute-regions node-region) attributes)
      (puthash (plist-get element :nodeId) node-region kite-dom-nodes))))

(defun kite--kill-dom ()
  t)
;  (ignore-errors
;    (kite-send "CSS.disable" nil
;               (lambda (response) (message "CSS disabled.")))))

(defun kite--websocket-url ()
  (websocket-url (kite-session-websocket kite-session)))

(defun kite-dom-inspect ()
  (interactive)
  (kite--log "opening dom")
  (lexical-let*
      ((kite-session kite-session)
       (buf (get-buffer-create
             (kite--dom-buffer (kite--websocket-url)))))
    (with-current-buffer buf
      (kite-dom-mode)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (remove-overlays)
      (use-local-map widget-keymap)
      (widget-setup)
      (set (make-local-variable 'kite-session) kite-session))
    (switch-to-buffer buf)
    (kite-send "CSS.enable" nil
               (lambda (response)
                 (message "CSS.enable got response %s" response)
                 (kite-send "CSS.getAllStyleSheets" nil
                            (lambda (response)
                              (message "CSS.getAllStyleSheets got response %s" response)))))
    (kite-send "DOM.getDocument" nil
               (lambda (response)
                 (kite--log "DOM.getDocument got response %s" response)
                 (with-current-buffer buf
                   (save-excursion
                     (kite--dom-insert-element (elt (plist-get (plist-get (plist-get response :result) :root) :children) 0)
                                                0 t)
                     (widget-setup)))))))

(defun kite--dom-buffer (websocket-url)
  (format "*kite dom %s*" websocket-url))

(defun kite--dom-DOM-setChildNodes (websocket-url packet)
  (kite--log "kite--DOM-setChildNodes got packet %s" packet)
  (with-current-buffer (kite--dom-buffer websocket-url)
    (save-excursion
      (let ((inhibit-read-only t)
            (node-region
             (gethash (plist-get packet :parentId) kite-dom-nodes)))
        (delete-region (node-region-inner-begin node-region)
                       (node-region-inner-end node-region))
        (goto-char (node-region-inner-begin node-region))
        (atomic-change-group
          (widget-insert (propertize "\n" 'kite-node-id (plist-get packet :parentId)))
          (mapcar (lambda (node)
                    (kite--dom-insert-element node (1+ (node-region-indent node-region)) t))
                  (plist-get packet :nodes))
          (widget-insert (propertize
                   (make-string (* kite-dom-offset (node-region-indent node-region)) 32)
                   'kite-node-id (plist-get packet :parentId)
                   'read-only t))))
      (widget-setup))))

(defun kite--dom-DOM-childNodeInserted (websocket-url packet)
  (kite--log "kite--DOM-childNodeInserted got packet %s" packet)
  (with-current-buffer (kite--dom-buffer websocket-url)
    (save-excursion
      (let ((inhibit-read-only t)
            (previous-node-id (plist-get packet :previousNodeId))
            (parent-node-id (plist-get packet :parentNodeId)))
        (if (eq previous-node-id 0)
            (let ((node-region (gethash parent-node-id kite-dom-nodes)))
              (goto-char (node-region-inner-begin node-region))
              (kite--dom-insert-element (plist-get packet :node)
                                         (1+ (node-region-indent node-region))
                                         t))
          (let ((node-region (gethash previous-node-id kite-dom-nodes)))
            (goto-char (node-region-line-end node-region))
            (kite--dom-insert-element (plist-get packet :node)
                                       (node-region-indent node-region)
                                       t))))
      (widget-setup))))

(defun kite--dom-DOM-childNodeCountUpdated (websocket-url packet)
  (kite--log "kite--DOM-childNodeCountUpdated got packet %s" packet))

(defun kite--dom-DOM-childNodeRemoved (websocket-url packet)
  (kite--log "kite--DOM-childNodeRemoved got packet %s" packet)
  (with-current-buffer (kite--dom-buffer websocket-url)
    (save-excursion
      (let ((inhibit-read-only t)
            (node-region (gethash (plist-get packet :nodeId) kite-dom-nodes)))
        (delete-region
         (node-region-line-begin node-region)
         (node-region-line-end node-region))))))

(defun kite--dom-DOM-attributeModified (websocket-url packet)
  (kite--log "kite--DOM-attributeModified got packet %s" packet)
  (with-current-buffer (kite--dom-buffer websocket-url)
    (save-excursion
      (message "packet is %s node-id is %s" packet (plist-get packet :nodeId))
      (message "kite-dom-nodes are %s" kite-dom-nodes)
      (let* ((inhibit-read-only t)
             (node-id (plist-get packet :nodeId))
             (attr-name (intern (plist-get packet :name)))
             (node-region (gethash node-id kite-dom-nodes))
             (attr-region (cdr (assq attr-name (node-region-attribute-regions node-region)))))
        (if attr-region
            ;; Modify existing attribute
            (progn
              (goto-char (1+ (attr-region-value-begin attr-region)))
              (delete-region (1+ (attr-region-value-begin attr-region))
                             (- (attr-region-value-end attr-region) 1))
              (widget-insert (propertize (plist-get packet :value)
                                  'keymap kite-dom-attr-value-keymap
                                  'field 'kite-dom-attribute
                                  'point-left 'kite--dom-attr-value-left
                                  'face 'kite-attribute-value-face)))
          ;; Insert new attribute
          (goto-char (attr-region-value-end (cdar (node-region-attribute-regions node-region))))
          (setf (node-region-attribute-regions node-region)
                (cons (kite--insert-attribute node-id (plist-get packet :name) (plist-get packet :value))
                      (node-region-attribute-regions node-region))))))))

(defun kite--dom-DOM-attributeRemoved (websocket-url packet)
  (kite--log "kite--DOM-attributeRemoved got packet %s" packet)
  (with-current-buffer (kite--dom-buffer websocket-url)
    (save-excursion
      (let* ((inhibit-read-only t)
             (attr-name (intern (plist-get packet :name)))
             (node-region (gethash (plist-get packet :nodeId) kite-dom-nodes))
             (attr-region (cdr (assq attr-name (node-region-attribute-regions node-region)))))
        (delete-region (attr-region-outer-begin attr-region)
                       (attr-region-outer-end attr-region))
        (setf (node-region-attribute-regions node-region)
              (assq-delete-all attr-name (node-region-attribute-regions node-region)))))))

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

(defun kite--rgba (r g b a)
  `((r . ,r)
    (g . ,g)
    (b . ,b)
    (a . ,a)))

(defun kite-dom-highlight-node ()
  (interactive)

  (kite-send "DOM.highlightNode"
             (list (cons 'nodeId
                         (get-char-property (point) 'kite-node-id))
                   (cons 'highlightConfig
                         `((showInfo . nil)
                           (contentColor . ,(kite--rgba 255 0 0 0.5))
                           (paddingColor . ,(kite--rgba 0 255 0 0.5))
                           (borderColor . ,(kite--rgba 0 0 255 0.5))
                           (marginColor . ,(kite--rgba 255 255 0 0.5)))))
             (lambda (response)
               (kite--log "DOM.highlightNode got response %s" response))))


(defun kite-dom-hide-highlight ()
  (interactive)
  (kite-send "DOM.hideHighlight"))

(defun kite-dom-show-matched-css ()
  (interactive)

  (kite-send "CSS.getMatchedStylesForNode"
             (list (cons 'nodeId
                         (get-char-property (point) 'kite-node-id)))
             (lambda (response)
               (message "CSS.getMatchedStylesForNode got response %s" response))))

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

(defconst kite--dom-pick-node-message
  "Now switch to your browser and select a DOM node")

(defun kite-dom-pick-node ()
  (interactive)
  (kite-send "DOM.setInspectModeEnabled"
             `((enabled . t)
               (highlightConfig
                . ((showInfo . nil)
                   (contentColor . ,(kite--rgba 255 0 0 0.5))
                   (paddingColor . ,(kite--rgba 0 255 0 0.5))
                   (borderColor . ,(kite--rgba 0 0 255 0.5))
                   (marginColor . ,(kite--rgba 255 255 0 0.5)))))
             (lambda (response)
               (message kite--dom-pick-node-message))))


(defun kite-dom-goto-node (node-id)
  (interactive)
  (let ((node-region (gethash node-id kite-dom-nodes)))
    (goto-char (node-region-line-begin node-region))
    (when (string= (current-message)
                   kite--dom-pick-node-message)
      (message nil))))

(defun kite--dom-Inspector-inspect (websocket-url packet)
  (lexical-let ((websocket-url websocket-url))
    (kite-send "DOM.requestNode" (list (assq 'objectId (plist-get packet :object)))
               (lambda (response)
                 (with-current-buffer (kite--dom-buffer websocket-url)
                   (kite-dom-goto-node
                    (plist-get (plist-get response :result) :nodeId)))))))

(defun kite--dom-node-at-point ()
  (get-text-property (point) 'kite-node-id))

(defun kite-dom-delete-node-or-attribute ()
  (interactive)
  (let ((node-id (kite--dom-node-at-point)))
    (when node-id
      (kite-send "DOM.removeNode" (list (cons 'nodeId node-id))))))

(defun kite-dom-narrow-to-node (&optional arg)
  (interactive)
  (let ((node-id (kite--dom-node-at-point)))
    (when node-id
      (let ((node-region (gethash node-id kite-dom-nodes)))
        (narrow-to-region
         (node-region-line-begin node-region)
         (node-region-line-end node-region))))))

(add-hook 'kite-DOM-attributeModified-hooks 'kite--dom-DOM-attributeModified)
(add-hook 'kite-DOM-attributeRemoved-hooks 'kite--dom-DOM-attributeRemoved)
(add-hook 'kite-DOM-childNodeCountUpdated-hooks 'kite--dom-DOM-childNodeCountUpdated)
(add-hook 'kite-DOM-childNodeInserted-hooks 'kite--dom-DOM-childNodeInserted)
(add-hook 'kite-DOM-childNodeRemoved-hooks 'kite--dom-DOM-childNodeRemoved)
(add-hook 'kite-DOM-setChildNodes-hooks 'kite--dom-DOM-setChildNodes)
(add-hook 'kite-Inspector-inspect-hooks 'kite--dom-Inspector-inspect)

(provide 'kite-dom)
