;;; kite-dom.el --- Kite DOM module implementation

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

;; This package implements the WebKit DOM inspector.
;;
;; It is part of Kite, a WebKit inspector front-end.


;;; Code:

(require 'kite-global)
(require 'kite-color)
(require 'kite-dom-css)
(require 'widget)
;; Try loading nxml-mode so we can steal their faces
(require 'nxml-mode nil t)

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
  attribute-regions
  widget
  parent
  child-count
  children
  node-id
  node-type)

(defstruct (attr-region)
  outer-begin
  outer-end
  value-begin
  value-end
  value-widget)

;; Per https://developer.mozilla.org/en-US/docs/DOM/Node.nodeType
(defconst kite-dom-element-node 1)
(defconst kite-dom-attribute-node 2)
(defconst kite-dom-text-node 3)
(defconst kite-dom-cdata-section-node 4)
(defconst kite-dom-entity-reference-node 5)
(defconst kite-dom-entity-node 6)
(defconst kite-dom-processing-instruction-node 7)
(defconst kite-dom-comment-node 8)
(defconst kite-dom-document-node 9)
(defconst kite-dom-document-type-node 10)
(defconst kite-dom-document-fragment-node 11)
(defconst kite-dom-notation-node 12)

;;; Below default colors and face definitions shamelessly stolen from
;;; nxml.  However, we try to derive from nxml faces if possible in
;;; case we we inherit any changes the user might have made to them.

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
      `((((class color) (background light)) (:inherit nxml-text-face :background ,kite-light-blue-color))
        (((class color) (background dark)) (:inherit nxml-text-face :background ,kite-dark-blue-color)))
    `((((class color) (background light)) (:background ,kite-light-blue-color))
      (((class color) (background dark)) (:background ,kite-dark-blue-color))))
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

;;; End of stolen nxml colors and faces.

(defvar kite--dom-widget-field-keymap
  (let ((map (copy-keymap widget-field-keymap)))
    (define-key map "\M-\C-n" 'kite-dom-forward-element)
    (define-key map "\M-\C-p" 'kite-dom-backward-element)
    (define-key map "\C-m" 'kite--widget-field-activate)
    map))

(defvar kite-dom-mode-map
  (let ((map (make-composed-keymap
              (copy-keymap widget-keymap)
              (copy-keymap special-mode-map))))
    (suppress-keymap map t)
    (kite--define-global-mode-keys map)
    (define-key map (kbd "RET") 'kite-dom-toggle-node)
    (define-key map (kbd "DEL") 'kite-dom-delete-node-or-attribute)
    (define-key map "\C-Cp" 'kite-dom-pick-node)
    (define-key map "\C-Ch" 'kite-dom-highlight-node)
    (define-key map "\C-CH" 'kite-dom-hide-highlight)
    (define-key map "\C-Cc" 'kite-dom-show-matched-css)
    (define-key map "\C-CC" 'kite-dom-show-computed-css)
    (define-key map "\C-xnd" 'kite-dom-narrow-to-node)
    (define-key map "\M-\C-n" 'kite-dom-forward-element)
    (define-key map "\M-\C-p" 'kite-dom-backward-element)
    (define-key map [mouse-movement] 'kite-mouse-movement)
    map)
  "Local keymap for `kite-dom-mode' buffers.")

(defun kite-mouse-movement (event)
  "Called on mouse movement in a kite-dom buffer.  Highlights the
line under mouse and the corresponding DOM node in the browser."
  (interactive "e")
  (let ((mouse-point (nth 1 (nth 1 event))))
    (when (numberp mouse-point)
      (let ((node-id (kite--dom-node-at-point mouse-point)))
        (when (not (eq node-id kite--dom-highlighted-node-id))
          (setq kite--dom-highlighted-node-id node-id)
          (remove-overlays nil nil 'kite-dom-node-highlight t)
          (if (null node-id)
              (kite-send "DOM.hideHighlight")
            (let* ((node-region (gethash node-id kite-dom-nodes))
                   (overlay (make-overlay
                             (node-region-line-begin node-region)
                             (node-region-line-end node-region))))
              (overlay-put overlay 'kite-dom-node-highlight t)
              (overlay-put overlay 'priority 10)
              (overlay-put overlay 'face '(:background "#444")))
            (kite-send
             "DOM.highlightNode"
             `((nodeId . node-id)
               (highlightConfig
                . `((showInfo . nil)
                    (contentColor . ,(kite--rgba 255 0 0 0.5))
                    (paddingColor . ,(kite--rgba 0 255 0 0.5))
                    (borderColor . ,(kite--rgba 0 0 255 0.5))
                    (marginColor . ,(kite--rgba 255 255 0 0.5))))))))))))

(define-derived-mode kite-dom-mode special-mode "kite-dom"
  "Toggle kite dom mode."
  :group 'kite
  (setq kite-buffer-type 'dom)
  (set (make-local-variable 'kill-buffer-hook) 'kite--kill-dom)
  (setq buffer-read-only nil)
  (set (make-local-variable 'kite-dom-nodes) (make-hash-table))
  (set (make-local-variable 'kite--dom-highlighted-node-id) nil)
  (set (make-local-variable 'track-mouse) t)

  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-setup)

  (kite-send
   "CSS.enable"
   nil
   (lambda (response)
     (kite-send
      "CSS.getAllStyleSheets"
      nil
      (lambda (response)
        (kite--log "CSS.getAllStyleSheets got response %s" response)))))

  (kite-send "DOM.getDocument" nil
             (lambda (response)
               (kite--log "DOM.getDocument got response %s" response)
               (save-excursion
                 (let ((inhibit-read-only t))
                   (kite--dom-insert-document
                    (kite--get response :result :root)))
                 (widget-setup))))

  (run-mode-hooks 'kite-dom-mode-hook))

(defun kite--dom-insert-document (root-plist)
  "Insert the whole HTML document into the DOM buffer, given the
ROOT-PLIST as received in the response to `DOM.getDocument'."
  (let ((html-plist
         (find-if
          (lambda (child-plist)
            (string= (plist-get child-plist :nodeName) "HTML"))
          (plist-get root-plist :children))))
    (if html-plist
        (progn
          (kite--dom-insert-element html-plist 0 nil)
          (insert "\n"))
      (error "Document doesn't seem to contain html element"))))

(defconst kite-dom-offset 2
  "Number of spaces to use for each DOM indentation level.")

(defun kite--notify-widget (widget &rest ignore)
  "Experimental callback for widget-validate.  Highlight the
widget with `modified-face' to show that its contents have been
edited."
  (let ((modified-face (widget-get widget :modified-value-face)))
    (unless (eq (widget-get widget :value-face)
                modified-face)
      (widget-put widget :value-face modified-face)
      (overlay-put (widget-get widget :field-overlay)
                   'face modified-face))))

(defun kite--validate-widget (widget)
  "Experimental callback for widget-validate."
  (let ((val (widget-apply widget :value-get)))
    (message "validate, widget value %s" val)
    (unless (> (length val) 0)
      (widget-put widget :error "too short!")
      widget)))

(defun kite--insert-attribute (node-id attr-name attr-value)
  "Insert an attribute name/value pair at point.  ATTR-NAME is
the attribute name and ATTR-VALUE is the string representation of
the attribute value.  NODE-ID is the ID of the element the
attribute belongs to.

Return an (ATTR-NAME . ATTR-REGION) cons cell, with ATTR-REGION
describing the buffer region where the attribute was inserted."
  (let ((attr-begin (point-marker))
        (attr-region (make-attr-region)))

    (setf (attr-region-outer-begin attr-region) (point-marker))
    (widget-insert (concat
                    " "
                    (propertize attr-name
                                'face 'kite-attribute-local-name-face
                                'font-lock-face 'kite-attribute-local-name-face)
                    "="))
    (setf (attr-region-value-begin attr-region) (point-marker))
    (setf (attr-region-value-widget attr-region)
          (widget-create
           'editable-field
           :size 0
           :format (propertize
                    "\"%v\""
                    'face 'kite-attribute-value-delimiter-face)
           :value-face 'kite-attribute-value-face
           :modified-value-face 'kite-modified-attribute-value-face
           :notify (function kite--notify-widget)
           :kite-node-id node-id
           :kite-attr-name attr-name
           :keymap kite--dom-widget-field-keymap
           :action (lambda (widget &rest ignore)
                     (lexical-let ((lex-widget widget))
                       (kite-send
                        "DOM.setAttributeValue"
                        `((nodeId . ,(widget-get widget :kite-node-id))
                          (name . ,(widget-get widget :kite-attr-name))
                          (value . ,(widget-value widget)))
                        (lambda (response)
                          (when (plist-member response :result)
                            (widget-put lex-widget
                                        :value-face
                                        'kite-attribute-value-face)
                            (overlay-put
                             (widget-get lex-widget :field-overlay)
                             'face 'kite-attribute-value-face))))))
           attr-value))
    (setf (attr-region-value-end attr-region) (point-marker))
    (setf (attr-region-outer-end attr-region) (point-marker))

    (put-text-property (attr-region-outer-begin attr-region)
                       (attr-region-outer-end attr-region)
                       'kite-attr-name attr-name)

    (cons (intern attr-name) attr-region)))

(defun kite--dom-render-attribute-regions (element)
  "Insert all attributes of ELEMENT.  ELEMENT is a plist
representing the element and its attributes, as provided by the
remote debugger.  Return a list of (ATTR-NAME . ATTR-REGION) cons
cells in the same order as the attributes in the element plist."
  (let* (attribute-info-list
         (node-id (plist-get element :nodeId))
         (attributes (plist-get element :attributes))
         (attr-index 0)
         (num-attrs (length attributes))
         (inhibit-read-only t))
    (while (< attr-index num-attrs)
      (setq attribute-info-list
            (cons
             (kite--insert-attribute
              node-id
              (elt attributes attr-index)
              (elt attributes (1+ attr-index)))
             attribute-info-list))
      (setq attr-index (+ 2 attr-index)))
    attribute-info-list))

(defun kite--dom-leading-whitespace (node1 node2)
  "Return the whitespace required before NODE2 given that it is
preceded by NODE1.  NODE1 may be nil, which indicates that NODE2
is the first child."
  (cond
   ((eq (node-region-node-type node2) kite-dom-element-node)
    (concat
     "\n"
     (make-string (* kite-dom-offset (node-region-indent node2)) 32)))
   (t
    "")))

(defun kite--dom-trailing-whitespace (node1 node2)
  "Return the whitespace required after NODE1 given that it is
followed by NODE2.  NODE2 may be nil, which indicates that NODE1
is the last child."
  (cond
   ((or (eq (node-region-node-type node1) kite-dom-text-node)
        (and (not (null node2))
             (eq (node-region-node-type node2) kite-dom-text-node)))
    "")
   ((null node2)
    (concat
     "\n "
     (make-string (* kite-dom-offset (- (node-region-indent node1) 1)) 32)))
   (t
    "")))

(defun kite--dom-update-inner-whitespace (node-region)
  "For each child, update leading and trailing whitespace."
  (save-excursion
    (let* ((children (node-region-children node-region))
           (last-child (- (length children) 1))
           (overlays (overlays-in 0 (buffer-size))) ;; (node-region-line-begin node-region)
           ;;(node-region-line-end node-region)))
           (overlay-lengths (mapcar (lambda (overlay)
                                      (- (overlay-end overlay)
                                         (overlay-start overlay)))
                                    overlays)))

      (loop for index from 0 upto last-child do
            (let* ((prev-child (and (> index 0)
                                    (nth (- index 1) children)))
                   (child (nth index children))
                   (next-child (and (< index last-child)
                                    (nth (1+ index) children))))

              ;; delete any leading whitespace
              (delete-region (node-region-line-begin child)
                             (node-region-outer-begin child))
              ;; insert leading whitespace
              (goto-char (node-region-line-begin child))
              (let ((save-point (point)))
                (widget-insert (propertize
                                (kite--dom-leading-whitespace prev-child child)
                                'kite-node-id (node-region-node-id child)))
                (set-marker (node-region-line-begin child) save-point)
                (set-marker (node-region-outer-begin child) (point)))

              ;; delete any trailing whitespace
              (delete-region (node-region-outer-end child)
                             (node-region-line-end child))
              ;; insert trailing whitespace
              (goto-char (node-region-outer-end child))
              (let ((save-point (point)))
                (widget-insert (propertize
                                (kite--dom-trailing-whitespace child next-child)
                                'kite-node-id (node-region-node-id child)))
                (set-marker (node-region-outer-end child) save-point)
                (set-marker (node-region-line-end child) (point)))))

      (mapcar* (lambda (overlay old-length)
                 (kite--log "overlay %s old-length %s" overlay old-length)
                 (move-overlay overlay (overlay-start overlay) (+ (overlay-start overlay) old-length)))
               overlays overlay-lengths))))

(defun kite--dom-insert-element (element indent loadp)
  "Insert ELEMENT at point, at indentation level INDENT.  If
LOADP, recursively and asynchronously load and insert children.

FIXME: this needs to be smarter about when to load children."
  (flet ((indent-prefix (indent node-id)
                        (propertize
                         (make-string (* kite-dom-offset indent) 32)
                         'kite-node-id node-id
                         'read-only t)))
    (let ((nodeType (plist-get element :nodeType))
          (node-id (plist-get element :nodeId))
          (localName (plist-get element :localName))
          (inhibit-read-only t)
          (node-region (make-node-region))
          attributes)

      (setf (node-region-node-id node-region) node-id)
      (setf (node-region-node-type node-region) nodeType)
      (setf (node-region-indent node-region) indent)

      (setf (node-region-line-begin node-region) (point-marker))
      (setf (node-region-child-count node-region)
            (plist-get element :childNodeCount))

      (cond
       ((and (eq nodeType kite-dom-element-node)
             (or (eq 0 (plist-get element :childNodeCount))
                 (plist-member element :children)))
        (if (> (plist-get element :childNodeCount) 0)
            (widget-insert (if (plist-member element :children) "-" "+"))
          (widget-insert " "))
        (widget-insert (propertize "<"
                                   'kite-node-id node-id
                                   'face 'kite-tag-delimiter-face))
        (setf (node-region-widget node-region)
              (widget-create
               'editable-field
               :size 1
               :value-face 'kite-element-local-name-face
               :modified-value-face 'kite-modified-element-local-name-face
               :notify (function kite--notify-widget)
               :validate (function kite--validate-widget)
               :match (lambda (x) (> (length (widget-value x)) 0))
               :kite-node-id node-id
               :keymap kite--dom-widget-field-keymap
               localName))
        (setq attributes (kite--dom-render-attribute-regions element))
        (widget-insert (concat (propertize ">"
                                           'kite-node-id node-id
                                           'read-only t
                                           'face 'kite-tag-delimiter-face)))
        (setf (node-region-inner-begin node-region) (point-marker))
        (put-text-property (node-region-line-begin node-region)
                           (node-region-inner-begin node-region)
                           'kite-node-id
                           node-id)

        (puthash node-id
                 (plist-get element :children)
                 (kite-session-dom-children-cache kite-session))

        (setf (node-region-children node-region)
              (mapcar (lambda (child)
                        (let ((new-node-region (kite--dom-insert-element child (1+ indent) loadp)))
                          (setf (node-region-parent new-node-region) node-region)
                          new-node-region))
                      (plist-get element :children)))

        (setf (node-region-inner-end node-region) (point-marker))
        (widget-insert
         (concat
          (propertize "<" 'face 'kite-tag-delimiter-face)
          (propertize "/" 'face 'kite-tag-slash-face)
          (propertize localName 'face 'kite-element-local-name-face)
          (propertize ">" 'face 'kite-tag-delimiter-face)))
        (put-text-property (node-region-inner-end node-region)
                           (point)
                           'kite-node-id
                           node-id))

       ((eq nodeType kite-dom-element-node)
        (widget-insert "+")
        (widget-insert (propertize "<"
                                   'face 'kite-tag-delimiter-face))
        (setf (node-region-widget node-region)
              (widget-create 'editable-field
                             :size 1
                             :value-face 'kite-element-local-name-face
                             :modified-value-face 'kite-modified-element-local-name-face
                             :notify (function kite--notify-widget)
                             :validate (function kite--validate-widget)
                             :match (lambda (x) (> (length (widget-value x)) 0))
                             :kite-node-id node-id
                             :keymap kite--dom-widget-field-keymap
                             localName))
        (setq attributes (kite--dom-render-attribute-regions element))
        (widget-insert (propertize ">"
                                   'face 'kite-tag-delimiter-face))
        (setf (node-region-inner-begin node-region) (point-marker))
        (widget-insert "...")
        (setf (node-region-inner-end node-region) (point-marker))
        (widget-insert
         (concat
          (propertize "<" 'face 'kite-tag-delimiter-face)
          (propertize "/" 'face 'kite-tag-slash-face)
          (propertize localName 'face 'kite-element-local-name-face)
          (propertize ">" 'face 'kite-tag-delimiter-face)))
        (put-text-property (node-region-line-begin node-region)
                           (point)
                           'kite-node-id
                           node-id)
        (when loadp
          (kite-send "DOM.requestChildNodes"
                     (list (cons 'nodeId (plist-get element :nodeId)))
                     (lambda (response) nil))))

       ((eq nodeType kite-dom-text-node)
        (setf (node-region-widget node-region)
              (widget-create 'editable-field
                             :size 1
                             :value-face 'kite-text-face
                             :modified-value-face 'kite-modified-text-face
                             :notify (function kite--notify-widget)
                             :validate (function kite--validate-widget)
                             :kite-node-id node-id
                             :keymap kite--dom-widget-field-keymap
                             (plist-get element :nodeValue))))

       ((eq nodeType kite-dom-comment-node)
        (widget-insert
         (propertize
          "<!--"
          'face 'kite-comment-delimiter-face))

        (setf (node-region-widget node-region)
              (widget-create 'editable-field
                             :size 1
                             :value-face 'kite-comment-content-face
                             :modified-value-face 'kite-modified-comment-content-face
                             :notify (function kite--notify-widget)
                             :validate (function kite--validate-widget)
                             :kite-node-id node-id
                             :keymap kite--dom-widget-field-keymap
                             (plist-get element :nodeValue)))
        (widget-insert
         (propertize
          "-->"
          'face 'kite-comment-delimiter-face)
         "\n")
        (put-text-property (node-region-line-begin node-region)
                           (point)
                           'kite-node-id
                           node-id))

       (t
        (error "Unknown node type %s" nodeType)))

      (setf (node-region-line-end node-region) (point-marker))

      (setf (node-region-outer-begin node-region)
            (copy-marker (node-region-line-begin node-region)))
      (setf (node-region-outer-end node-region)
            (copy-marker (node-region-line-end node-region)))

      (when (eq nodeType kite-dom-element-node)
        (kite--dom-update-inner-whitespace node-region))

      (setf (node-region-attribute-regions node-region) attributes)
      (when (node-region-inner-begin node-region)
        (set-marker-insertion-type (node-region-inner-begin node-region) nil)
        (set-marker-insertion-type (node-region-inner-end node-region) t))
      (when (node-region-outer-begin node-region)
        (set-marker-insertion-type (node-region-outer-begin node-region) t)
        (set-marker-insertion-type (node-region-outer-end node-region) nil))
      (when (node-region-line-begin node-region)
        (set-marker-insertion-type (node-region-line-begin node-region) t)
        (set-marker-insertion-type (node-region-line-end node-region) nil))
      (puthash (plist-get element :nodeId) node-region kite-dom-nodes)
      node-region)))

(defun kite--kill-dom ()
  "Obsolete. FIXME"
  t)
;  (ignore-errors
;    (kite-send "CSS.disable" nil
;               (lambda (response) (message "CSS disabled.")))))

(defun kite--websocket-url ()
  "Obsolete. FIXME"
  (websocket-url (kite-session-websocket kite-session)))

(defun kite--dom-buffer (websocket-url)
  "Obsolete. FIXME"
  (let ((buffer-iterator (buffer-list))
        found)
    (while (and buffer-iterator (not found))
      (let ((buffer-kite-session (buffer-local-value
                                  'kite-session
                                  (car buffer-iterator))))
        (when (and buffer-kite-session
                   (string= (websocket-url (kite-session-websocket buffer-kite-session))
                            websocket-url)
                   (eq 'dom (buffer-local-value 'kite-buffer-type (car buffer-iterator))))
          (setq found (car buffer-iterator))))
      (setq buffer-iterator (cdr buffer-iterator)))
    found))

(defun kite--delete-child-widgets (node-region)
  "Delete all widgets in the node-region's children,
recursively."
  (dolist (child (node-region-children node-region))
    (kite--delete-widgets child)))

(defun kite--delete-widgets (node-region)
  "Delete the node-region's widgets and all its children's
widgets, recursively"
  (widget-delete (node-region-widget node-region))
  (dolist (attr-name-and-region (node-region-attribute-regions node-region))
    (widget-delete (attr-region-value-widget (cdr attr-name-and-region))))
  (kite--delete-child-widgets node-region))

(defun kite--dom-set-element-toggle-char (node-region char)
  "Set the character denoting whether an element is opened,
closed, or doesn't have any child nodes, for NODE-REGION to
CHARACTER."
  (save-excursion
    (goto-char (node-region-outer-begin node-region))
    (forward-char)
    (widget-insert (propertize
                    char
                    'kite-node-id
                    (node-region-node-id node-region)))
    (forward-char -1)
    (delete-char -1)))

(defun kite--dom-show-child-nodes (parent-node-id children)
  (let ((inhibit-read-only t)
        (node-region
         (gethash parent-node-id kite-dom-nodes)))
    (save-excursion
      (kite--dom-set-element-toggle-char node-region "-")
      (kite--delete-child-widgets node-region)
      (delete-region (node-region-inner-begin node-region)
                     (node-region-inner-end node-region))
      (goto-char (node-region-inner-begin node-region))
      (atomic-change-group
        (setf (node-region-children node-region)
              (mapcar
               (lambda (child)
                 (let ((new-node-region
                        (kite--dom-insert-element
                         child
                         (1+ (node-region-indent node-region))
                         nil)))
                   (setf (node-region-parent new-node-region)
                         node-region)
                   new-node-region))
               children))))
    (widget-setup)
    (kite--dom-update-inner-whitespace node-region)))

(defun kite--dom-DOM-setChildNodes (websocket-url packet)
  "Callback invoked for the `DOM.setChildNodes' notification,
which the remote debugger wants to provide us with missing DOM
structure, for example in response to a `DOM.requestChildNodes'
request."
  (kite--log "kite--DOM-setChildNodes got packet %s" packet)
  (with-current-buffer (kite--dom-buffer websocket-url)
    (puthash (plist-get packet :parentId)
             (plist-get packet :nodes)
             (kite-session-dom-children-cache kite-session))
    (kite--dom-show-child-nodes (plist-get packet :parentId)
                                (plist-get packet :nodes))))

(defun kite--dom-DOM-childNodeInserted (websocket-url packet)
  "Callback invoked for the `DOM.childNodeInserted' notification,
which the remote debugger sends when a script has inserted a
child node into a DOM element."
  (kite--log "kite--DOM-childNodeInserted got packet %s" packet)
  (with-current-buffer (kite--dom-buffer websocket-url)
    (save-excursion
      (let* ((inhibit-read-only t)
             (previous-node-id (plist-get packet :previousNodeId))
             (parent-node-id (plist-get packet :parentNodeId))
             (node-region (gethash parent-node-id kite-dom-nodes))
             (previous-node-region
              (gethash previous-node-id kite-dom-nodes)))

        (kite--dom-set-element-toggle-char node-region "-")

        (goto-char (if (eq previous-node-id 0)
                       (node-region-inner-begin node-region)
                     (node-region-line-end previous-node-region)))
        (let ((new-node-region
               (kite--dom-insert-element
                (plist-get packet :node)
                (1+ (node-region-indent node-region))
                t)))
          (if (eq previous-node-id 0)
              (push new-node-region (node-region-children node-region))
            (let ((insert-position
                   (1+ (position previous-node-region
                                 (node-region-children node-region)))))
              (setf (node-region-children node-region)
                    (append
                     (subseq (node-region-children node-region)
                             0
                             insert-position)
                     (list new-node-region)
                     (subseq (node-region-children node-region)
                             insert-position)))))
          (setf (node-region-parent new-node-region) node-region))
        (widget-setup)
        (kite--dom-update-inner-whitespace node-region)))))

(defun kite--dom-DOM-childNodeCountUpdated (websocket-url packet)
  "Callback invoked for the `DOM.childNodeCountUpdated' notification,
which the remote debugger sends when the number of child nodes of
a DOM element has changed."
  (kite--log "kite--DOM-childNodeCountUpdated got packet %s" packet))

(defun kite--dom-DOM-childNodeRemoved (websocket-url packet)
  "Callback invoked for the `DOM.childNodeRemoved' notification,
which the remote debugger sends when a script has removed a child
node from a DOM element."
  (kite--log "kite--DOM-childNodeRemoved got packet %s" packet)
  (with-current-buffer (kite--dom-buffer websocket-url)
    (save-excursion
      (let ((inhibit-read-only t)
            (node-region (gethash (plist-get packet :nodeId) kite-dom-nodes)))
        (kite--delete-widgets node-region)
        (delete-region
         (node-region-line-begin node-region)
         (node-region-line-end node-region))
        (setf (node-region-children (node-region-parent node-region))
              (delete node-region
                      (node-region-children (node-region-parent node-region))))
        (when (eq 0 (length (node-region-children (node-region-parent node-region))))
          (kite--dom-set-element-toggle-char node-region " "))

        (kite--dom-update-inner-whitespace (node-region-parent node-region)))
      (widget-setup))))

(defun kite--dom-DOM-attributeModified (websocket-url packet)
  "Callback invoked for the `DOM.attributeModified' notification,
which the remote debugger sends when a script has modified the
value of an attribute in a DOM element."
  (kite--log "kite--DOM-attributeModified got packet %s" packet)
  (with-current-buffer (kite--dom-buffer websocket-url)
    (save-excursion
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
                                         'face 'kite-attribute-value-face)))
          ;; Insert new attribute
          (goto-char (attr-region-value-end (cdar (node-region-attribute-regions node-region))))
          (setf (node-region-attribute-regions node-region)
                (cons (kite--insert-attribute node-id (plist-get packet :name) (plist-get packet :value))
                      (node-region-attribute-regions node-region)))))
      (widget-setup))))

(defun kite--dom-DOM-attributeRemoved (websocket-url packet)
  "Callback invoked for the `DOM.attributeRemoved' notification,
which the remote debugger sends when a script has removed an
attribute from a DOM element."
  (kite--log "kite--DOM-attributeRemoved got packet %s" packet)
  (with-current-buffer (kite--dom-buffer websocket-url)
    (save-excursion
      (let* ((inhibit-read-only t)
             (attr-name (intern (plist-get packet :name)))
             (node-region (gethash (plist-get packet :nodeId) kite-dom-nodes))
             (attr-region (cdr (assq attr-name (node-region-attribute-regions node-region)))))
        (kite--log "kite--DOM-attributeRemoved, attr-region=%s (%s)" attr-region (node-region-attribute-regions node-region))
        (widget-delete (attr-region-value-widget attr-region))
        (delete-region (attr-region-outer-begin attr-region)
                       (attr-region-outer-end attr-region))
        (setf (node-region-attribute-regions node-region)
              (assq-delete-all attr-name (node-region-attribute-regions node-region)))))))

(defun kite-dom-backward-up-element (&optional arg)
  "Move backward over one element, or up the tree if this is there are
no previous siblings.
With ARG, do it that many times.
Negative ARG means move forward."
  (interactive "p")
  t)

(defun kite-dom-down-element (&optional arg)
  "Move forward down into the content of an element.
With ARG, do this that many times.
Negative ARG means move backward but still down."
  (interactive "p")
  t)

(defun kite-dom-forward-element (&optional arg)
  "Move forward over one element.
With ARG, do it that many times.
Negative ARG means move backward."
  (interactive "p")
  (let* ((node-region (kite--dom-node-region-at-point))
         (parent-children (node-region-children
                           (node-region-parent node-region)))
         (node-position (position node-region parent-children))
         (new-position (max 0 (min (+ node-position arg)
                                   (- (length parent-children) 1))))
         (next-node-region (nth new-position parent-children)))
    (goto-char (1+ (node-region-outer-begin next-node-region)))))

(defun kite-dom-backward-element (&optional arg)
  "Move backward over one element.
With ARG, do it that many times.
Negative ARG means move forward."
  (interactive "p")
  (kite-dom-forward-element (- arg)))

(defun kite-backward-paragraph (&optional arg)
  "Move backward to start of paragraph.
With argument ARG, do it ARG times;
a negative argument ARG = -N means move forward N paragraphs."
  (interactive "p")
  t)

(defun kite-forward-paragraph (&optional arg)
  "Move forward to end of paragraph.
With argument ARG, do it ARG times;
a negative argument ARG = -N means move backward N paragraphs."
  (interactive "p")
  t)

(defun kite-mark-paragraph (&optional arg)
  "Put point at beginning of this paragraph, mark at end.
The paragraph marked is the one that contains point or follows point.

FIXME: not yet implemented."
  (interactive "p")
  t)

(defun kite--rgba (r g b a)
  "Return the given RGBA color value in the WebKit remote
debugger API `RGBA' structure format."
  `((r . ,r)
    (g . ,g)
    (b . ,b)
    (a . ,a)))

(defun kite-dom-highlight-node ()
  "Highlight the node at point in the browser by sending the
`DOM.highlightNode' message to the remote debugger."
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
  "Send the `DOM.hideHighlight' message to the remote debugger,
which removes any highlight previously activated via
`kite-dom-highlight-node'."
  (interactive)
  (kite-send "DOM.hideHighlight"))

(defconst kite--dom-pick-node-message
  "Now switch to your browser and select a DOM node"
  "Message displayed by `kite-dom-pick-node'.")

(defun kite-dom-pick-node ()
  "Put the remote debugger into the mode in which the user can
select an element interactively using the mouse.  Once the user
selects an element, `kite--dom-Inspector-inspect' is invoked
which in turn invokes `kite-dom-goto-node' to move point to the
selected element in the DOM view."
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
  "Move point to the node with the given NODE-ID.  Clears any
existing message, in order to remove the message put there by
`kite-dom-pick-node'.

FIXME: the latter should be moved into
`kite--dom-Inspector-inspect'."
  (interactive)
  (let ((node-region (gethash node-id kite-dom-nodes)))
    (goto-char (node-region-line-begin node-region))
    (when (string= (current-message)
                   kite--dom-pick-node-message)
      (message nil))))

(defun kite--dom-Inspector-inspect (websocket-url packet)
  "Callback invoked for the `Inspector.inspect' notification,
which the remote debugger sends when the user selects an element
with the mouse.  Invokes `kite-dom-goto-node' for the element in
question."
  (lexical-let ((websocket-url websocket-url))
    (kite-send "DOM.requestNode" (list (assq 'objectId (plist-get packet :object)))
               (lambda (response)
                 (with-current-buffer (kite--dom-buffer websocket-url)
                   (kite-dom-goto-node
                    (plist-get (plist-get response :result) :nodeId)))))))

(defun kite--dom-node-at-point (&optional arg)
  "Return the `nodeId' for node at point."
  (interactive)
  (let ((point-arg (or arg (point))))
    (or (get-text-property point-arg 'kite-node-id)
        (let ((widget (widget-at point-arg)))
          (when widget
            (widget-get widget :kite-node-id))))))

(defun kite--dom-node-region-at-point (&optional arg)
  "Return then node region for node at point."
  (interactive)
  (gethash (kite--dom-node-at-point arg) kite-dom-nodes))

(defun kite-dom-delete-node-or-attribute ()
  "Delete node or attribute at point."
  (interactive)
  (let ((node-id (kite--dom-node-at-point)))
    (when node-id
      (let ((attr-name (get-text-property (point) 'kite-attr-name)))
        (if attr-name
            (kite-send "DOM.removeAttribute"
                       `((nodeId . ,node-id)
                         (name . ,attr-name)))
          (kite-send "DOM.removeNode" `((nodeId . ,node-id))))))))

(defun kite-dom-narrow-to-node (&optional arg)
  "Narrow buffer to node at point."
  (interactive)
  (let ((node-id (kite--dom-node-at-point)))
    (when node-id
      (let ((node-region (gethash node-id kite-dom-nodes)))
        (narrow-to-region
         (node-region-line-begin node-region)
         (node-region-line-end node-region))))))

(defun kite-dom-toggle-node ()
  (interactive)
  (let ((node-region (kite--dom-node-region-at-point)))
    (when (> (node-region-child-count node-region) 0)
      (if (null (node-region-children node-region))
          (let ((cached-children
                 (gethash
                  (node-region-node-id node-region)
                  (kite-session-dom-children-cache kite-session))))
            (if cached-children
                (kite--dom-show-child-nodes
                 (node-region-node-id node-region)
                 cached-children)
              (message "getting children for %s"
                       (node-region-node-id node-region))
              (kite-send
               "DOM.requestChildNodes"
               `((nodeId . ,(node-region-node-id node-region))))))
        (save-excursion
          (kite--delete-child-widgets node-region)
          (setf (node-region-children node-region))
          (let ((inhibit-read-only t))
            (goto-char (node-region-inner-begin node-region))
            (insert "...")
            (delete-region (point)
                           (node-region-inner-end node-region))
            (goto-char (1+ (node-region-outer-begin node-region)))
            (widget-insert
             (propertize "+"
                         'kite-node-id
                         (node-region-node-id node-region)))
            (forward-char -1)
            (delete-char -1)))))))

(add-hook 'kite-DOM-attributeModified-hooks 'kite--dom-DOM-attributeModified)
(add-hook 'kite-DOM-attributeRemoved-hooks 'kite--dom-DOM-attributeRemoved)
(add-hook 'kite-DOM-childNodeCountUpdated-hooks 'kite--dom-DOM-childNodeCountUpdated)
(add-hook 'kite-DOM-childNodeInserted-hooks 'kite--dom-DOM-childNodeInserted)
(add-hook 'kite-DOM-childNodeRemoved-hooks 'kite--dom-DOM-childNodeRemoved)
(add-hook 'kite-DOM-setChildNodes-hooks 'kite--dom-DOM-setChildNodes)
(add-hook 'kite-Inspector-inspect-hooks 'kite--dom-Inspector-inspect)

(provide 'kite-dom)

;;; kite-dom.el ends here
