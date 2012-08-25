;;; kite-dom.el --- Kite DOM module implementation

;; Copyright (C) 2012 Julian Scheid

;; Author: Julian Scheid
;; Keywords: tools, WWW

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

(defvar kite-dom-mode-map nil
  "Local keymap for `kite-dom-mode' buffers.")

(setq kite-dom-mode-map
      (let ((map (make-sparse-keymap))
            (menu-map (make-sparse-keymap)))
        ;;(define-key map [remap self-insert-command] 'kite-dom-no-edit)
        (set-keymap-parent map widget-keymap)
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

        (define-key map [mouse-movement] 'kite-mouse-movement)

        map))

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

  (add-hook
   (make-local-variable 'kite-after-mode-hooks)
   (lambda ()
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
                  (with-current-buffer buf
                    (save-excursion
                      (kite--dom-insert-element
                       (elt (plist-get
                             (plist-get
                              (plist-get
                               response
                               :result)
                              :root)
                             :children) 0)
                       0 t)
                      (widget-setup))))))))

(defconst kite-dom-offset 2)

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
                   :kite-node-id node-id
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
                   :kite-node-id node-id
                   attr-value)
    (widget-insert (propertize "\""
                        'kite-node-id node-id
                        'face 'kite-attribute-value-delimiter-face))
    (setf (attr-region-value-end attr-region) (point-marker))
    (setf (attr-region-outer-end attr-region) (point-marker))

    (cons (intern attr-name) attr-region)))

(defun kite--dom-render-attribute-regions (element)
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

(defun kite--dom-insert-element (element indent loadp)
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

      (setf (node-region-line-begin node-region) (point-marker))

      (cond
       ((and (eq nodeType 1)
             (or (eq 0 (plist-get element :childNodeCount))
                 (plist-member element :children)))
        (widget-insert (concat (indent-prefix indent node-id)
                               (propertize "<"
                                           'kite-node-id node-id
                                           'face 'kite-tag-delimiter-face)))
        (widget-create
         'editable-field
         :size 1
         :value-face 'kite-element-local-name-face
         :modified-value-face 'kite-modified-element-local-name-face
         :notify (function kite--notify-widget)
         :validate (function kite--validate-widget)
         :match (lambda (x) (> (length (widget-value x)) 0))
         :kite-node-id node-id
         localName)
        (setq attributes (kite--dom-render-attribute-regions element))
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
        (widget-insert
         (concat
          (indent-prefix indent node-id)
          (propertize "<" 'face 'kite-tag-delimiter-face)
          (propertize "/" 'face 'kite-tag-slash-face)
          (propertize localName 'face 'kite-element-local-name-face)
          (propertize ">\n" 'face 'kite-tag-delimiter-face)))
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
                       :kite-node-id node-id
                       localName)
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
          (propertize ">" 'face 'kite-tag-delimiter-face)
          "\n"))
        (put-text-property (node-region-line-begin node-region)
                           (point)
                           'kite-node-id
                           node-id)
        (when loadp
          (kite-send "DOM.requestChildNodes"
                     (list (cons 'nodeId (plist-get element :nodeId)))
                     (lambda (response) nil))))

       ((eq nodeType 3)
        (widget-insert
         (concat
          (indent-prefix indent node-id)
          (propertize (replace-regexp-in-string
                       "\\(^\\(\\s \\|\n\\)+\\|\\(\\s \\|\n\\)+$\\)" ""
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
       (buf (kite--dom-buffer (kite--websocket-url))))
    (if buf
        (switch-to-buffer buf)
      (setq buf (generate-new-buffer
                 (format "*kite dom %s*"
                         (kite-session-unique-name kite-session))))
      (with-current-buffer buf
        (kite-dom-mode)
        (let ((inhibit-read-only t))
          (erase-buffer))
        (remove-overlays)
        (widget-setup)
        (set (make-local-variable 'kite-session) kite-session))
      (switch-to-buffer buf)
      (kite-send
       "CSS.enable"
       nil
       (lambda (response)
         (message "CSS.enable got response %s" response)
         (kite-send
          "CSS.getAllStyleSheets"
          nil
          (lambda (response)
            (message "CSS.getAllStyleSheets got response %s" response)))))
      (kite-send "DOM.getDocument" nil
                 (lambda (response)
                   (kite--log "DOM.getDocument got response %s" response)
                   (with-current-buffer buf
                     (save-excursion
                       (kite--dom-insert-element
                        (elt (plist-get
                              (plist-get
                               (plist-get
                                response
                                :result)
                               :root)
                              :children) 0)
                        0 t)
                       (widget-setup))))))))

(defun kite--dom-buffer (websocket-url)
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

(defun kite--dom-node-at-point (&optional arg)
  (interactive)
  (let ((point-arg (or arg (point))))
    (or (get-text-property point-arg 'kite-node-id)
        (let ((widget (widget-at point-arg)))
          (when widget
            (widget-get widget :kite-node-id))))))

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
