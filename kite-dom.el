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
(require 'cl)
(require 'widget)
;; Try loading nxml-mode so we can steal their faces
(require 'nxml-mode nil t)

(eval-when-compile
  (require 'wid-edit))

(defstruct (kite-dom-node)
  line-begin
  line-end
  outer-begin
  outer-end
  inner-begin
  inner-end
  indent
  attr-alist
  widget
  parent
  child-count
  children
  id
  type
  value
  local-name
  openp)

(defstruct (kite-dom-attr)
  name
  value
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
    `((((class color) (background light))
       (:foreground ,kite-dark-blue-color))
      (((class color) (background dark))
       (:foreground ,kite-light-green-color))))
  "Face used to highlight data enclosed between delimiters.
By default, this is inherited by `kite-attribute-value-face'
and `kite-processing-instruction-content-face'."
  :group 'kite-highlighting-faces)

(defface kite-name-face
  (if (facep 'nxml-name-face)
      '((t (:inherit nxml-name-face)))
    `((((class color) (background light))
       (:foreground ,kite-green-color))
      (((class color) (background dark))
       (:foreground ,kite-sky-blue-color))))
  "Face used to highlight various names.
This includes element and attribute names, processing
instruction targets and the CDATA keyword in a CDATA section.
This is not used directly, but only via inheritance by other faces."
  :group 'kite-highlighting-faces)

(defface kite-ref-face
  (if (facep 'nxml-ref-face)
      '((t (:inherit nxml-ref-face)))
    `((((class color) (background light))
       (:foreground ,kite-light-blue-color))
      (((class color) (background dark))
       (:foreground ,kite-dark-green-color))))
  "Face used to highlight character and entity references.
This is not used directly, but only via inheritance by other faces."
  :group 'kite-highlighting-faces)

(defface kite-delimiter-face
  (if (facep 'nxml-delimiter-face)
      '((t (:inherit nxml-delimiter-face)))
    `((((class color) (background light))
       (:foreground ,kite-light-blue-color))
      (((class color) (background dark))
       (:foreground ,kite-dark-green-color))
      (t (:bold t))))
  "Face used to highlight delimiters.
This is not used directly, but only via inheritance by other faces."
  :group 'kite-highlighting-faces)

(defface kite-text-face
  (if (facep 'nxml-text-face)
      `((((class color) (background light))
         (:inherit nxml-text-face :background ,kite-light-blue-color))
        (((class color) (background dark))
         (:inherit nxml-text-face :background ,kite-dark-blue-color)))
    `((((class color) (background light))
       (:background ,kite-light-blue-color))
      (((class color) (background dark))
       (:background ,kite-dark-blue-color))))
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
  "Face used for the delimiters of processing instructions, i.e
<? and ?>."
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
  "Face used for the delimiters of CDATA sections, i.e <![, [,
and ]]>."
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
  "Face used for the delimiters of character references, i.e &#
and ;."
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
  "Face used for slashes in tags, both in end-tags and
empty-elements."
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

(defface kite-node-highlight-face
  '((t (:background "#444")))
  "Face used for highlighting the DOM element under the mouse cursor."
  :group 'kite-highlighting-faces)

;;; End of stolen nxml colors and faces.

(defvar kite--dom-widget-field-keymap
  (let ((map (copy-keymap widget-field-keymap)))
    (define-key map "\M-\C-n" 'kite-dom-forward-element)
    (define-key map "\M-\C-p" 'kite-dom-backward-element)
    (define-key map "\C-m" 'kite--widget-field-activate)
    map)
  "Keymap to use in DOM inspector widget fields.")

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
    (define-key map [mouse-movement] 'kite--dom-mouse-movement)
    map)
  "Local keymap for `kite-dom-mode' buffers.")

(defvar kite--dom-highlighted-node-id)

(defvar kite--dom-pending-set-node-values nil
  "An alist of (NODE-ID . CHANGED-VALUE) that keeps track of
which node value changes have been sent to the remote debugging
server but not acknowledged yet.  `DOM.characterDataModified'
uses this to see which notifications to ignore.")

(defvar kite--dom-pending-set-attribute-values nil
  "A list of (NODE-ID ATTR-NAME CHANGED-VALUE) that keeps track
of which attribute value changes have been sent to the remote
debugging server but not acknowledged yet.
`DOM.attributeModified' uses this to see which notifications to
ignore.")

(defvar kite--dom-setting-remote-value nil
  "Scope variable (should be set with a let binding) that
signifies that a DOM change originated on the remote end.  Used
by nested functions to ensure the change isn't sent back to the
server.")

(defvar kite-dom-node-modified-hooks nil
  "List of functions to call each time a DOM node (that Kite
knows about) is changed in any way.  Each function is called once
for the node that has changed and for each of its
parents (recursively, in bottom-top order), with a
`kite-dom-node' structure as the single argument.

Note that it won't be called when a node was modified that Kite
doesn't yet know about, even if Kite might know about one of its
parents.")

(defvar kite-dom-node-removed-hooks nil
  "List of functions to call when a DOM node (that Kite knows
about) is removed.  Each function is called once for the removed
node's children and for the node itself (recursively, in
bottom-top order), with a `kite-dom-node' structure as the single
argument.

Note that it won't be called when a node was removed that Kite
doesn't yet know about, even if Kite might know about one of its
parents.")

(defun kite--dom-mouse-movement (event)
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
            (let* ((dom-node (kite--dom-node-for-id node-id))
                   (overlay (make-overlay
                             (kite-dom-node-line-begin dom-node)
                             (kite-dom-node-line-end dom-node))))
              (overlay-put overlay 'kite-dom-node-highlight t)
              (overlay-put overlay 'priority 10)
              (overlay-put overlay 'face 'kite-node-highlight-face))
            (kite-send
             "DOM.highlightNode"
             :params
             (list
              :nodeId node-id
              :highlightConfig
              `((showInfo . nil)
                (contentColor . ,(kite--rgba 255 0 0 0.5))
                (paddingColor . ,(kite--rgba 0 255 0 0.5))
                (borderColor . ,(kite--rgba 0 0 255 0.5))
                (marginColor . ,(kite--rgba 255 255 0 0.5)))))))))))

(define-derived-mode kite-dom-mode special-mode "kite-dom"
  "Toggle kite dom mode."
  :group 'kite
  (setq kite-buffer-type 'dom)
  (setq buffer-read-only nil)
  (set (make-local-variable 'kite--dom-highlighted-node-id) nil)
  (set (make-local-variable 'kite--dom-pending-set-node-values) nil)
  (set (make-local-variable 'track-mouse) t)

  (let ((inhibit-read-only t))
    (erase-buffer)
    (remove-overlays)
    (save-excursion
      (when (kite-session-document-root kite-session)
        (kite--dom-insert-document
         (kite-session-document-root kite-session))
        (widget-setup))))

  (run-mode-hooks 'kite-dom-mode-hook)

  (kite-send
   "CSS.enable"
   :success-function
   (lambda (result)
     (kite-send
      "CSS.getAllStyleSheets"
      :success-function
      (lambda (result)
        (kite--log "CSS.getAllStyleSheets got result" result)
        (run-hooks 'kite-async-init-hook))))))

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
          (kite--dom-insert-element
           (or (kite--dom-node-for-id (plist-get html-plist :nodeId))
               (kite--dom-create-node html-plist
                                      nil)))
          (insert "\n"))
      (error "Document doesn't seem to contain html element"))))

(defconst kite-dom-offset 2
  "Number of spaces to use for each DOM indentation level.")

(defun kite--dom-notify-widget (widget &rest ignore)
  "Experimental callback for widget-validate.  Highlight the
widget with `modified-face' to show that its contents have been
edited."
  (let ((modified-face (widget-get widget :modified-value-face)))
    (unless (or (null modified-face)
                (eq (widget-get widget :value-face)
                    modified-face))
      (widget-put widget :value-face modified-face)
      (overlay-put (widget-get widget :field-overlay)
                   'face modified-face))))

(defun kite--dom-validate-widget (widget)
  "Experimental callback for widget-validate."
  (let ((val (widget-apply widget :value-get)))
    (unless (> (length val) 0)
      (widget-put widget :error "too short!")
      widget)))

(defun kite--dom-render-attribute (dom-node dom-attr)
  "Render an attribute of DOM-NODE, the one designated by
DOM-ATTR, at point."
  (setf (kite-dom-attr-outer-begin dom-attr) (point-marker))
  (widget-insert (concat
                  " "
                  (propertize
                   (kite-dom-attr-name dom-attr)
                   'face 'kite-attribute-local-name-face
                   'font-lock-face 'kite-attribute-local-name-face)
                  "="))
  (setf (kite-dom-attr-value-begin dom-attr) (point-marker))
  (setf (kite-dom-attr-value-widget dom-attr)
        (widget-create
         'editable-field
         :size 0
         :format (propertize
                  "\"%v\""
                  'face 'kite-attribute-value-delimiter-face)
         :value-face 'kite-attribute-value-face
         :modified-value-face 'kite-modified-attribute-value-face
         :kite-node-id (kite-dom-node-id dom-node)
         :kite-attr dom-attr
         :keymap kite--dom-widget-field-keymap
         :notify
         (lambda (widget &rest ignore)
           (unless (or kite--dom-setting-remote-value
                       (string=
                        (widget-value widget)
                        (kite-dom-attr-value
                         (widget-get widget :kite-attr))))
             (lexical-let ((lex-widget widget))
               (push
                (list (widget-get widget :kite-node-id)
                      (kite-dom-attr-name
                       (widget-get widget :kite-attr))
                      (widget-value widget))
                kite--dom-pending-set-attribute-values)
               (kite-send
                "DOM.setAttributeValue"
                :params
                (list :nodeId
                      (widget-get widget :kite-node-id)
                      :name
                      (kite-dom-attr-name
                       (widget-get widget :kite-attr))
                      :value
                      (widget-value widget))
                :success-function
                (lambda (result)
                  (setcdr
                   (last kite--dom-pending-set-attribute-values 2)
                   nil))
                :error-function
                (lambda (error-result)
                  (setcdr
                   (last kite--dom-pending-set-attribute-values 2)
                   nil)
                  (kite--default-error-handler error-result))))))

         (kite-dom-attr-value dom-attr)))
  (setf (kite-dom-attr-value-end dom-attr) (point-marker))
  (setf (kite-dom-attr-outer-end dom-attr) (point-marker))

  (put-text-property (kite-dom-attr-outer-begin dom-attr)
                     (kite-dom-attr-outer-end dom-attr)
                     'kite-attr dom-attr))

(defun kite--dom-render-attr-alist (dom-node)
  "Insert all attributes of ELEMENT.  ELEMENT is a plist
representing the element and its attributes, as provided by the
remote debugger.  Return a list of (ATTR-NAME . KITE-DOM-ATTR) cons
cells in the same order as the attributes in the element plist."

  (dolist (dom-attr (mapcar 'cdr
                            (kite-dom-node-attr-alist dom-node)))

    (kite--dom-render-attribute dom-node dom-attr)))

(defun kite--dom-leading-whitespace (node1 node2)
  "Return the whitespace required before NODE2 given that it is
preceded by NODE1.  NODE1 may be nil, which indicates that NODE2
is the first child."
  (cond
   ((eq (kite-dom-node-type node2) kite-dom-element-node)
    (concat
     "\n"
     (make-string (* kite-dom-offset (kite-dom-node-indent node2))
                  32)))
   (t
    "")))

(defun kite--dom-trailing-whitespace (node1 node2)
  "Return the whitespace required after NODE1 given that it is
followed by NODE2.  NODE2 may be nil, which indicates that NODE1
is the last child."
  (cond
   ((or (eq (kite-dom-node-type node1) kite-dom-text-node)
        (and (not (null node2))
             (eq (kite-dom-node-type node2) kite-dom-text-node)))
    "")
   ((null node2)
    (concat
     "\n "
     (make-string (* kite-dom-offset (- (kite-dom-node-indent node1)
                                        1))
                  32)))
   (t
    "")))

(defun kite--dom-update-inner-whitespace (dom-node)
  "For each child, update leading and trailing whitespace."
  (save-excursion
    (let* ((children (kite-dom-node-children dom-node))
           (last-child (- (length children) 1))
           (overlays (overlays-in 0 (buffer-size)))
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
              (delete-region (kite-dom-node-line-begin child)
                             (kite-dom-node-outer-begin child))
              ;; insert leading whitespace
              (goto-char (kite-dom-node-line-begin child))
              (let ((save-point (point)))
                (widget-insert
                 (propertize
                  (kite--dom-leading-whitespace prev-child child)
                  'kite-node-id (kite-dom-node-id child)))
                (set-marker (kite-dom-node-line-begin child)
                            save-point)
                (set-marker (kite-dom-node-outer-begin child)
                            (point)))

              ;; delete any trailing whitespace
              (delete-region (kite-dom-node-outer-end child)
                             (kite-dom-node-line-end child))
              ;; insert trailing whitespace
              (goto-char (kite-dom-node-outer-end child))
              (let ((save-point (point)))
                (widget-insert
                 (propertize
                  (kite--dom-trailing-whitespace child next-child)
                  'kite-node-id (kite-dom-node-id child)))
                (set-marker (kite-dom-node-outer-end child)
                            save-point)
                (set-marker (kite-dom-node-line-end child)
                            (point)))))

      (mapcar*
       (lambda (overlay old-length)
         (kite--log "overlay %s old-length %s" overlay old-length)
         (move-overlay overlay
                       (overlay-start overlay)
                       (+ (overlay-start overlay)
                          old-length)))
       overlays overlay-lengths))))

(defun kite--dom-create-node (element-plist parent)
  "Create a `kite-dom-node' structure with the information from
  the given ELEMENT-PLIST and the given PARENT node, which should
  also be a `kite-dom-node' structure or nil.  Also add the new
  node to the session's `kite-session-dom-nodes' hash map.
  Return the newly created node."
  (let ((dom-node
         (make-kite-dom-node
          :parent parent
          :id (plist-get element-plist :nodeId)
          :type (plist-get element-plist :nodeType)
          :value (plist-get element-plist :nodeValue)
          :child-count (plist-get element-plist :childNodeCount)
          :local-name (plist-get element-plist :localName)
          :indent (if parent (1+ (kite-dom-node-indent parent)) 0)
          :attr-alist
          (let* (attribute-structs
                 (attributes (plist-get element-plist :attributes))
                 (attr-index (- (length attributes) 2)))
            (while (>= attr-index 0)
              (push (cons
                     (elt attributes attr-index)
                     (make-kite-dom-attr
                      :name (elt attributes attr-index)
                      :value (elt attributes (1+ attr-index))))
                    attribute-structs)
              (setq attr-index (- attr-index 2)))
            attribute-structs))))

    (setf (kite-dom-node-children dom-node)
          (mapcar (lambda (child)
                    (kite--dom-create-node child dom-node))
                  (plist-get element-plist :children)))

    (puthash (kite-dom-node-id dom-node)
             dom-node
             (kite-session-dom-nodes kite-session))
    dom-node))

(defun kite--dom-insert-element (dom-node)
  "Render given ELEMENT at point."
  (setf (kite-dom-node-line-begin dom-node) (point-marker))
  (flet
      ((indent-prefix (indent node-id)
                      (propertize
                       (make-string (* kite-dom-offset indent) 32)
                       'kite-node-id node-id
                       'read-only t)))

    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))

      (cond
       ((and (eq (kite-dom-node-type dom-node)
                 kite-dom-element-node)
             (or (eq 0 (kite-dom-node-child-count dom-node))
                 (kite-dom-node-children dom-node)))
        (if (> (kite-dom-node-child-count dom-node) 0)
            (widget-insert (if (kite-dom-node-children dom-node)
                               "-"
                             "+"))
          (widget-insert " "))
        (widget-insert (propertize "<"
                                   'kite-node-id (kite-dom-node-id
                                                  dom-node)
                                   'face 'kite-tag-delimiter-face
                                   'field 'boundary))
        (setf (kite-dom-node-widget dom-node)
              (widget-create
               'editable-field
               :size 1
               :value-face 'kite-element-local-name-face
               :modified-value-face
               'kite-modified-element-local-name-face
               :notify (function kite--dom-notify-widget)
               :validate (function kite--dom-validate-widget)
               :match (lambda (x) (> (length (widget-value x)) 0))
               :kite-node-id (kite-dom-node-id dom-node)
               :keymap kite--dom-widget-field-keymap
               (kite-dom-node-local-name dom-node)))
        (kite--dom-render-attr-alist dom-node)
        (widget-insert
         (concat (propertize ">"
                             'kite-node-id (kite-dom-node-id dom-node)
                             'read-only t
                             'face 'kite-tag-delimiter-face)))
        (setf (kite-dom-node-inner-begin dom-node) (point-marker))
        (put-text-property (kite-dom-node-line-begin dom-node)
                           (kite-dom-node-inner-begin dom-node)
                           'kite-node-id
                           (kite-dom-node-id dom-node))

        (mapc #'kite--dom-insert-element
              (kite-dom-node-children dom-node))

        (setf (kite-dom-node-inner-end dom-node) (point-marker))
        (widget-insert
         (concat
          (propertize "<"
                      'face 'kite-tag-delimiter-face
                      'field 'boundary)
          (propertize "/" 'face 'kite-tag-slash-face)
          (propertize (kite-dom-node-local-name dom-node)
                      'face 'kite-element-local-name-face)
          (propertize ">" 'face 'kite-tag-delimiter-face)))
        (put-text-property (kite-dom-node-inner-end dom-node)
                           (point)
                           'kite-node-id
                           (kite-dom-node-id dom-node)))

       ((eq (kite-dom-node-type dom-node)
            kite-dom-element-node)
        (widget-insert "+")
        (widget-insert (propertize "<"
                                   'face 'kite-tag-delimiter-face
                                   'field 'boundary))
        (setf (kite-dom-node-widget dom-node)
              (widget-create
               'editable-field
               :size 1
               :value-face 'kite-element-local-name-face
               :modified-value-face
               'kite-modified-element-local-name-face
               :notify (function kite--dom-notify-widget)
               :validate (function kite--dom-validate-widget)
               :match (lambda (x) (> (length (widget-value x)) 0))
               :kite-node-id (kite-dom-node-id dom-node)
               :keymap kite--dom-widget-field-keymap
               (kite-dom-node-local-name dom-node)))
        (kite--dom-render-attr-alist dom-node)
        (widget-insert (propertize ">"
                                   'face 'kite-tag-delimiter-face))
        (setf (kite-dom-node-inner-begin dom-node) (point-marker))
        (widget-insert "...")
        (setf (kite-dom-node-inner-end dom-node) (point-marker))
        (widget-insert
         (concat
          (propertize "<"
                      'face 'kite-tag-delimiter-face
                      'field 'boundary)
          (propertize "/" 'face 'kite-tag-slash-face)
          (propertize (kite-dom-node-local-name dom-node)
                      'face 'kite-element-local-name-face)
          (propertize ">" 'face 'kite-tag-delimiter-face)))
        (put-text-property (kite-dom-node-line-begin dom-node)
                           (point)
                           'kite-node-id
                           (kite-dom-node-id dom-node)))

       ((eq (kite-dom-node-type dom-node)
            kite-dom-text-node)
        (setf (kite-dom-node-widget dom-node)
              (widget-create
               'editable-field
               :value-face 'kite-text-face
               :value-create
               (lambda (widget)
                 "Evil hack to avoid newline in editable field."
                 ;; Note: we have to use a :size of nil (rather than
                 ;; 0) because otherwise `widget-after-change' will
                 ;; trim trailing whitespace.

                 ;; Temporarily set field size to 0 because otherwise
                 ;; `widget-field-value-create' will insert a trailing
                 ;; newline
                 (widget-put widget :size 0)
                 (widget-field-value-create widget)
                 (widget-put widget :size nil)

                 ;; Since no trailing newline was inserted the overlay
                 ;; is one character too long, so make it shorter
                 (let ((overlay-end
                        (cdr (widget-get widget :field-overlay))))
                   (move-marker overlay-end
                                (1- (marker-position overlay-end)))))
              :notify
               (lambda (widget child &optional event)
                 (let ((dom-node (kite--dom-node-for-id
                                  (widget-get widget :kite-node-id))))
                   (when (and
                          (or (not (boundp
                                    'kite--dom-setting-remote-value))
                              (not kite--dom-setting-remote-value))
                          (not (string=
                                (widget-value widget)
                                (kite-dom-node-value dom-node))))
                     (push (cons
                            (kite-dom-node-id dom-node)
                            (widget-value widget))
                           kite--dom-pending-set-node-values)
                     (kite-send
                      "DOM.setNodeValue"
                      :params
                      (list :nodeId (widget-get widget
                                                :kite-node-id)
                            :value (widget-value widget))
                      :success-function
                      (lambda (result)
                        (setcdr
                         (last kite--dom-pending-set-node-values 2)
                         nil))
                      :error-function
                      (lambda (error-result)
                        (setcdr
                         (last kite--dom-pending-set-node-values 2)
                         nil)
                        (kite--default-error-handler error-result))))))
               :validate (function kite--dom-validate-widget)
               :kite-node-id (kite-dom-node-id dom-node)
               :keymap kite--dom-widget-field-keymap
               (kite-dom-node-value dom-node))))

       ((eq (kite-dom-node-type dom-node)
            kite-dom-comment-node)
        (widget-insert
         (propertize
          "<!--"
          'face 'kite-comment-delimiter-face
          'field 'boundary))

        (setf (kite-dom-node-widget dom-node)
              (widget-create
               'editable-field
               :size 1
               :value-face 'kite-comment-content-face
               :modified-value-face
               'kite-modified-comment-content-face
               :notify (function kite--dom-notify-widget)
               :validate (function kite--dom-validate-widget)
               :kite-node-id (kite-dom-node-id dom-node)
               :keymap kite--dom-widget-field-keymap
               (kite-dom-node-value dom-node)))
        (widget-insert
         (propertize
          "-->"
          'face 'kite-comment-delimiter-face)
         "\n")
        (put-text-property (kite-dom-node-line-begin dom-node)
                           (point)
                           'kite-node-id
                           (kite-dom-node-id dom-node)))

       (t
        (error "Unknown node type %s" (kite-dom-node-type dom-node))))

      (setf (kite-dom-node-line-end dom-node) (point-marker))

      (setf (kite-dom-node-outer-begin dom-node)
            (copy-marker (kite-dom-node-line-begin dom-node)))
      (setf (kite-dom-node-outer-end dom-node)
            (copy-marker (kite-dom-node-line-end dom-node)))

      (when (eq (kite-dom-node-type dom-node)
                kite-dom-element-node)
        (kite--dom-update-inner-whitespace dom-node)))

    (when (kite-dom-node-inner-begin dom-node)
      (set-marker-insertion-type (kite-dom-node-inner-begin dom-node)
                                 nil)
      (set-marker-insertion-type (kite-dom-node-inner-end dom-node)
                                 t))
    (when (kite-dom-node-outer-begin dom-node)
      (set-marker-insertion-type (kite-dom-node-outer-begin dom-node)
                                 t)
      (set-marker-insertion-type (kite-dom-node-outer-end dom-node)
                                 nil))
    (when (kite-dom-node-line-begin dom-node)
      (set-marker-insertion-type (kite-dom-node-line-begin dom-node)
                                 t)
      (set-marker-insertion-type (kite-dom-node-line-end dom-node)
                                 nil))))

(defun kite--dom-delete-child-widgets (dom-node)
  "Delete all widgets in the dom-node's children,
recursively."
  (dolist (child (kite-dom-node-children dom-node))
    (kite--dom-delete-widgets child)))

(defun kite--dom-delete-widgets (dom-node)
  "Delete the dom-node's widgets and all its children's
widgets, recursively"
  (when (kite-dom-node-widget dom-node)
    (widget-delete (kite-dom-node-widget dom-node))
    (setf (kite-dom-node-widget dom-node)))
  (dolist (dom-attr (mapcar 'cdr
                            (kite-dom-node-attr-alist dom-node)))
    (when (kite-dom-attr-value-widget dom-attr)
      (widget-delete (kite-dom-attr-value-widget dom-attr))
      (setf (kite-dom-attr-value-widget dom-attr))))
  (kite--dom-delete-child-widgets dom-node))

(defun kite--dom-set-element-toggle-char (dom-node char)
  "Set the character denoting whether an element is opened,
closed, or doesn't have any child nodes, for DOM-NODE to
CHARACTER."
  (save-excursion
    (goto-char (kite-dom-node-outer-begin dom-node))
    (forward-char)
    (widget-insert (propertize
                    char
                    'kite-node-id
                    (kite-dom-node-id dom-node)))
    (forward-char -1)
    (delete-char -1)))

(defun kite--dom-show-child-nodes (dom-node)
  "Render the children of the given DOM-NODE."
  (let ((inhibit-read-only t))
    (save-excursion
      (kite--dom-set-element-toggle-char dom-node "-")
      (kite--dom-delete-child-widgets dom-node)
      (delete-region (kite-dom-node-inner-begin dom-node)
                     (kite-dom-node-inner-end dom-node))
      (goto-char (kite-dom-node-inner-begin dom-node))
      (atomic-change-group
        (mapc
         #'kite--dom-insert-element
         (kite-dom-node-children dom-node))))
    (widget-setup)
    (kite--dom-update-inner-whitespace dom-node)))

(defun kite--dom-DOM-setChildNodes (websocket-url packet)
  "Callback invoked for the `DOM.setChildNodes' notification,
which the remote debugger wants to provide us with missing DOM
structure, for example in response to a `DOM.requestChildNodes'
request."
  (kite--log "kite--DOM-setChildNodes got packet %s" packet)

  (let ((kite-session (gethash websocket-url kite-active-sessions))
        (dom-node (kite--dom-node-for-id
                   (plist-get packet :parentId))))
    (kite--dom-delete-child-widgets dom-node)
    (dolist (child (kite-dom-node-children dom-node))
      (kite--dom-node-removed child))
    (setf (kite-dom-node-children dom-node)
          (mapcar
           (lambda (element)
             (kite--dom-create-node element dom-node))
           (plist-get packet :nodes)))
    (let ((dom-buffer (kite--find-buffer websocket-url 'dom)))
      (when dom-buffer
        (with-current-buffer dom-buffer
          (kite--dom-show-child-nodes dom-node))))
    (kite--dom-node-modified dom-node)))

(defun kite--dom-DOM-childNodeInserted (websocket-url packet)
  "Callback invoked for the `DOM.childNodeInserted' notification,
which the remote debugger sends when a script has inserted a
child node into a DOM element."
  (kite--log "kite--DOM-childNodeInserted got packet %s" packet)
  (with-current-buffer (kite--find-buffer websocket-url 'dom)
    (save-excursion
      (let* ((inhibit-read-only t)
             (previous-node-id (plist-get packet :previousNodeId))
             (parent-node-id (plist-get packet :parentNodeId))
             (dom-node (kite--dom-node-for-id parent-node-id))
             (previous-kite-dom-node
              (kite--dom-node-for-id previous-node-id)))

        (kite--dom-set-element-toggle-char dom-node "-")

        (goto-char (if (eq previous-node-id 0)
                       (kite-dom-node-inner-begin dom-node)
                     (kite-dom-node-line-end previous-kite-dom-node)))
        (let ((new-kite-dom-node
               (kite--dom-create-node (plist-get packet :node)
                                      dom-node)))
          (kite--dom-insert-element new-kite-dom-node)
          (if (eq previous-node-id 0)
              (push new-kite-dom-node
                    (kite-dom-node-children dom-node))
            (let ((insert-position
                   (1+ (position
                        previous-kite-dom-node
                        (kite-dom-node-children dom-node)))))
              (setf (kite-dom-node-children dom-node)
                    (append
                     (subseq
                      (kite-dom-node-children dom-node)
                      0
                      insert-position)
                     (list new-kite-dom-node)
                     (subseq
                      (kite-dom-node-children dom-node)
                      insert-position))))))
        (widget-setup)
        (kite--dom-update-inner-whitespace dom-node)
        (kite--dom-node-modified dom-node)))))

(defun kite--dom-DOM-childNodeCountUpdated (websocket-url packet)
  "Callback invoked for the `DOM.childNodeCountUpdated' notification,
which the remote debugger sends when the number of child nodes of
a DOM element has changed."
  (kite--log "kite--DOM-childNodeCountUpdated got packet %s" packet)
  (with-current-buffer (kite--find-buffer websocket-url 'dom)
    (save-excursion
      (let ((inhibit-read-only t)
            (dom-node (kite--dom-node-for-id
                       (plist-get packet :nodeId))))

        (goto-char (1+ (kite-dom-node-outer-begin dom-node)))
        (widget-insert
         (propertize (if (> (plist-get packet :childNodeCount) 0)
                         "+"
                       " ")
                     'kite-node-id
                     (kite-dom-node-id dom-node)))
        (forward-char -1)
        (delete-char -1)

        (cond
         ((and (null (kite-dom-node-children dom-node))
               (> (plist-get packet :childNodeCount) 0))
          ;; No children yet and now children added
          (goto-char (kite-dom-node-inner-begin dom-node))
          (widget-insert "..."))
         ((eq (plist-get packet :childNodeCount) 0)
          (delete-region
           (kite-dom-node-inner-begin dom-node)
           (kite-dom-node-inner-end dom-node))))
        (kite--dom-node-modified dom-node)))))

(defun kite--dom-DOM-childNodeRemoved (websocket-url packet)
  "Callback invoked for the `DOM.childNodeRemoved' notification,
which the remote debugger sends when a script has removed a child
node from a DOM element."
  (kite--log "kite--DOM-childNodeRemoved got packet %s" packet)
  (with-current-buffer (kite--find-buffer websocket-url 'dom)
    (save-excursion
      (let ((inhibit-read-only t)
            (dom-node (kite--dom-node-for-id
                       (plist-get packet :nodeId))))
        (kite--dom-node-removed dom-node)
        (kite--dom-delete-widgets dom-node)
        (delete-region
         (kite-dom-node-line-begin dom-node)
         (kite-dom-node-line-end dom-node))
        (setf (kite-dom-node-children (kite-dom-node-parent dom-node))
              (delete dom-node
                      (kite-dom-node-children
                       (kite-dom-node-parent dom-node))))
        (when (eq 0 (length (kite-dom-node-children
                             (kite-dom-node-parent dom-node))))
          (kite--dom-set-element-toggle-char dom-node " "))

        (kite--dom-update-inner-whitespace
         (kite-dom-node-parent dom-node))
        (widget-setup)
        (kite--dom-node-modified
         (kite-dom-node-parent dom-node))))))

(defun kite--dom-DOM-attributeModified (websocket-url packet)
  "Callback invoked for the `DOM.attributeModified' notification,
which the remote debugger sends when a script has modified the
value of an attribute in a DOM element."
  (kite--log "kite--DOM-attributeModified got packet %s" packet)
  (with-current-buffer (kite--find-buffer websocket-url 'dom)
    (save-excursion
      (let* ((inhibit-read-only t)
             (node-id (plist-get packet :nodeId))
             (attr-name (plist-get packet :name))
             (dom-node (kite--dom-node-for-id node-id))
             (dom-attr
              (cdr (assoc attr-name
                          (kite-dom-node-attr-alist dom-node)))))
        (if dom-attr
            ;; Modify existing attribute
            (progn
              (let ((kite--dom-setting-remote-value t)
                    (inhibit-read-only t))
                (when (not (member
                            (list node-id
                                  attr-name
                                  (plist-get packet :value))
                            kite--dom-pending-set-attribute-values))
                  (widget-value-set
                   (kite-dom-attr-value-widget dom-attr)
                   (plist-get packet :value)))
                (setf (kite-dom-attr-value dom-attr)
                      (plist-get packet :value))))
          ;; Insert new attribute
          (goto-char
           (if (kite-dom-node-attr-alist dom-node)
               (kite-dom-attr-value-end
                (cdar (kite-dom-node-attr-alist dom-node)))
             (1- (kite-dom-node-inner-begin dom-node))))

          (let ((dom-attr (make-kite-dom-attr
                           :name (plist-get packet :name)
                           :value (plist-get packet :value))))
            (push (cons (plist-get packet :name) dom-attr)
                  (kite-dom-node-attr-alist dom-node))
            (kite--dom-render-attribute dom-node dom-attr)))
        (widget-setup)
        (kite--dom-node-modified dom-node)))))

(defun kite--dom-DOM-attributeRemoved (websocket-url packet)
  "Callback invoked for the `DOM.attributeRemoved' notification,
which the remote debugger sends when a script has removed an
attribute from a DOM element."
  (kite--log "kite--DOM-attributeRemoved got packet %s" packet)
  (with-current-buffer (kite--find-buffer websocket-url 'dom)
    (save-excursion
      (let* ((inhibit-read-only t)
             (attr-name (plist-get packet :name))
             (dom-node (kite--dom-node-for-id
                        (plist-get packet :nodeId)))
             (dom-attr
              (cdr (assoc attr-name
                          (kite-dom-node-attr-alist dom-node)))))
        (kite--log "kite--DOM-attributeRemoved, kite-dom-attr=%s (%s)"
                   dom-attr
                   (kite-dom-node-attr-alist dom-node))
        (widget-delete (kite-dom-attr-value-widget dom-attr))
        (delete-region (kite-dom-attr-outer-begin dom-attr)
                       (kite-dom-attr-outer-end dom-attr))
        (setf (kite-dom-node-attr-alist dom-node)
              (delete-if
               (lambda (item)
                 (equal (car item) attr-name))
               (kite-dom-node-attr-alist dom-node)))
        (kite--dom-node-modified dom-node)))))

(defun kite--dom-DOM-characterDataModified (websocket-url packet)
  "Callback invoked for the `DOM.characterDataModified' notification,
which the remote debugger sends when a script has changed the
contents of a text node."
  (with-current-buffer (kite--find-buffer websocket-url 'dom)
    (save-excursion
      (let* ((dom-node (kite--dom-node-for-id
                        (plist-get packet :nodeId)))
             (widget (kite-dom-node-widget dom-node))
             (kite--dom-setting-remote-value t)
             (inhibit-read-only t))
        (unless (member (cons
                         (plist-get packet :nodeId)
                         (plist-get packet :characterData))
                        kite--dom-pending-set-node-values)

          (setf (kite-dom-node-value dom-node)
                (plist-get packet :characterData))
          (when (not (string=
                      (widget-value widget)
                      (plist-get packet :characterData)))
            (widget-value-set
             (kite-dom-node-widget dom-node)
             (plist-get packet :characterData))))
        (kite--dom-node-modified dom-node)))))

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
  (let* ((dom-node (kite--dom-kite-dom-node-at-point))
         (parent-children (kite-dom-node-children
                           (kite-dom-node-parent dom-node)))
         (node-position (position dom-node parent-children))
         (new-position (max 0 (min (+ node-position arg)
                                   (- (length parent-children) 1))))
         (next-kite-dom-node (nth new-position parent-children)))
    (goto-char (1+ (kite-dom-node-outer-begin next-kite-dom-node)))))

(defun kite-dom-backward-element (&optional arg)
  "Move backward over one element.
With ARG, do it that many times.
Negative ARG means move forward."
  (interactive "p")
  (kite-dom-forward-element (- arg)))

(defun kite-dom-backward-paragraph (&optional arg)
  "Move backward to start of paragraph.
With argument ARG, do it ARG times;
a negative argument ARG = -N means move forward N paragraphs."
  (interactive "p")
  t)

(defun kite-dom-forward-paragraph (&optional arg)
  "Move forward to end of paragraph.
With argument ARG, do it ARG times;
a negative argument ARG = -N means move backward N paragraphs."
  (interactive "p")
  t)

(defun kite-dom-mark-paragraph (&optional arg)
  "Put point at beginning of this paragraph, mark at end.
The paragraph marked is the one that contains point or follows point.

FIXME: not yet implemented."
  (interactive "p")
  t)

(defun kite-dom-highlight-node ()
  "Highlight the node at point in the browser by sending the
`DOM.highlightNode' message to the remote debugger."
  (interactive)
  (kite-send "DOM.highlightNode"
             :params
             (list :nodeId (get-char-property (point) 'kite-node-id)
                   :highlightConfig
                   `((showInfo . nil)
                     (contentColor . ,(kite--rgba 255 0 0 0.5))
                     (paddingColor . ,(kite--rgba 0 255 0 0.5))
                     (borderColor . ,(kite--rgba 0 0 255 0.5))
                     (marginColor . ,(kite--rgba 255 255 0 0.5))))))

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
             :params
             (list :enabled t
                   :highlightConfig
                   `((showInfo . nil)
                     (contentColor . ,(kite--rgba 255 0 0 0.5))
                     (paddingColor . ,(kite--rgba 0 255 0 0.5))
                     (borderColor . ,(kite--rgba 0 0 255 0.5))
                     (marginColor . ,(kite--rgba 255 255 0 0.5))))
             :success-function
             (lambda (result)
               (message kite--dom-pick-node-message))))

(defun kite--dom-Inspector-inspect (websocket-url packet)
  "Callback invoked for the `Inspector.inspect' notification,
which the remote debugger sends when the user selects an element
with the mouse.  Invokes `kite-dom-goto-node' for the element in
question."
  (lexical-let ((websocket-url websocket-url))
    (kite-send "DOM.requestNode"
               :params
               (list :objectId (kite--get packet :object :objectId))
               :success-function
               (lambda (result)
                 (with-current-buffer
                     (kite--find-buffer websocket-url 'dom)
                   (kite-dom-goto-node
                    (plist-get result :nodeId))
                   (when (string= (current-message)
                                  kite--dom-pick-node-message)
                     (message nil)))))))

(defun kite--dom-node-for-id (node-id)
  "Return the `kite-dom-node' for the given NODE-ID."
  (gethash node-id
   (kite-session-dom-nodes kite-session)))

(defun kite--dom-node-at-point (&optional arg)
  "Return the `nodeId' for node at point."
  (interactive)
  (let ((point-arg (or arg (point))))
    (or (get-text-property point-arg 'kite-node-id)
        (let ((widget (widget-at point-arg)))
          (when widget
            (widget-get widget :kite-node-id))))))

(defun kite--dom-kite-dom-node-at-point (&optional arg)
  "Return then node region for node at point."
  (interactive)
  (kite--dom-node-for-id (kite--dom-node-at-point arg)))

(defun kite-dom-delete-node-or-attribute ()
  "Delete node or attribute at point."
  (interactive)
  (let ((node-id (kite--dom-node-at-point)))
    (when node-id
      (let ((attr (get-text-property (point) 'kite-attr)))
        (if attr
            (kite-send "DOM.removeAttribute"
                       :params
                       (list :nodeId node-id
                             :name (kite-dom-attr-name attr)))
          (kite-send "DOM.removeNode"
                     :params
                     (list :nodeId node-id)))))))

(defun kite-dom-narrow-to-node (&optional arg)
  "Narrow buffer to node at point."
  (interactive)
  (let ((node-id (kite--dom-node-at-point)))
    (when node-id
      (let ((dom-node (kite--dom-node-for-id node-id)))
        (narrow-to-region
         (kite-dom-node-line-begin dom-node)
         (kite-dom-node-line-end dom-node))))))

(defun kite--dom-show-element-children (dom-node)
  "Render the children of the given DOM-NODE if they have been
received from the remote debugger before.  Otherwise, request the
children to be sent."
  (when (> (kite-dom-node-child-count dom-node) 0)
    (if (kite-dom-node-children dom-node)
        (kite--dom-show-child-nodes dom-node)
      (kite-send
       "DOM.requestChildNodes"
       :params
       (list :nodeId (kite-dom-node-id dom-node))))))

(defun kite-dom-toggle-node ()
  "Toggle visibility of contents of node at point."
  (interactive)
  (let ((dom-node (kite--dom-kite-dom-node-at-point)))
    (when (> (kite-dom-node-child-count dom-node) 0)
      (if (not (kite-dom-node-openp dom-node))
          (progn
            (setf (kite-dom-node-openp dom-node) t)
            (kite--dom-show-element-children dom-node))
        (save-excursion
          (kite--dom-delete-child-widgets dom-node)
          (setf (kite-dom-node-openp dom-node))
          (let ((inhibit-read-only t))
            (goto-char (kite-dom-node-inner-begin dom-node))
            (insert "...")
            (delete-region (point)
                           (kite-dom-node-inner-end dom-node))
            (goto-char (1+ (kite-dom-node-outer-begin dom-node)))
            (widget-insert
             (propertize "+"
                         'kite-node-id
                         (kite-dom-node-id dom-node)))
            (forward-char -1)
            (delete-char -1)))))))

(defun kite--dom-ensure-node-visible (dom-node)
  "Ensure that node with given NODE-ID is visible.
First (recursively) ensures that any parent nodes are visible."
  (when (kite-dom-node-parent dom-node)
    (kite--dom-ensure-node-visible (kite-dom-node-parent
                                    dom-node)))

  (unless (kite-dom-node-openp (kite-dom-node-parent
                                dom-node))
    (kite--dom-show-element-children
     (kite-dom-node-parent dom-node))))

(defun kite-dom-goto-node (node-id)
  "Ensure that node with given NODE-ID is visible and move point
to its beginning."
  (let ((dom-node (kite--dom-node-for-id node-id)))
    (kite--dom-ensure-node-visible dom-node)
    (goto-char (1+ (kite-dom-node-outer-begin dom-node)))))

(defun kite--dom-initialize ()
  "Reset internal DOM state.  If the DOM buffer is open, insert
the document into it."
  (clrhash (kite-session-dom-nodes kite-session))
  (setf (kite-session-document-root kite-session))
  (kite-send "DOM.getDocument"
             :success-function
             (lambda (result)
               (setf (kite-session-document-root kite-session)
                     (kite--get result :root))
               (let ((dom-buffer
                      (kite--find-buffer
                       (websocket-url
                        (kite-session-websocket kite-session))
                       'dom)))
                 (when dom-buffer
                   (with-current-buffer dom-buffer
                     (save-excursion
                       (let ((inhibit-read-only t))
                         (mapc 'widget-delete widget-field-list)
                         (erase-buffer)
                         (remove-overlays)
                         (kite--dom-insert-document
                          (kite--get result :root))
                         (widget-setup))))))
               (kite--log "DOM initialized."))))

(defun kite--dom-DOM-documentUpdated (websocket-url packet)
  "Responds to the `DOM.documentUpdated' notification by
re-requesting the document."
  (kite--dom-initialize))

(defun kite--dom-node-modified (dom-node)
  "The given DOM-NODE has been modified (and, implicitly, all of
its parents).  Invokes `kite-dom-node-modified-hooks' for the
node and its parents (in that order, recursively.)."
  (run-hook-with-args 'kite-dom-node-modified-hooks dom-node)
  (when (kite-dom-node-parent dom-node)
    (kite--dom-node-modified (kite-dom-node-parent dom-node))))

(defun kite--dom-node-removed (dom-node)
  "The given DOM-NODE has been removed (and, implicitly, all of
its children).  Invokes `kite-dom-node-removed-hooks' for the
node's children and the node (in that order, recursively.)"
  (dolist (child (kite-dom-node-children dom-node))
    (kite--dom-node-removed child))
  (run-hook-with-args 'kite-dom-node-removed-hooks dom-node))

(add-hook 'kite-DOM-attributeModified-hooks
          'kite--dom-DOM-attributeModified)
(add-hook 'kite-DOM-attributeRemoved-hooks
          'kite--dom-DOM-attributeRemoved)
(add-hook 'kite-DOM-childNodeCountUpdated-hooks
          'kite--dom-DOM-childNodeCountUpdated)
(add-hook 'kite-DOM-childNodeInserted-hooks
          'kite--dom-DOM-childNodeInserted)
(add-hook 'kite-DOM-childNodeRemoved-hooks
          'kite--dom-DOM-childNodeRemoved)
(add-hook 'kite-DOM-setChildNodes-hooks
          'kite--dom-DOM-setChildNodes)
(add-hook 'kite-DOM-documentUpdated-hooks
          'kite--dom-DOM-documentUpdated)
(add-hook 'kite-DOM-characterDataModified-hooks
          'kite--dom-DOM-characterDataModified)
(add-hook 'kite-Inspector-inspect-hooks
          'kite--dom-Inspector-inspect)

(provide 'kite-dom)

;;; kite-dom.el ends here
