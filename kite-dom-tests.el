(defun --kite-equal-wildcard (o1 o2)
  (or (eq '* o1)
      (eq '* o2)
      (eq o1 o2)
      (and (listp o1)
           (listp o2)
           (--kite-equal-wildcard (car o1) (car o2))
           (--kite-equal-wildcard (cdr o1) (cdr o2)))
      (and (stringp o1)
           (stringp o2)
           (string= o1 o2))
      (and (numberp o1)
           (numberp o2)
           (= o1 o2))))

(defconst simple-element
  '((attributes . [])
    (children . [((attributes . [])
                  (childNodeCount . 1)
                  (nodeValue . nil)
                  (localName . "head")
                  (nodeName . "HEAD")
                  (nodeType . 1)
                  (nodeId . 89)
                  (children . [((attributes . ["href" "foo"])
                                (childNodeCount . 0)
                                (nodeValue . nil)
                                (localName . "link")
                                (nodeName . "LINK")
                                (nodeType . 1)
                                (nodeId . 87))]))

                 ((attributes . [])
                  (childNodeCount . 0)
                  (nodeValue . nil)
                  (localName . "body")
                  (nodeName . "BODY")
                  (nodeType . 1)
                  (nodeId . 90))])

    (childNodeCount . 2)
    (nodeValue . nil)
    (localName . "html")
    (nodeName . "HTML")
    (nodeType . 1)
    (nodeId . 88)))

(ert-deftest kite-test-dom-simple ()
  "A very simple HTML document is rendered correctly"

  (with-temp-buffer
    (kite-dom-mode)

    (let ((inhibit-read-only t))
      (--kite-dom-insert-element simple-element 0 nil)

      (should (string= (buffer-substring-no-properties (point-min)
                                                       (point-max))
                       (concat
                        "<html>\n"
                        "  <head>\n"
                        "    <link href=\"foo\">\n"
                        "    </link>\n"
                        "  </head>\n"
                        "  <body>\n"
                        "  </body>\n"
                        "</html>\n"))))))



(ert-deftest kite-test-dom-textnodes ()
  "A simple document with text nodes is rendered correctly"

  (with-temp-buffer
    (kite-dom-mode)

    (let ((inhibit-read-only t))
      (--kite-dom-insert-element
       '((attributes . [])
         (childNodeCount . 2)
         (nodeValue . nil)
         (localName . "dummy")
         (nodeName . "DUMMY")
         (nodeType . 1)
         (nodeId . 1)
         (children . [((attributes . [])
                       (childNodeCount . 1)
                       (nodeValue . nil)
                       (localName . "child1")
                       (nodeName . "child1")
                       (nodeType . 1)
                       (nodeId . 2)
                       (children . [((attributes . [])
                                     (childNodeCount . 0)
                                     (nodeValue . "test")
                                     (nodeType . 3)
                                     (nodeId . 3))]))
                      ((attributes . [])
                       (childNodeCount . 1)
                       (nodeValue . nil)
                       (localName . "child2")
                       (nodeName . "child2")
                       (nodeType . 1)
                       (nodeId . 4)
                       (children . [((attributes . [])
                                     (childNodeCount . 0)
                                     (nodeValue . "foobar")
                                     (nodeType . 3)
                                     (nodeId . 5))]))]))
       0 nil)

      (should (string= (buffer-substring-no-properties (point-min)
                                                       (point-max))
                       (concat
                        "<dummy>\n"
                        "  <child1>\n"
                        "    test\n"
                        "  </child1>\n"
                        "  <child2>\n"
                        "    foobar\n"
                        "  </child2>\n"
                        "</dummy>\n"))))))

(ert-deftest kite-test-dom-children-not-loaded ()
  "A node whose children aren't loaded is rendered correctly"

  (with-temp-buffer
    (kite-dom-mode)

    (flet ((--kite-send (command params callback)
                        (should (string= command "DOM.requestChildNodes"))
                        (should (equal params '((nodeId . 2))))))

      (let ((inhibit-read-only t))
        (--kite-dom-insert-element
         '((attributes . [])
           (childNodeCount . 2)
           (nodeValue . nil)
           (localName . "empty")
           (nodeName . "EMPTY")
           (nodeType . 1)
           (nodeId . 2))
         0 t))

      (should (string= (buffer-substring-no-properties (point-min)
                                                       (point-max))
                       "<empty>...</empty>\n"))

      (flet ((--kite-dom-buffer (websocket-url) (current-buffer)))
        (--kite-DOM-setChildNodes nil
                                  '((parentId . 2)
                                    (nodes . [((attributes . [])
                                               (childNodeCount . 0)
                                               (nodeValue . nil)
                                               (localName . "foo")
                                               (nodeName . "FOO")
                                               (nodeType . 1)
                                               (nodeId . 2))]))))

      (should (string= (buffer-substring-no-properties (point-min)
                                                       (point-max))
                       (concat "<empty>\n"
                               "  <foo>\n"
                               "  </foo>\n"
                               "</empty>\n"))))))


(ert-deftest kite-test-dom-insert-only ()
  "DOM is mutated correctly when node is inserted in front"

  (with-temp-buffer
    (kite-dom-mode)
    (let ((inhibit-read-only t))

      (--kite-dom-insert-element simple-element 0 nil)

      (flet ((--kite-dom-buffer (websocket-url) (current-buffer)))

        (--kite-DOM-childNodeInserted "dummy"
                                      '((node (attributes . [])
                                              (childNodeCount . 0)
                                              (nodeValue . nil)
                                              (localName . "div")
                                              (nodeName . "DIV")
                                              (nodeType . 1)
                                              (nodeId . 100))
                                        (previousNodeId . 0)
                                        (parentNodeId . 90)))

        (should (string= (buffer-substring-no-properties (point-min)
                                                         (point-max))
                         (concat
                          "<html>\n"
                          "  <head>\n"
                          "    <link href=\"foo\">\n"
                          "    </link>\n"
                          "  </head>\n"
                          "  <body>\n"
                          "    <div>\n"
                          "    </div>\n"
                          "  </body>\n"
                          "</html>\n")))))))

(ert-deftest kite-test-dom-insert-before ()
  "DOM is mutated correctly when node is inserted before another node"

  (with-temp-buffer
    (kite-dom-mode)
    (let ((inhibit-read-only t))

      (--kite-dom-insert-element simple-element 0 nil)

      (flet ((--kite-dom-buffer (websocket-url) (current-buffer)))

        (--kite-DOM-childNodeInserted "dummy"
                                      '((node (attributes . [])
                                              (childNodeCount . 0)
                                              (nodeValue . nil)
                                              (localName . "div")
                                              (nodeName . "DIV")
                                              (nodeType . 1)
                                              (nodeId . 100))
                                        (previousNodeId . 0)
                                        (parentNodeId . 89)))))

    (should (string= (buffer-substring-no-properties (point-min)
                                                     (point-max))
                     (concat
                      "<html>\n"
                      "  <head>\n"
                      "    <div>\n"
                      "    </div>\n"
                      "    <link href=\"foo\">\n"
                      "    </link>\n"
                      "  </head>\n"
                      "  <body>\n"
                      "  </body>\n"
                      "</html>\n")))))

(ert-deftest kite-test-dom-insert-after ()
  "DOM is mutated correctly when node is inserted after another node"

  (with-temp-buffer
    (kite-dom-mode)
    (let ((inhibit-read-only t))

      (--kite-dom-insert-element simple-element 0 nil)

      (flet ((--kite-dom-buffer (websocket-url) (current-buffer)))

        (--kite-DOM-childNodeInserted "dummy"
                                      '((node (attributes . [])
                                              (childNodeCount . 0)
                                              (nodeValue . nil)
                                              (localName . "div")
                                              (nodeName . "DIV")
                                              (nodeType . 1)
                                              (nodeId . 100))
                                        (previousNodeId . 87)
                                        (parentNodeId . 89)))))

    (should (string= (buffer-substring-no-properties (point-min)
                                                     (point-max))
                     (concat
                      "<html>\n"
                      "  <head>\n"
                      "    <link href=\"foo\">\n"
                      "    </link>\n"
                      "    <div>\n"
                      "    </div>\n"
                      "  </head>\n"
                      "  <body>\n"
                      "  </body>\n"
                      "</html>\n")))))

(ert-deftest kite-test-dom-remove ()
  "DOM is mutated correctly when node is removed"

  (with-temp-buffer
    (kite-dom-mode)
    (let ((inhibit-read-only t))

      (--kite-dom-insert-element simple-element 0 nil)

      (flet ((--kite-dom-buffer (websocket-url) (current-buffer)))

        (--kite-DOM-childNodeRemoved "dummy"
                                     '((nodeId . 89) (parentNodeId . 88)))))

    (should (string= (buffer-substring-no-properties (point-min)
                                                     (point-max))
                     (concat
                      "<html>\n"
                      "  <body>\n"
                      "  </body>\n"
                      "</html>\n")))))

(ert-deftest kite-test-dom-modify-attribute ()
  "DOM is mutated correctly when attribute is modified"

  (with-temp-buffer
    (kite-dom-mode)
    (let ((inhibit-read-only t))

      (--kite-dom-insert-element simple-element 0 nil)

      (flet ((--kite-dom-buffer (websocket-url) (current-buffer)))

        (--kite-DOM-attributeModified "dummy"
                                      '((value . "frobnicate") (name . "href") (nodeId . 87)))))

    (should (string= (buffer-substring-no-properties (point-min)
                                                     (point-max))
                     (concat
                      "<html>\n"
                      "  <head>\n"
                      "    <link href=\"frobnicate\">\n"
                      "    </link>\n"
                      "  </head>\n"
                      "  <body>\n"
                      "  </body>\n"
                      "</html>\n")))
    (let* ((node-info (gethash 87 kite-dom-nodes))
           (attr-info (cdr (assoc 'href (node-region-attribute-regions node-info)))))

      (should (eq (length "\"frobnicate\"")
                  (- (marker-position (attr-region-value-end attr-info))
                     (marker-position (attr-region-value-begin attr-info))))))))

(ert-deftest kite-test-dom-add-attribute ()
  "DOM is mutated correctly when attribute is modified"

  (with-temp-buffer
    (kite-dom-mode)
    (let ((inhibit-read-only t))

      (--kite-dom-insert-element simple-element 0 nil)

      (flet ((--kite-dom-buffer (websocket-url) (current-buffer)))

        (--kite-DOM-attributeModified "dummy"
                                      '((value . "bar") (name . "baz") (nodeId . 87)))))

    (should (string= (buffer-substring-no-properties (point-min)
                                                     (point-max))
                     (concat
                      "<html>\n"
                      "  <head>\n"
                      "    <link href=\"foo\" baz=\"bar\">\n"
                      "    </link>\n"
                      "  </head>\n"
                      "  <body>\n"
                      "  </body>\n"
                      "</html>\n")))

    (let* ((node-info (gethash 87 kite-dom-nodes))
           (attr-info (cdr (assq 'baz (node-region-attribute-regions node-info)))))
      (should (not (null attr-info))))))

(ert-deftest kite-test-dom-remove-attribute ()
  "DOM is mutated correctly when attribute is removed"

  (with-temp-buffer
    (kite-dom-mode)
    (let ((inhibit-read-only t))

      (--kite-dom-insert-element simple-element 0 nil)

      (flet ((--kite-dom-buffer (websocket-url) (current-buffer)))

        (--kite-DOM-attributeRemoved "dummy"
                                      '((name . "href") (nodeId . 87)))))

    (should (string= (buffer-substring-no-properties (point-min)
                                                     (point-max))
                     (concat
                      "<html>\n"
                      "  <head>\n"
                      "    <link>\n"
                      "    </link>\n"
                      "  </head>\n"
                      "  <body>\n"
                      "  </body>\n"
                      "</html>\n")))

    (let* ((node-info (gethash 87 kite-dom-nodes))
           (attr-info (cdr (assq 'href (node-region-attribute-regions node-info)))))
      (should (null attr-info)))))

(ert-deftest kite-test-rgba ()
  "--kite-rgba works as intended"

  (should (equal (--kite-rgba 1 2 3 4)
                 '((r . 1)
                   (g . 2)
                   (b . 3)
                   (a . 4))))

  (should (equal (--kite-rgba 4 3 2 1)
                 '((r . 4)
                   (g . 3)
                   (b . 2)
                   (a . 1)))))

(ert-deftest kite-dimmed-face-foreground ()
  "--kite-dimmed-face-foreground works as intended"
  (unwind-protect
      (progn
        (defface ert-kite-test-face
          '((t :background "#111111"
               :foreground "#333333"))
          "test face 1")

        (should (equal
                 (color-name-to-rgb
                  (--kite-dimmed-face-foreground
                   'ert-kite-test-face
                   0.5))

                 (mapcar* (lambda (x y)
                            (/ (+ x y ) 2))
                          (color-name-to-rgb "#333333")
                          (color-name-to-rgb "#111111")))))

    (put 'ert-kite-test-face 'face-defface-spec nil)))

(ert-deftest kite-dom-hide-highlight ()
  "kite-dom-hide-highlight sends message to server"
  (flet ((--kite-send (command &optional params callback)
                      (should (string= command "DOM.hideHighlight"))))
    (kite-dom-hide-highlight)))

(ert-deftest kite-dom-highlight-node ()
  "kite-dom-hide-highlight sends message to server"

  (with-temp-buffer
    (kite-dom-mode)
    (let ((inhibit-read-only t))
      (--kite-dom-insert-element simple-element 0 nil))
    (flet ((--kite-send (command params callback)
                        (should (string= command "DOM.highlightNode"))
                        (should (--kite-equal-wildcard
                                 params
                                 '((nodeId . 88)
                                   (highlightConfig
                                    . ((showInfo . nil)
                                       (contentColor . ((r . *) (g . *) (b . *) (a . *)))
                                       (paddingColor . ((r . *) (g . *) (b . *) (a . *)))
                                       (borderColor . ((r . *) (g . *) (b . *) (a . *)))
                                       (marginColor . ((r . *) (g . *) (b . *) (a . *))))))))))
      (goto-char (point-min))
      (kite-dom-highlight-node))))

(ert-deftest kite-test-dom-inspect ()
  (let (sent-packets)
    (with-temp-buffer
      (flet ((--kite-dom-buffer (websocket-url) (current-buffer))
             (--kite-websocket-url () t)
             (--kite-send (command params callback)
                          (setq sent-packets (cons (list command params callback)
                                                   sent-packets))))
        (kite-dom-inspect)))
    (should (--kite-equal-wildcard sent-packets
                                   '(("DOM.getDocument" nil *)
                                     ("CSS.enable" nil *))))))

(defun kite-run-tests ()
  (interactive)
  (ert-run-tests-interactively "^kite-"))

(defun kite-run-coverage ()
  (interactive)
  (testcover-start "kite-dom.el")
  (ert-run-tests-interactively "^kite-"))

(provide 'kite-dom-tests)
