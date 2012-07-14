(defun kite-test--should-send-packets (body expected-packets &optional response-function)
  (let ((response-function* (or response-function
                                (lambda (command params)
                                  '(ok))))
        sent-packets)
    (flet ((kite-send (command params callback)
                      (setq sent-packets (cons (list command params callback)
                                               sent-packets))
                      (when callback
                        (funcall callback (funcall response-function* command params)))))
      (funcall body))
    (should (kite--equal-wildcard sent-packets
                                   expected-packets))))

(ert-deftest kite-next-instruction-breakpoint-test ()
  "Test that make-kite-next-instruction-breakpoint works"

  ;; Test to-string function

  (should (string=
           (kite--breakpoint-to-string
            (make-kite-next-instruction-breakpoint))
           "next instruction"))

  ;; Test setting breakpoints

  (kite-test--should-send-packets
   (lambda ()
     (kite--set-breakpoint
      (make-kite-next-instruction-breakpoint) nil))
   '(("Debugger.pause" nil nil)))

  ;; Test removing breakpoints

  (kite-test--should-send-packets
   (lambda ()
     (kite--remove-breakpoint
      (make-kite-next-instruction-breakpoint) nil))
   '(("Debugger.resume" nil nil))))


(ert-deftest kite-all-exceptions-breakpoint-test ()
  "Test that make-kite-all-exceptions-breakpoint works"

  ;; Test to-string function

  (message "result %s" (make-kite-all-exceptions-breakpoint))

  (should (string=
           (kite--breakpoint-to-string
            (make-kite-all-exceptions-breakpoint))
           "all exceptions"))

  ;; Test setting breakpoints

  (kite-test--should-send-packets
   (lambda ()
     (kite--set-breakpoint
      (make-kite-all-exceptions-breakpoint) nil))
   '(("Debugger.setPauseOnExceptions"
      ((state . "all"))
      nil)))

  ;; Test removing breakpoints

  (kite-test--should-send-packets
   (lambda ()
     (kite--remove-breakpoint
      (make-kite-all-exceptions-breakpoint) nil))
   '(("Debugger.setPauseOnExceptions"
      ((state . "none"))
      nil))))

(ert-deftest kite-uncaught-exceptions-breakpoint-test ()
  "Test that make-kite-uncaught-exceptions-breakpoint works"

  ;; Test to-string function

  (should (string=
           (kite--breakpoint-to-string
            (make-kite-uncaught-exceptions-breakpoint))
           "uncaught exceptions"))

  ;; Test setting breakpoints

  (kite-test--should-send-packets
   (lambda ()
     (kite--set-breakpoint
      (make-kite-uncaught-exceptions-breakpoint) nil))
   '(("Debugger.setPauseOnExceptions"
      ((state . "uncaught"))
      nil)))

  ;; Test removing breakpoints

  (kite-test--should-send-packets
   (lambda ()
     (kite--remove-breakpoint
      (make-kite-uncaught-exceptions-breakpoint) nil))
   '(("Debugger.setPauseOnExceptions"
      ((state . "none"))
      nil))))

(ert-deftest kite-dom-node-breakpoint-test ()
  "Test that make-kite-dom-node-breakpoint work"

  ;; Test to-string function

  (should (string=
           (kite--breakpoint-to-string
            (make-kite-dom-node-breakpoint
             :node-id 1234
             :type 'subtree-modified))
           "modification of subtree of node #1234"))

  (should (string=
           (kite--breakpoint-to-string
            (make-kite-dom-node-breakpoint
             :node-id 2345
             :type 'attribute-modified))
           "modification of attribute of node #2345"))

  (should (string=
           (kite--breakpoint-to-string
            (make-kite-dom-node-breakpoint
             :node-id 3456
             :type 'node-removed))
           "removal of node #3456"))

  ;; Test setting breakpoints

  (kite-test--should-send-packets
   (lambda ()
     (kite--set-breakpoint
      (make-kite-dom-node-breakpoint
       :node-id 4567
       :type 'node-setd)
      nil))
   '(("DOM.setDOMBreakpoint"
      ((nodeId . 4567)
       (type . node-setd))
      nil)))

  (kite-test--should-send-packets
   (lambda ()
     (kite--set-breakpoint
      (make-kite-dom-node-breakpoint
       :node-id 5678
       :type 'attribute-modified)
      nil))
   '(("DOM.setDOMBreakpoint"
      ((nodeId . 5678)
       (type . attribute-modified))
      nil)))

  (kite-test--should-send-packets
   (lambda ()
     (kite--set-breakpoint
      (make-kite-dom-node-breakpoint
       :node-id 6789
       :type 'subtree-modified)
      nil))
   '(("DOM.setDOMBreakpoint"
      ((nodeId . 6789)
       (type . subtree-modified))
      nil)))

  ;; Test removing breakpoints

  (kite-test--should-send-packets
   (lambda ()
     (kite--remove-breakpoint
      (make-kite-dom-node-breakpoint
       :node-id 4567
       :type 'node-removed)
      nil))
   '(("DOM.removeDOMBreakpoint"
      ((nodeId . 4567)
       (type . node-removed))
      nil)))

  (kite-test--should-send-packets
   (lambda ()
     (kite--remove-breakpoint
      (make-kite-dom-node-breakpoint
       :node-id 5678
       :type 'attribute-modified)
      nil))
   '(("DOM.removeDOMBreakpoint"
      ((nodeId . 5678)
       (type . attribute-modified))
      nil)))

  (kite-test--should-send-packets
   (lambda ()
     (kite--remove-breakpoint
      (make-kite-dom-node-breakpoint
       :node-id 6789
       :type 'subtree-modified)
      nil))
   '(("DOM.removeDOMBreakpoint"
      ((nodeId . 6789)
       (type . subtree-modified))
      nil))))


(ert-deftest kite-dom-event-breakpoint-test ()
  "Test that make-kite-dom-event-breakpoint work"

  ;; Test to-string function

  (should (string=
           (kite--breakpoint-to-string
            (make-kite-dom-event-breakpoint
             :event-name "foo"))
           "DOM event `foo'"))

  (kite-test--should-send-packets
   (lambda ()
     (kite--set-breakpoint
      (make-kite-dom-event-breakpoint
       :event-name "foo")
      nil))
   '(("DOM.setEventListenerBreakpoint"
      ((eventName . "foo"))
      nil)))

  (kite-test--should-send-packets
   (lambda ()
     (kite--remove-breakpoint
      (make-kite-dom-event-breakpoint
       :event-name "foo")
      nil))
   '(("DOM.removeEventListenerBreakpoint"
      ((eventName . "foo"))
      nil))))


(ert-deftest kite-instrumentation-breakpoint-test ()
  "Test that make-kite-instrumentation-breakpoint work"

  ;; Test to-string function

  (should (string=
           (kite--breakpoint-to-string
            (make-kite-instrumentation-breakpoint
             :event-name "foo"))
           "Native event `foo'"))

  (kite-test--should-send-packets
   (lambda ()
     (kite--set-breakpoint
      (make-kite-instrumentation-breakpoint
       :event-name "foo")
      nil))
   '(("DOM.setInstrumentationBreakpoint"
      ((eventName . "foo"))
      nil)))

  (kite-test--should-send-packets
   (lambda ()
     (kite--remove-breakpoint
      (make-kite-instrumentation-breakpoint
       :event-name "foo")
      nil))
   '(("DOM.removeInstrumentationBreakpoint"
      ((eventName . "foo"))
      nil))))


(ert-deftest kite-xhr-breakpoint-test ()
  "Test that make-kite-xhr-breakpoint work"

  ;; Test to-string function

  (should (string=
           (kite--breakpoint-to-string
            (make-kite-xhr-breakpoint
             :url-substring "foo"))
           "XMLHttpRequest with substring `foo'"))

  (kite-test--should-send-packets
   (lambda ()
     (kite--set-breakpoint
      (make-kite-xhr-breakpoint
       :url-substring "foo")
      nil))
   '(("DOM.setXHRBreakpoint"
      ((url . "foo"))
      nil)))

  (kite-test--should-send-packets
   (lambda ()
     (kite--remove-breakpoint
      (make-kite-xhr-breakpoint
       :url-substring "foo")
      nil))
   '(("DOM.removeXHRBreakpoint"
      ((url . "foo"))
      nil))))

(ert-deftest kite-breakpoint-ewoc-test ()
  "Simple test that breakpoint ewoc works"
  (with-temp-buffer
    (let ((ewoc (kite--make-breakpoint-ewoc)))
      (ewoc-enter-last ewoc (make-kite-dom-event-breakpoint :event-name "bar"))
      (ewoc-enter-last ewoc (make-kite-xhr-breakpoint :url-substring "foo")))
    (should (string= (buffer-string)
                     (concat "Breakpoints:\n"
                             "* Break on DOM event `bar'\n"
                             "* Break on XMLHttpRequest with substring `foo'\n"
                             "\n")))))

(ert-deftest kite-breakpoint-ewoc-wrong-sort-order-test ()
  "Test that breakpoint ewoc sort order works"
  (with-temp-buffer
    (let ((ewoc (kite--make-breakpoint-ewoc)))
      (kite--add-breakpoint ewoc (make-kite-xhr-breakpoint :url-substring "foo"))
      (kite--add-breakpoint ewoc (make-kite-dom-event-breakpoint :event-name "bar")))
    (should (string= (buffer-string)
                     (concat "Breakpoints:\n"
                             "* Break on DOM event `bar'\n"
                             "* Break on XMLHttpRequest with substring `foo'\n"
                             "\n")))))

(ert-deftest kite-breakpoint-ewoc-correct-sort-order-test ()
  "Test that breakpoint ewoc sort order works when inserting in order"
  (with-temp-buffer
    (let ((ewoc (kite--make-breakpoint-ewoc)))
      (kite--add-breakpoint ewoc (make-kite-dom-event-breakpoint :event-name "bar"))
      (kite--add-breakpoint ewoc (make-kite-xhr-breakpoint :url-substring "foo")))
    (should (string= (buffer-string)
                     (concat "Breakpoints:\n"
                             "* Break on DOM event `bar'\n"
                             "* Break on XMLHttpRequest with substring `foo'\n"
                             "\n")))))

(ert-deftest kite-toggle-next-instruction-breakpoint-test ()
  (let (invalidate-args
        (kite-session (make-kite-session)))
    (flet ((ewoc-invalidate (ewoc &rest nodes)
                            (setq invalidate-args
                                  (cons (cons ewoc nodes) invalidate-args))))
      (with-temp-buffer
        (setf (kite-session-breakpoint-ewoc kite-session)
              (kite--make-breakpoint-ewoc))

        (should (null (ewoc-nth (kite-session-breakpoint-ewoc kite-session) 0)))
        (should (eq 0 (length invalidate-args)))
        (kite-test--should-send-packets
         (lambda ()
           (kite-toggle-next-instruction-breakpoint))
         '(("Debugger.pause" nil *)))
        (should (eq 1 (length invalidate-args)))
        (should (null (ewoc-nth (kite-session-breakpoint-ewoc kite-session) 1)))
        (let ((new-ewoc-element (ewoc-nth (kite-session-breakpoint-ewoc kite-session) 0)))
          (should (not (null new-ewoc-element)))
          (should (kite-next-instruction-breakpoint-p (ewoc-data new-ewoc-element))))
        (kite-test--should-send-packets
         (lambda ()
           (kite-toggle-next-instruction-breakpoint))
         '(("Debugger.resume" nil *)))
        (should (eq 2 (length invalidate-args)))
        (should (null (ewoc-nth (kite-session-breakpoint-ewoc kite-session) 0)))))))


(provide 'kite-breakpoint-tests)
