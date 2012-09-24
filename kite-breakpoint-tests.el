;;; kite-breakpoint-tests.el --- Kite test suite for breakpoints

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

;; Kite test suite for breakpoints.
;;
;; It is part of Kite, a WebKit inspector front-end.


;;; Code:

(defun kite-test--should-send-packets (body expected-packets &optional response-function)
  (let ((response-function* (or response-function
                                (lambda (command params)
                                  '(ok))))
        sent-packets)
    (flet ((kite-send (command &rest keyword-args)
                      (setq sent-packets (cons (list command
                                                     (plist-get keyword-args :params)
                                                     (plist-get keyword-args :success-function))
                                               sent-packets))
                      (when (plist-get keyword-args :success-function)
                        (funcall (plist-get keyword-args :success-function)
                                 (funcall response-function*
                                          command
                                          (plist-get keyword-args :params))))))
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
      (:state "all")
      nil)))

  ;; Test removing breakpoints

  (kite-test--should-send-packets
   (lambda ()
     (kite--remove-breakpoint
      (make-kite-all-exceptions-breakpoint) nil))
   '(("Debugger.setPauseOnExceptions"
      (:state "none")
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
      (:state "uncaught")
      nil)))

  ;; Test removing breakpoints

  (kite-test--should-send-packets
   (lambda ()
     (kite--remove-breakpoint
      (make-kite-uncaught-exceptions-breakpoint) nil))
   '(("Debugger.setPauseOnExceptions"
      (:state "none")
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
   '(("DOMDebugger.setDOMBreakpoint"
      (:nodeId
       4567
       :type node-setd)
      nil)))

  (kite-test--should-send-packets
   (lambda ()
     (kite--set-breakpoint
      (make-kite-dom-node-breakpoint
       :node-id 5678
       :type 'attribute-modified)
      nil))
   '(("DOMDebugger.setDOMBreakpoint"
      (:nodeId
       5678
       :type attribute-modified)
      nil)))

  (kite-test--should-send-packets
   (lambda ()
     (kite--set-breakpoint
      (make-kite-dom-node-breakpoint
       :node-id 6789
       :type 'subtree-modified)
      nil))
   '(("DOMDebugger.setDOMBreakpoint"
      (:nodeId
       6789
       :type subtree-modified)
      nil)))

  ;; Test removing breakpoints

  (kite-test--should-send-packets
   (lambda ()
     (kite--remove-breakpoint
      (make-kite-dom-node-breakpoint
       :node-id 4567
       :type 'node-removed)
      nil))
   '(("DOMDebugger.removeDOMBreakpoint"
      (:nodeId
       4567
       :type node-removed)
      nil)))

  (kite-test--should-send-packets
   (lambda ()
     (kite--remove-breakpoint
      (make-kite-dom-node-breakpoint
       :node-id 5678
       :type 'attribute-modified)
      nil))
   '(("DOMDebugger.removeDOMBreakpoint"
      (:nodeId
       5678
       :type attribute-modified)
      nil)))

  (kite-test--should-send-packets
   (lambda ()
     (kite--remove-breakpoint
      (make-kite-dom-node-breakpoint
       :node-id 6789
       :type 'subtree-modified)
      nil))
   '(("DOMDebugger.removeDOMBreakpoint"
      (:nodeId
       6789
       :type subtree-modified)
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
   '(("DOMDebugger.setEventListenerBreakpoint"
      (:eventName "foo")
      nil)))

  (kite-test--should-send-packets
   (lambda ()
     (kite--remove-breakpoint
      (make-kite-dom-event-breakpoint
       :event-name "foo")
      nil))
   '(("DOMDebugger.removeEventListenerBreakpoint"
      (:eventName "foo")
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
   '(("DOMDebugger.setInstrumentationBreakpoint"
      (:eventName "foo")
      nil)))

  (kite-test--should-send-packets
   (lambda ()
     (kite--remove-breakpoint
      (make-kite-instrumentation-breakpoint
       :event-name "foo")
      nil))
   '(("DOMDebugger.removeInstrumentationBreakpoint"
      (:eventName "foo")
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
      (:url "foo")
      nil)))

  (kite-test--should-send-packets
   (lambda ()
     (kite--remove-breakpoint
      (make-kite-xhr-breakpoint
       :url-substring "foo")
      nil))
   '(("DOM.removeXHRBreakpoint"
      (:url "foo")
      nil))))

(provide 'kite-breakpoint-tests)

;;; kite-breakpoint-tests.el ends here
