;;; kite-breakpoint.el --- Kite breakpoint implementations

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

;; This package provides implementations for the various breakpoint
;; types supported by WebKit inspector.
;;
;; It is part of Kite, a WebKit inspector front-end.


;;; Code:

(require 'browse-url nil t)

(defstruct
  (kite-breakpoint
   (:constructor nil))   ; no default constructor
  to-string-function
  set-function
  remove-function
  sort-priority)

(defstruct
  (kite-next-instruction-breakpoint
   (:include kite-breakpoint)
   (:constructor nil)   ; no default constructor
   (:constructor
    make-kite-next-instruction-breakpoint
    (&aux
     (to-string-function 'kite--next-instruction-breakpoint-to-string)
     (set-function 'kite--set-next-instruction-breakpoint)
     (remove-function 'kite--remove-next-instruction-breakpoint)
     (sort-priority 0)))))

(defun kite--next-instruction-breakpoint-to-string (breakpoint)
  "Return the string representation of breakpoints of type `next-instruction'"
  "next instruction")

(defun kite--set-next-instruction-breakpoint (breakpoint response-handler)
  "Set a breakpoint of type `next-instruction'"
  (kite-send "Debugger.pause" nil response-handler))

(defun kite--remove-next-instruction-breakpoint (breakpoint response-handler)
  "Remove a breakpoint of type `next-instruction'"
  (kite-send "Debugger.resume" nil response-handler))

(defstruct
  (kite-all-exceptions-breakpoint
   (:include kite-breakpoint)
   (:constructor nil)   ; no default constructor
   (:constructor
    make-kite-all-exceptions-breakpoint
    (&aux
     (to-string-function 'kite--all-exceptions-breakpoint-to-string)
     (set-function 'kite--set-all-exceptions-breakpoint)
     (remove-function 'kite--remove-exceptions-breakpoint)
     (sort-priority 1)))))

(defun kite--all-exceptions-breakpoint-to-string (breakpoint)
  "Return the string representation of breakpoints of type `all-exceptions'"
  "all exceptions")

(defun kite--set-all-exceptions-breakpoint (breakpoint response-handler)
  (kite-send "Debugger.setPauseOnExceptions"
             (list (cons 'state "all"))
             response-handler))

(defstruct
  (kite-uncaught-exceptions-breakpoint
   (:include kite-breakpoint)
   (:constructor nil)   ; no default constructor
   (:constructor
    make-kite-uncaught-exceptions-breakpoint
    (&aux
     (to-string-function 'kite--uncaught-exceptions-breakpoint-to-string)
     (set-function 'kite--set-uncaught-exceptions-breakpoint)
     (remove-function 'kite--remove-exceptions-breakpoint)
     (sort-priority 1)))))

(defun kite--set-uncaught-exceptions-breakpoint (breakpoint response-handler)
  (kite-send "Debugger.setPauseOnExceptions"
             (list (cons 'state "uncaught"))
             response-handler))

(defun kite--remove-exceptions-breakpoint (breakpoint response-handler)
  (kite-send "Debugger.setPauseOnExceptions"
             (list (cons 'state "none"))
             response-handler))

(defun kite--uncaught-exceptions-breakpoint-to-string (breakpoint)
  "Return the string representation of breakpoints of type `uncaught-exceptions'"
  "uncaught exceptions")

;; Location breakpoints

(defstruct
  (kite-location-breakpoint
   (:include kite-breakpoint)
   (:constructor nil)   ; no default constructor
   (:constructor
    make-kite-location-breakpoint
    (&key
     locations
     condition
     &aux
     (to-string-function 'kite--location-breakpoint-to-string)
     (set-function 'kite--set-location-breakpoint)
     (remove-function 'kite--remove-location-breakpoint)
     (sort-priority 2))))
  locations
  condition)

;; DOM Node breakpoints

(defstruct
  (kite-dom-node-breakpoint
   (:include kite-breakpoint)
   (:constructor nil)   ; no default constructor
   (:constructor
    make-kite-dom-node-breakpoint
    (&key
     node-id
     type
     &aux
     (to-string-function 'kite--dom-node-breakpoint-to-string)
     (set-function 'kite--set-dom-node-breakpoint)
     (remove-function 'kite--remove-dom-node-breakpoint)
     (sort-priority 3))))
  node-id
  type)

(defun kite--dom-node-breakpoint-to-string (breakpoint)
  (let ((type (kite-dom-node-breakpoint-type breakpoint))
        (node-id (kite-dom-node-breakpoint-node-id breakpoint)))
    (cond
     ((eq type 'subtree-modified)
      (format "modification of subtree of node #%s" node-id))
     ((eq type 'attribute-modified)
      (format "modification of attribute of node #%s" node-id))
     ((eq type 'node-removed)
      (format "removal of node #%s" node-id)))))

(defun kite--set-dom-node-breakpoint (breakpoint response-handler)
  (kite-send "DOM.setDOMBreakpoint"
             (list (cons 'nodeId (kite-dom-node-breakpoint-node-id breakpoint))
                   (cons 'type (kite-dom-node-breakpoint-type breakpoint)))
             response-handler))

(defun kite--remove-dom-node-breakpoint (breakpoint response-handler)
  (kite-send "DOM.removeDOMBreakpoint"
             (list (cons 'nodeId (kite-dom-node-breakpoint-node-id breakpoint))
                   (cons 'type (kite-dom-node-breakpoint-type breakpoint)))
             response-handler))

;; DOM Event breakpoints

(defstruct
  (kite-dom-event-breakpoint
   (:include kite-breakpoint)
   (:constructor nil)   ; no default constructor
   (:constructor
    make-kite-dom-event-breakpoint
    (&key
     event-name
     &aux
     (to-string-function 'kite--dom-event-breakpoint-to-string)
     (set-function 'kite--set-dom-event-breakpoint)
     (remove-function 'kite--remove-dom-event-breakpoint)
     (sort-priority 4))))
  event-name)

(defun kite--dom-event-breakpoint-to-string (breakpoint)
  (format "DOM event `%s'" (kite-dom-event-breakpoint-event-name breakpoint)))

(defun kite--set-dom-event-breakpoint (breakpoint response-handler)
  (kite-send "DOM.setEventListenerBreakpoint"
             (list (cons 'eventName (kite-dom-event-breakpoint-event-name breakpoint)))
             response-handler))

(defun kite--remove-dom-event-breakpoint (breakpoint response-handler)
  (kite-send "DOM.removeEventListenerBreakpoint"
             (list (cons 'eventName (kite-dom-event-breakpoint-event-name breakpoint)))
             response-handler))

;; Instrumentation breakpoints

(defstruct
  (kite-instrumentation-breakpoint
   (:include kite-breakpoint)
   (:constructor nil)   ; no default constructor
   (:constructor
    make-kite-instrumentation-breakpoint
    (&key
     event-name
     &aux
     (to-string-function 'kite--instrumentation-breakpoint-to-string)
     (set-function 'kite--set-instrumentation-breakpoint)
     (remove-function 'kite--remove-instrumentation-breakpoint)
     (sort-priority 5))))
  event-name)

(defun kite--instrumentation-breakpoint-to-string (breakpoint)
  (format "Native event `%s'" (kite-instrumentation-breakpoint-event-name breakpoint)))

(defun kite--set-instrumentation-breakpoint (breakpoint response-handler)
  (kite-send "DOM.setInstrumentationBreakpoint"
             (list (cons 'eventName (kite-instrumentation-breakpoint-event-name breakpoint)))
             response-handler))

(defun kite--remove-instrumentation-breakpoint (breakpoint response-handler)
  (kite-send "DOM.removeInstrumentationBreakpoint"
             (list (cons 'eventName (kite-instrumentation-breakpoint-event-name breakpoint)))
             response-handler))

;; XHR breakpoints

(defstruct
  (kite-xhr-breakpoint
   (:include kite-breakpoint)
   (:constructor nil)   ; no default constructor
   (:constructor
    make-kite-xhr-breakpoint
    (&key
     url-substring
     &aux
     (to-string-function 'kite--xhr-breakpoint-to-string)
     (set-function 'kite--set-xhr-breakpoint)
     (remove-function 'kite--remove-xhr-breakpoint)
     (sort-priority 6))))
  url-substring)

(defun kite--xhr-breakpoint-to-string (breakpoint)
  (format "XMLHttpRequest with substring `%s'" (kite-xhr-breakpoint-url-substring breakpoint)))

(defun kite--set-xhr-breakpoint (breakpoint response-handler)
  (kite-send "DOM.setXHRBreakpoint"
             (list (cons 'url (kite-xhr-breakpoint-url-substring breakpoint)))
             response-handler))

(defun kite--remove-xhr-breakpoint (breakpoint response-handler)
  (kite-send "DOM.removeXHRBreakpoint"
             (list (cons 'url (kite-xhr-breakpoint-url-substring breakpoint)))
             response-handler))

;; Generic functions

(defun kite--breakpoint-to-string (breakpoint)
  (funcall (kite-breakpoint-to-string-function breakpoint)
           breakpoint))

(defun kite--set-breakpoint (breakpoint response-handler)
  (funcall (kite-breakpoint-set-function breakpoint)
           breakpoint response-handler))

(defun kite--remove-breakpoint (breakpoint response-handler)
  (funcall (kite-breakpoint-remove-function breakpoint)
           breakpoint response-handler))

;; EWOC functions

(defun kite--make-breakpoint-ewoc ()
  (ewoc-create
   (lambda (breakpoint)
     (insert (format
              "* Break on %s"
              (kite--breakpoint-to-string breakpoint))))
   "Breakpoints:"))

;; High-level functions

(defun kite--add-breakpoint (ewoc breakpoint)
  (let ((insert-after (ewoc-nth ewoc 0)))
    (if (or (null insert-after)
            (>= (kite-breakpoint-sort-priority (ewoc-data insert-after))
                (kite-breakpoint-sort-priority breakpoint)))
        (ewoc-enter-first ewoc breakpoint)
      (setq insert-after (ewoc-next ewoc insert-after))
      (while (and insert-after
                  (< (kite-breakpoint-sort-priority (ewoc-data insert-after))
                     (kite-breakpoint-sort-priority breakpoint)))
        (setq insert-after (ewoc-next ewoc insert-after)))
      (if insert-after
          (ewoc-enter-after ewoc insert-after breakpoint)
        (ewoc-enter-last ewoc breakpoint)))))

(defun kite-set-xhr-breakpoint ()
  (interactive)
  (kite--add-breakpoint
   (kite-session-breakpoint-ewoc kite-session)
   (make-kite-xhr-breakpoint
    :url-substring (read-from-minibuffer
                    "XHR breakpoint on URL substring: "
                    (and (boundp 'browse-url-url-at-point)
                         (browse-url-url-at-point))
                    nil nil 'kite-url-substring-history))))

(defun kite-set-dom-event-breakpoint ()
  (interactive)
  (kite--add-breakpoint
   (kite-session-breakpoint-ewoc kite-session)
   (make-kite-dom-event-breakpoint
    :event-name (read-from-minibuffer
                 "Breakpoint on DOM event: "
                 nil nil nil 'kite-dom-event-history))))

(defun kite-set-instrumentation-breakpoint ()
  (interactive)
  (kite--add-breakpoint
   (kite-session-breakpoint-ewoc kite-session)
   (make-kite-instrumentation-breakpoint
    :event-name (read-from-minibuffer
                 "Breakpoint on native event: "
                 nil nil nil 'kite-instrumentation-history))))

(defun kite-toggle-exception-breakpoint ()
  (interactive)
  (lexical-let ((breakpoint-ewoc (kite-session-breakpoint-ewoc kite-session)))
    (let ((uncaught-exceptions-breakpoints
           (ewoc-collect breakpoint-ewoc 'kite-uncaught-exceptions-breakpoint-p))
          (all-exceptions-breakpoints
           (ewoc-collect breakpoint-ewoc 'kite-all-exceptions-breakpoint-p)))
    (cond
     (uncaught-exceptions-breakpoints
      (ewoc-filter breakpoint-ewoc (lambda (breakpoint)
                                     (not (kite-uncaught-exceptions-breakpoint-p breakpoint))))
      (kite--add-breakpoint breakpoint-ewoc
                            (make-kite-all-exceptions-breakpoint)))
     (all-exceptions-breakpoints
      (ewoc-filter breakpoint-ewoc (lambda (breakpoint)
                                     (not (kite-all-exceptions-breakpoint-p breakpoint)))))
     (t
      (kite--add-breakpoint breakpoint-ewoc
                            (make-kite-uncaught-exceptions-breakpoint)))))))

(defun kite-toggle-next-instruction-breakpoint ()
  (interactive)
  (lexical-let*
      ((breakpoint-ewoc (kite-session-breakpoint-ewoc kite-session))
       (old-breakpoints
        (ewoc-collect breakpoint-ewoc 'kite-next-instruction-breakpoint-p)))
    (if old-breakpoints
        (kite--remove-breakpoint (car old-breakpoints)
                                 (lambda (response)
                                   (ewoc-filter breakpoint-ewoc
                                                (lambda (breakpoint)
                                                  (not (kite-next-instruction-breakpoint-p
                                                        breakpoint))))
                                   (ewoc-invalidate breakpoint-ewoc)))
      (lexical-let ((new-breakpoint (make-kite-next-instruction-breakpoint)))
        (kite--set-breakpoint new-breakpoint
                              (lambda (response)
                                (kite--add-breakpoint breakpoint-ewoc new-breakpoint)
                                (ewoc-invalidate breakpoint-ewoc)))))))

(provide 'kite-breakpoint)

;;; kite-breakpoint.el ends here
