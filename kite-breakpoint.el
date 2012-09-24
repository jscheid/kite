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

(require 'kite-global)
(require 'cl)

(require 'browse-url nil t)

(defconst kite--dom-breakpoint-names
  '("abort" "beforecopy" "beforecut" "beforeload" "beforepaste"
    "beforeunload" "blocked" "blur" "cached" "change" "checking" "click"
    "close" "complete" "compositionend" "compositionstart"
    "compositionupdate" "connect" "contextmenu" "copy" "cut" "dblclick"
    "devicemotion" "deviceorientation" "display" "downloading" "drag"
    "dragend" "dragenter" "dragleave" "dragover" "dragstart" "drop"
    "error" "focus" "focusin" "focusout" "hashchange" "input" "invalid"
    "keydown" "keypress" "keyup" "load" "loadstart" "message"
    "mousedown" "mousemove" "mouseout" "mouseover" "mouseup"
    "mousewheel" "noupdate" "obsolete" "offline" "online" "open"
    "overflowchanged" "pagehide" "pageshow" "paste" "popstate"
    "readystatechange" "reset" "resize" "scroll" "search" "select"
    "selectstart" "selectionchange" "storage" "submit" "textInput"
    "unload" "updateready" "versionchange" "webkitvisibilitychange"
    "write" "writeend" "writestart" "zoom" "DOMActivate" "DOMFocusIn"
    "DOMFocusOut" "DOMAttrModified" "DOMCharacterDataModified"
    "DOMNodeInserted" "DOMNodeInsertedIntoDocument" "DOMNodeRemoved"
    "DOMNodeRemovedFromDocument" "DOMSubtreeModified" "DOMContentLoaded"
    "webkitBeforeTextInserted" "webkitEditableContentChanged" "canplay"
    "canplaythrough" "durationchange" "emptied" "ended" "loadeddata"
    "loadedmetadata" "pause" "play" "playing" "ratechange" "seeked"
    "seeking" "timeupdate" "volumechange" "waiting" "addtrack"
    "cuechange" "enter" "exit" "webkitbeginfullscreen"
    "webkitendfullscreen" "webkitsourceopen" "webkitsourceended"
    "webkitsourceclose" "progress" "stalled" "suspend"
    "webkitAnimationEnd" "webkitAnimationStart"
    "webkitAnimationIteration" "webkitTransitionEnd" "orientationchange"
    "timeout" "touchstart" "touchmove" "touchend" "touchcancel"
    "success" "loadend" "webkitfullscreenchange" "webkitfullscreenerror"
    "webkitspeechchange" "webglcontextlost" "webglcontextrestored"
    "webglcontextcreationerror" "audioprocess" "connecting"
    "addstream" "removestream" "statechange" "show"
    "webkitpointerlocklost")
  "WebKit DOM breakpoint names, taken from
  Source/WebCore/dom/EventNames.h")

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
  (kite-send "Debugger.pause" :success-function response-handler))

(defun kite--remove-next-instruction-breakpoint (breakpoint response-handler)
  "Remove a breakpoint of type `next-instruction'"
  (kite-send "Debugger.resume" :success-function response-handler))

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
             :params '(:state "all")
             :success-function response-handler))

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
             :params '(:state "uncaught")
             :success-function response-handler))

(defun kite--remove-exceptions-breakpoint (breakpoint response-handler)
  (kite-send "Debugger.setPauseOnExceptions"
             :params '(:state "none")
             :success-function response-handler))

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
  (kite-send "DOMDebugger.setDOMBreakpoint"
             :params
             (list :nodeId (kite-dom-node-breakpoint-node-id breakpoint)
                   :type (kite-dom-node-breakpoint-type breakpoint))
             :success-function response-handler))

(defun kite--remove-dom-node-breakpoint (breakpoint response-handler)
  (kite-send "DOMDebugger.removeDOMBreakpoint"
             :params
             (list :nodeId (kite-dom-node-breakpoint-node-id breakpoint)
                   :type (kite-dom-node-breakpoint-type breakpoint))
             :success-function response-handler))

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
  (kite-send "DOMDebugger.setEventListenerBreakpoint"
             :params
             (list :eventName (kite-dom-event-breakpoint-event-name breakpoint))
             :success-function response-handler))

(defun kite--remove-dom-event-breakpoint (breakpoint response-handler)
  (kite-send "DOMDebugger.removeEventListenerBreakpoint"
             :params
             (list :eventName (kite-dom-event-breakpoint-event-name breakpoint))
             :success-function response-handler))

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
  (kite-send "DOMDebugger.setInstrumentationBreakpoint"
             :params
             (list :eventName (kite-instrumentation-breakpoint-event-name breakpoint))
             :success-function response-handler))

(defun kite--remove-instrumentation-breakpoint (breakpoint response-handler)
  (kite-send "DOMDebugger.removeInstrumentationBreakpoint"
             :params
             (list :eventName (kite-instrumentation-breakpoint-event-name breakpoint))
             :success-function response-handler))

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
             :params
             (list :url (kite-xhr-breakpoint-url-substring breakpoint))
             :success-function response-handler))

(defun kite--remove-xhr-breakpoint (breakpoint response-handler)
  (kite-send "DOM.removeXHRBreakpoint"
             :params
             (list :url (kite-xhr-breakpoint-url-substring breakpoint))
             :success-function response-handler))

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

;; High-level functions

(defun kite--session-has-breakpoint (kite-session predicate)
  (find-if predicate
           (kite-session-breakpoint-list kite-session)))

(defun kite--session-add-breakpoint (kite-session breakpoint)
  (lexical-let ((lex-kite-session kite-session)
                (lex-breakpoint breakpoint))
    (kite--set-breakpoint
     breakpoint
     (lambda (result)
       (setf (kite-session-breakpoint-list lex-kite-session)
             (stable-sort
              (cons lex-breakpoint
                    (kite-session-breakpoint-list lex-kite-session))
              (lambda (a b)
                (< (kite-breakpoint-sort-priority a)
                   (kite-breakpoint-sort-priority b)))))
       (message "Breakpoint set")))))

(defun kite--session-remove-breakpoints (kite-session predicate)
  (mapc (lambda (breakpoint)
          (when (funcall predicate breakpoint)
            (lexical-let ((lex-breakpoint breakpoint)
                          (lex-kite-session kite-session))
              (kite--remove-breakpoint
               breakpoint
               (lambda (result)
                 (setf (kite-session-breakpoint-list lex-kite-session)
                       (delete lex-breakpoint
                               (kite-session-breakpoint-list lex-kite-session)))
                 (message "Breakpoint removed"))))))
        (copy-seq (kite-session-breakpoint-list kite-session))))

(defun kite-set-xhr-breakpoint ()
  (interactive)
  (let ((kite-session (kite--select-session)))
    (kite--session-add-breakpoint
     kite-session
     (make-kite-xhr-breakpoint
      :url-substring (read-from-minibuffer
                      "XHR breakpoint on URL substring: "
                      (and (boundp 'browse-url-url-at-point)
                           (browse-url-url-at-point))
                      nil nil 'kite-url-substring-history)))))

(defun kite-set-dom-event-breakpoint ()
  (interactive)
  (let ((kite-session (kite--select-session))
        (event-name (completing-read
                     "Breakpoint on DOM event: "
                     kite--dom-breakpoint-names
                     nil
                     'confirm
                     nil
                     'kite-dom-event-history)))
    (when event-name
      (kite--session-add-breakpoint
       kite-session
       (make-kite-dom-event-breakpoint
        :event-name event-name)))))

(defun kite-set-instrumentation-breakpoint ()
  (interactive)
  (let ((kite-session (kite--select-session)))
    (kite--session-add-breakpoint
     kite-session
     (make-kite-instrumentation-breakpoint
      :event-name (read-from-minibuffer
                   "Breakpoint on native event: "
                   nil nil nil 'kite-instrumentation-history)))))

(defun kite-toggle-exception-breakpoint ()
  (interactive)
  (cond

   ;; break on uncaught -> break on all
   ((kite--session-has-breakpoint
     kite-session
     #'kite-uncaught-exceptions-breakpoint-p)
    (kite--session-remove-breakpoints
     kite-session
     #'kite-uncaught-exceptions-breakpoint-p)
    (kite--session-add-breakpoint
     kite-session
     (make-kite-all-exceptions-breakpoint)))

   ;; break on all -> don't break on exception
   ((kite--session-has-breakpoint
     kite-session
     #'kite-all-exceptions-breakpoint-p)
    (kite--session-remove-breakpoints
     kite-session
     #'kite-all-exceptions-breakpoint-p))

   ;; don't break -> break on uncaught
   (t
    (kite--session-add-breakpoint
     kite-session
     (make-kite-uncaught-exceptions-breakpoint)))))

(defun kite-toggle-next-instruction-breakpoint ()
  (interactive)
  (if (kite--session-has-breakpoint
       kite-session
       #'kite-next-instruction-breakpoint-p)
      (kite--session-remove-breakpoints
       kite-session
       #'kite-next-instruction-breakpoint-p)
    (kite--session-add-breakpoint
       kite-session
     (make-kite-next-instruction-breakpoint))))

(provide 'kite-breakpoint)

;;; kite-breakpoint.el ends here
