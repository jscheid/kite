;;; kite-global.el --- Variables and functions used by all Kite modules

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

;; Basic infrastructure for Kite, a WebKit inspector front-end.


;;; Code:

(require 'cl)
(require 'websocket)
(require 'json)

(defvar kite-Console-messageAdded-hooks nil)
(defvar kite-CSS-mediaQueryResultChanged-hooks nil)
(defvar kite-DOM-attributeModified-hooks nil)
(defvar kite-DOM-attributeRemoved-hooks nil)
(defvar kite-DOM-childNodeCountUpdated-hooks nil)
(defvar kite-DOM-childNodeInserted-hooks nil)
(defvar kite-DOM-childNodeRemoved-hooks nil)
(defvar kite-DOM-documentUpdated-hooks nil)
(defvar kite-DOM-setChildNodes-hooks nil)
(defvar kite-Debugger-globalObjectCleared-hooks nil)
(defvar kite-Debugger-paused-hooks nil)
(defvar kite-Debugger-resumed-hooks nil)
(defvar kite-Debugger-scriptParsed-hooks nil)
(defvar kite-Inspector-inspect-hooks nil)
(defvar kite-Network-dataReceived-hooks nil)
(defvar kite-Network-loadingFinished-hooks nil)
(defvar kite-Network-requestWillBeSent-hooks nil)
(defvar kite-Network-responseReceived-hooks nil)
(defvar kite-Page-domContentEventFired-hooks nil)
(defvar kite-Page-frameNavigated-hooks nil)
(defvar kite-Page-loadEventFired-hooks nil)
(defvar kite-Runtime-isolatedContextCreated-hooks nil)

(defun kite--define-global-mode-keys (map)
  (define-key map "!" 'kite-reload-page))

(defvar kite-active-sessions
  (make-hash-table :test 'equal :weakness 'value)
  "Maps webservice URL to kite-session structs for each active
  debugging session")

(defvar kite-active-session-list nil
  "List of kite-session structs.  Maintains order for
  kite-active-sessions, with most recently opened session last")

(defvar kite-session)
(make-variable-buffer-local 'kite-session)

(defvar kite-buffer-type)
(make-variable-buffer-local 'kite-buffer-type)

(defvar kite-most-recent-session nil
  "Keeps track of the most recently used Kite session.  FIXME:
  this should be an list instead so that once the most recently
  used session is closed we can fall back to the second most
  recently used.")

(defconst kite--debugger-state-resumed
  (propertize "Resumed" 'face 'success))

(defconst kite--debugger-state-paused
  (propertize "Paused" 'face 'warning))

(defstruct (kite-session)
  "Represents an active debugging session, i.e. a connection to a
remote WebKit debugger instance."
  websocket
  page-favicon-url
  page-thumbnail-url
  page-url
  page-title
  breakpoint-list
  unique-name
  (script-infos (make-hash-table :test 'equal))
  (debugger-state kite--debugger-state-resumed)
  (next-request-id 0)
  (pending-requests (make-hash-table))
  buffers
  frame-tree
  execution-context-list
  default-context
  current-context
  (error-count 0)
  last-message
  (source-map-cache (make-hash-table :test 'equal))
  (dom-nodes (make-hash-table))
  document-root
  emulate-touch-events
  show-paint-rects
  disable-cache
  can-override-device-metrics
  can-override-geo-location
  can-override-device-orientation
  can-clear-browser-cache
  can-clear-browser-cookies
  can-set-script-source)

(defstruct (kite-script-info)
  "Information about a script used in a debugging session.  Used
to cache data received via the ` Debugger.scriptParsed'
notification."
  url
  start-line
  start-column
  end-line
  end-column
  source-map-url)

(defun kite--default-success-handler (result)
  (kite--log "Ignored success result: %s" result))

(defun kite--default-error-handler (error-result)
  (error "Kite: %s" (plist-get error-result :message)))

(defun* kite-send (method &key params success-function error-function callback-args)
  "Send a JSON-RPC 2.0 packet to the remote debugger for the
current session.  The current session is retrieved from variable
`kite-session', which is buffer-local or taken from a let
binding.

METHOD is the method to be set for the JSON-RPC request.  PARAMS
is a plist of parameters to be set for the JSON-RPC request.

SUCCESS-FUNCTION is a function invoked with the JSON-RPC server
result in case of success.  ERROR-FUNCTION is a function invoked
with the JSON-RPC server error in case of error.  CALLBACK-ARGS
are passed as the second argument to SUCCESS-FUNCTION or
ERROR-FUNCTION.

SUCCESS-FUNCTION or ERROR-FUNCTION are invoked with the same
current buffer that was current when `kite-send' was invoked."
  (if (websocket-openp (kite-session-websocket kite-session))
    (let ((callback-buffer (current-buffer))
          (request-id (incf (kite-session-next-request-id kite-session))))
      (puthash request-id (list (or success-function
                                    #'kite--default-success-handler)
                                (or error-function
                                    #'kite--default-error-handler)
                                callback-buffer
                                callback-args)
               (kite-session-pending-requests kite-session))
      (let ((json-request (json-encode
                           (list
                            (cons :jsonrpc "2.0")
                            (cons :method method)
                            (cons :params params)
                            (cons :id request-id)))))
        (kite--log "Sending request: %s" json-request)
        (websocket-send-text (kite-session-websocket kite-session)
                             json-request)))
    (error "This kite session is closed")))

(defun kite--get (object &rest members)
  "Convenience function for getting members of nested data
structures.  For each item of MEMBERS, retrieves an item from
OBJECT and applies subsequent members to that item.  Returns the
last item yielded by this operation.  A keyword member looks up
the item using `plist-get'. A number members looks up the item
using `elt'.  Other member types are not currently implemented."
  (let ((result object))
    (while members
      (cond
       ((keywordp (car members))
        (setq result (plist-get result (car members))))
       ((numberp (car members))
        (setq result (elt result (car members))))
       (t
        (error "Don't know how to interpret member: %s" (car members))))
      (setq members (cdr members)))
    result))

(defun kite--find-frame-recursive (frame-tree frame-id)
  (if (string= frame-id (plist-get (plist-get frame-tree :frame) :id))
      (plist-get frame-tree :frame)
    (let* ((children (plist-get frame-tree :childFrames))
           (num-children (length children))
           (child-index 0)
           found-frame)
      (while (and (< child-index num-children)
                  (null found-frame))
        (setq found-frame (kite--find-frame-recursive (elt children child-index) frame-id))
        (setq child-index (1+ child-index)))
      found-frame)))

(defun kite--frame-by-id (frame-id)
  (kite--find-frame-recursive (kite-session-frame-tree kite-session) frame-id))

(defun kite--release-object (object-id)
  "Release the object with the given OBJECT-ID on the browser
side."
  (when (null object-id)
    (error "kite--release-object called with null OBJECT-ID"))
  (kite-send "Runtime.releaseObject"
             :params `((objectId . ,object-id))))

(defun kite--find-buffer (websocket-url type)
  "Return the buffer corresponding to the given WEBSOCKET-URL and
buffer TYPE and return it, or nil if no such buffer is currently
open."
  (let ((buffer-iterator (buffer-list))
        found)
    (while (and buffer-iterator (not found))
      (let ((buffer-kite-session (buffer-local-value
                                  'kite-session
                                  (car buffer-iterator))))
        (when (and buffer-kite-session
                   (string= (websocket-url (kite-session-websocket buffer-kite-session))
                            websocket-url)
                   (eq type (buffer-local-value 'kite-buffer-type (car buffer-iterator))))
          (setq found (car buffer-iterator))))
      (setq buffer-iterator (cdr buffer-iterator)))
    found))

(defun kite--get-buffer-create (websocket-url
                                type
                                &optional
                                after-load-function
                                use-major-mode
                                use-buffer-name)
  "Return the buffer corresponding to the given WEBSOCKET-URL and
buffer TYPE and return it.  If no such buffer is currently open,
create one.  USE-MAJOR-MODE and USE-BUFFER-NAME are used if
given, otherwise they are derived from TYPE.  AFTER-LOAD-FUNCTION
is invoked with the new buffer current after it has been
initialized."
  (or (let ((buf (kite--find-buffer websocket-url type)))
        (when buf
          (prog1
              (switch-to-buffer buf)
            (when after-load-function
              (funcall after-load-function)))))
      (lexical-let*
          ((-kite-session (gethash websocket-url kite-active-sessions))
           (buf (generate-new-buffer
                 (or use-buffer-name
                     (format "*kite %s %s*"
                             type
                             (kite-session-unique-name -kite-session))))))
        (push buf (kite-session-buffers -kite-session))
        (switch-to-buffer buf)
        (with-current-buffer buf
          (let ((kite-session -kite-session)
                (mode (or use-major-mode
                          (intern (format "kite-%s-mode" type)))))
            (funcall mode))
          (when after-load-function
            (add-hook 'kite-async-init-hook after-load-function nil t))
          (setq kite-session -kite-session)
          (set (make-local-variable 'kite-buffer-type) type)
          (add-hook 'kill-buffer-hook 'kite--kill-buffer nil t)
        buf))))

(defun kite--log (format-string &rest args)
  "Print a message to the kite debug logging buffer"
  (with-current-buffer
      (get-buffer-create (format "*kite log*"))
    (save-excursion
      (goto-char (point-max))
      (insert (concat (apply 'format format-string args) "\n")))))

(defun kite--select-session ()
  "Used by global commands to select a session to act upon.  If
the command is executed in a buffer with a local binding for
`kite-session', use that.  Otherwise, use the most recent session
if available.  Otherwise, raise an error."
  (cond
   ((and (boundp 'kite-session)
         kite-session)
    kite-session)
   ((not (null kite-most-recent-session))
    (gethash kite-most-recent-session
             kite-active-sessions))
   (t
    (error "No kite sessions active."))))

(provide 'kite-global)

;;; kite-global.el ends here
