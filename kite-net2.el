;;; kite-net2.el --- Kite network module implementation

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

;; This package implements the WebKit inspector network module.
;;
;; It is part of Kite, a WebKit inspector front-end.


;;; Code:

(require 'kite-global)
(require 'kite-util)
(require 'ewoc)

(defstruct (kite-request)
  "Information about a network request."
  id
  frame-id
  loader-id
  document-url
  request
  request-timestamp
  initiator
  redirect-response
  was-served-from-cached-p
  resource-type
  response-timestamp
  response
  data-length
  encoded-data-length
  data-received-timestamp
  loading-finished-timestamp
  loading-failed-timestamp
  error-text
  canceled-p
  cached-resource)

(defconst kite--network-url-width 50)
(defconst kite--network-status-width 3)
(defconst kite--network-size-width 10)

(defconst kite--network-display-placeholder "x"
  "The 'display text property won't render on whitespace, so this
is an arbitrary non-whitespace character used for such text.")

(defconst kite--network-header-string
  (eval-when-compile
    (propertize
     (concat
      (kite--fill-overflow
       "Method+URL"
       kite--network-url-width)
      "  "
      (kite--fill-overflow
       "Sta"
       kite--network-status-width)
      "  "
      (kite--fill-overflow
       "Size"
       kite--network-size-width)
      "  ")
     'face 'kite-table-head)))

(defvar kite--network-ewoc nil
  "Local variable in a network buffer pointing to the ewoc for
rendering the network requests.")

(defvar kite--network-sorted-requests nil
  "Local variable in a network buffer containing `kite-request'
structs, sorted in the user's preferred sorting order.  Updated
by `kite--network-invalidate'")

(defvar kite--network-min-time nil
  "Local variable in a network buffer containing the earliest
point in time at which a network event was recorded.  Updated by
`kite--network-invalidate'")

(defvar kite--network-max-time nil
  "Local variable in a network buffer containing the latest point
in time at which a network event was recorded.  Updated by
`kite--network-invalidate'")


;;; Network buffer face declarations

(defface bg:kite-requestStart
  '((t :background "#f00"))
  "Bar chart for requestStart status"
  :version "24.1"
  :group 'kite-faces)

(defface bg:kite-receiveHeadersEnd
  '((t :background "#0f0"))
  "Bar chart for receiveHeadersEnd status"
  :version "24.1"
  :group 'kite-faces)

(defface bg:kite-sendEnd
  '((t :background "#0ff"))
  "Bar chart for receiveHeadersEnd status"
  :version "24.1"
  :group 'kite-faces)

(defface bg:kite-pageStart
  '((t))
  "Bar chart for receiveHeadersEnd status"
  :version "24.1"
  :group 'kite-faces)

(defface bg:kite-dataReceived
  '((t :background "#f0f"))
  "Bar chart for receiveHeadersEnd status"
  :version "24.1"
  :group 'kite-faces)

(defface bg:kite-tick
  '((t :background "#fff"))
  "Bar chart tick color"
  :version "24.1"
  :group 'kite-faces)

(defface kite-table-head
  '((t :inherit highlight))
  "Basic face used to highlight warnings."
  :version "24.1"
  :group 'kite-faces)

(defface bg:kite-table-head
  (list (list t :background (face-attribute 'kite-table-head :foreground nil 'default)))
  "Basic face used to highlight warnings."
  :version "24.1"
  :group 'kite-faces)


;;; Helper functions for accessing `kite-request' structs

(defun kite-session-request-for-id (kite-session request-id)
  "Return the `kite-request' structure corresponding to the given
REQUEST-ID, or nil if not found."
  (let ((result
         (gethash request-id (kite-session-requests-by-id
                              kite-session))))
    (if result
        result
      (error "No request found with ID %s" request-id))))

(defun kite-request-url (request)
  "Return the URL associated with the given KITE-REQUEST."
  (plist-get (kite-request-request request) :url))

(defun kite-request-method (request)
  "Return the HTTP method associated with the given
KITE-REQUEST."
  (plist-get (kite-request-request request) :method))

(defun kite-request-status-code (request)
  "Return the HTTP status code associated with the given
KITE-REQUEST."
  (plist-get (kite-request-response request) :status))

(defun kite-request-times (request)
  "Return an alist of (EVENT-NAME . EVENT-TIME) for all events
that have been recorded so far for the given REQUEST, which
should be a `kite-request' struct.  EVENT-NAME is a symbol, for
possible event names see the source code.  EVENT-TIME is a unix
timestamp.  Events with a null, negative or zero event time are
excluded from the result.  The result is unsorted."
  (let (filtered
        (unfiltered
         (append
          (list :requestStart
                (kite-request-request-timestamp request)
                :responseStart
                (kite-request-response-timestamp request)
                :dataReceived
                (kite-request-data-received-timestamp request)
                :loadingFinished
                (kite-request-loading-finished-timestamp request)
                :loadingFailed
                (kite-request-loading-failed-timestamp request))
          (plist-get (kite-request-response request) :timing))))
    (while unfiltered
      (when (and (cadr unfiltered)
                 (> (cadr unfiltered) 0))
        (push (cons
               (car unfiltered)
               (cadr unfiltered))
              filtered))
      (setq unfiltered (cddr unfiltered)))
    filtered))


;;; Network buffer UI code

(defun kite--network2-barchart-width ()
  "Return barchart width for current buffer, in pixels."
  (/ (* (frame-pixel-width)
        (- (frame-width)
           (string-width kite--network-header-string)
           10))
     (frame-width)))

(defun kite--network2-best-tick-step (max-width
                                      min-tick-width
                                      total-time)
  "Determine the best step width and unit for the ticks rendered
in the header line.  MAX-WIDTH is the width in pixels across
which the barchart is spread out.  MIN-TICK-WIDTH is the minimum
acceptable tick spacing in pixels.  TOTAL-TIME is the total time
corresponding to MAX-WIDTH in seconds.

Return a plist with `:tick-factor-unit', a string denoting the
time unit to use for labelling the ticks; `tick-factor-num' and
`tick-factor-den', which together form a ratio in seconds
corresponding to that unit (e.g. 1/1000 for milliseconds); and
`:tick-step', the amount (in that unit) by which to increment the
labelling for each tick step.

For example, if the best tick step is found to be 5 ms, the
return value will be (:unit \"ms\" :num 1 :den 1000 :step 5)."
  (let* ((tick-steps (let ((per-1000 '(1 2 5 10 20 50 100 200 500))
                           (per-60 '(1 2 5 10 15 30))
                           (per-24 '(1 2 5 12)))
                       (append
                        (mapcar (lambda (n) (cons n 'ns)) per-1000)
                        (mapcar (lambda (n) (cons n 'ms)) per-1000)
                        (mapcar (lambda (n) (cons n 's)) per-60)
                        (mapcar (lambda (n) (cons n 'm)) per-60)
                        (mapcar (lambda (n) (cons n 'h)) per-24))))
         (units '((ns 1 1000000)
                  (ms 1 1000)
                  (s 1 1)
                  (m 60 1)
                  (h 3600 1)))
         (use-tick-step
          (let ((tick-iter tick-steps))
            (while (and tick-iter
                        (< (/ (* max-width
                                 (car (car tick-iter))
                                 (nth 1 (assq (cdr (car tick-iter))
                                              units)))
                              (* total-time
                                 (nth 2 (assq (cdr (car tick-iter))
                                              units))))
                           min-tick-width))
              (setq tick-iter (cdr tick-iter)))
            (car tick-iter))))
    (list :step (car use-tick-step)
          :num (nth 1 (assq (cdr use-tick-step) units))
          :den (nth 2 (assq (cdr use-tick-step) units))
          :unit (symbol-name (cdr use-tick-step)))))

(defun kite--network2-update-header ()
  "Update the header line for the kite-network buffer to reflect
the current minimum and maximum network event times."
  (let* ((barchart-width (kite--network2-barchart-width))
         (hpos (/ (* (kite--frame-inner-width)
                     (string-width kite--network-header-string))
                  (frame-width)))
         (total-time (- kite--network-max-time
                        kite--network-min-time))
         (header kite--network-header-string)
         (best-tick-step
          (kite--network2-best-tick-step
           barchart-width
           (* 9 (/ (frame-pixel-width)
                   (frame-width)))
           total-time))
         (tick-factor-num (plist-get best-tick-step :num))
         (tick-factor-den (plist-get best-tick-step :den))
         (tick-step (plist-get best-tick-step :step))
         (tick-factor-unit (plist-get best-tick-step :unit))
         (current-tick 0))

    (while (<= (* current-tick tick-factor-num)
               (* total-time tick-factor-den))
      (setq header
            (concat
             header
             (propertize
              kite--network-display-placeholder
              'face
              'kite-table-head
              'display
              (cons 'space
                    (list
                     :align-to
                     (list
                      (+ hpos
                         (/ (* barchart-width
                               current-tick
                               tick-factor-num)
                            (* total-time
                               tick-factor-den)))))))
             (propertize
              kite--network-display-placeholder
              'face 'bg:kite-table-head
              'display '(space . (:width (1))))
             (propertize
              kite--network-display-placeholder
              'face 'kite-table-head
              'display '(space . (:width (3))))
             (propertize
              (concat (number-to-string current-tick)
                      tick-factor-unit)
              'face 'kite-table-head)))
      (setq current-tick (+ current-tick tick-step)))

    (ewoc-set-hf kite--network-ewoc
                 (concat header "\n")
                 "\n")))

(defun kite--network-render-barchart (request)
  "Render the bar chart for the given request and return it as a
string.  REQUEST should be a `kite-request' struct."
  (let*
      (barchart
       (request-times (sort* (kite-request-times request)
                             '< :key 'cdr))
       (time-delta
        (if (= kite--network-max-time
               kite--network-min-time)
            1                           ; avoid division by zero
          (- kite--network-max-time
             kite--network-min-time)))
       (barchart-width (kite--network2-barchart-width))
       (scaled-times
        (cons '(:pageStart . 0)
              (mapcar (lambda (item)
                        (cons (car item)
                              (round
                               (* barchart-width
                                  (/ (- (cdr item)
                                        kite--network-min-time)
                                     time-delta)))))
                      request-times))))
    (setcar (car (last scaled-times)) :requestFinished)
    (while scaled-times
      (let ((left (cdr (nth 0 scaled-times)))
            (right (cdr (nth 1 scaled-times))))
        (when (and (not (null right))
                   (< left right))
          (setq barchart
                (concat
                 barchart
                 (propertize
                  kite--network-display-placeholder
                  'face
                  (intern (concat "bg:kite-"
                                  (substring
                                   (symbol-name
                                    (caar scaled-times))
                                   1)))
                  'display `(space :height (1 . mm)
                                   :width (,(- right left))))))))
      (setq scaled-times (cdr scaled-times)))
    barchart))

(defun kite--network2-render-request (request-index)
  "Render the given request and insert it into the buffer.
Called by `kite--network-ewoc'.  REQUEST-INDEX is the request
index stored in the ewoc node and is used to look up a
`kite-request' in `kite--network--sorted-requests'."
  (let ((inhibit-read-only t)
        (request (nth request-index kite--network-sorted-requests)))
    (when request
      (let ((barchart "<<BARCHART>>"))
        (insert
         (concat
          (kite--fill-overflow (concat
                                (kite-request-method request)
                                " "
                                (kite-request-url request))
                               kite--network-url-width)
          "  "
          (kite--fill-overflow
           (if (and (kite-request-status-code request)
                    (not (= 0 (kite-request-status-code request))))
               (number-to-string (kite-request-status-code request))
             "---") kite--network-status-width)
          "  "
          (kite--fill-overflow
           (if (kite-request-data-length request)
               (file-size-human-readable
                (kite-request-data-length request))
             "") kite--network-size-width)
          "  "
          (kite--network-render-barchart request)
          "\n"))))))

(define-derived-mode kite-network2-mode special-mode "kite-network2"
  "Toggle kite network 2 mode."
  :group 'kite

  (setq show-trailing-whitespace nil)
  (setq line-spacing (max (or line-spacing 0) 2))

  (let ((inhibit-read-only t))
    (erase-buffer)
    (set (make-local-variable 'kite--network-ewoc)
         (ewoc-create
          (symbol-function 'kite--network2-render-request)
          "\n"
          (format "\
Reload the page to show network information.

You can reload the page using the following key binding(s):
%s"
                  (or (mapconcat
                       (lambda (str)
                         (format "`%s'" (key-description str)))
                       (where-is-internal 'kite-reload-page
                                          overriding-local-map
                                          nil
                                          nil)
                       ", ")
                      "`M-x kite-reload-page'"))
          t))
    (set (make-local-variable 'kite--network-sorted-requests) nil)

    (when (> (hash-table-count 
              (kite-session-requests-by-id kite-session))
             0)
      (let (all-times)
        (maphash (lambda (request-id request)
                   (setq all-times
                         (nconc
                          (mapcar 'cdr (kite-request-times request))
                          all-times)))
                 (kite-session-requests-by-id kite-session))
        (set (make-local-variable 'kite--network-min-time)
             (apply 'min all-times))
        (set (make-local-variable 'kite--network-max-time)
             (apply 'max all-times)))
      (kite--network2-update-header)))
  (kite--network-invalidate-in-buffer)
  (run-mode-hooks 'kite-network-mode-hook))

(defun kite--network-invalidate-in-buffer (&optional request)

  ;; Ensure number of ewocs matches number of requests
  (let ((num-ewocs (length (ewoc-collect kite--network-ewoc
                                         (lambda (item) t))))
        (num-requests
         (hash-table-count (kite-session-requests-by-id
                            kite-session))))

    (while (< num-ewocs num-requests)
      (ewoc-enter-last kite--network-ewoc num-ewocs)
      (setq num-ewocs (1+ num-ewocs)))
    (ewoc-filter kite--network-ewoc
                 (lambda (data)
                   (< data num-requests))))

  (let ((updated-times
         (when request
           ;; See if min/max times changes
           (let* ((request-times
                   (mapcar 'cdr (kite-request-times request)))
                  (new-min-time
                   (apply 'min (append
                                (when kite--network-min-time
                                  (list kite--network-min-time))
                                request-times)))
                  (new-max-time
                   (apply 'max (append
                                (when kite--network-max-time
                                  (list kite--network-max-time))
                                request-times)))
                  (updated-times
                   (not (and (eql new-min-time
                                  kite--network-min-time)
                             (eql new-max-time
                                  kite--network-max-time)))))

             (setq kite--network-min-time new-min-time)
             (setq kite--network-max-time new-max-time)

             updated-times))))

    ;; Re-sort requests
    (let ((prev-sorted-requests kite--network-sorted-requests))
      (setq kite--network-sorted-requests
            (sort
             (let (unsorted-request)
               (maphash
                (lambda (key value)
                  (push value unsorted-request))
                (kite-session-requests-by-id kite-session))
               unsorted-request)
             (lambda (request1 request2)
               (string< (kite-request-url request1)
                        (kite-request-url request2)))))

      (if updated-times
          ;; Min/max changed, refresh everything
          (progn
            (kite--network2-update-header)
            (ewoc-refresh kite--network-ewoc))

        ;; Min/max unchanged, see which ewocs need to be
        ;; invalidated
        (let (first-difference
              request-index
              (index 0)
              (old-iter prev-sorted-requests)
              (new-iter kite--network-sorted-requests))
          (while (and new-iter
                      (or (null first-difference)
                          (null request-index)))
            (when (and (null first-difference)
                       (or (null old-iter)
                           (not (equal (kite-request-id
                                        (car old-iter))
                                       (kite-request-id
                                        (car new-iter))))))
              (setq first-difference index))
            (when (and request
                       (equal (kite-request-id (car new-iter))
                              (kite-request-id request)))
              (setq request-index index))
            (setq index (1+ index))
            (setq old-iter (cdr old-iter))
            (setq new-iter (cdr new-iter)))

          ;; Invalidate them, first the ewoc corresponding to the
          ;; invalidated request (unless included below)
          (when (and request-index
                     (or (null first-difference)
                         (< request-index first-difference)))
            (ewoc-invalidate kite--network-ewoc
                             (ewoc-nth kite--network-ewoc
                                       request-index)))

          ;; Then any ewocs added or changed due to updated sort
          ;; order
          (when first-difference
            (loop for index
                  from first-difference
                  to (1- (length kite--network-sorted-requests))
                  do
                  (ewoc-invalidate kite--network-ewoc
                                   (ewoc-nth kite--network-ewoc
                                             index)))))))))

(defun kite--network-invalidate (websocket-url &optional request)
  "Invalidate the display of network requests in the network
buffer corresponding to the given WEBSOCKET-URL, if that buffer
is open.  If REQUEST is given, try to minimize the amount of
redisplay work to be done by only invalidating affected ewoc
nodes."

  (let ((buffer (kite--find-buffer websocket-url 'network2)))
    (when buffer
      (with-current-buffer buffer
        (kite--network-invalidate-in-buffer request)))))


;;; Notification callbacks

(defun kite--Network-requestWillBeSent (websocket-url packet)
  "Respond to `Network.requestWillBeSent' notifications by
creating a new `kite-request' struct, adding it to the session,
and re-rendering the network buffer, if available."

  ;; Hokey test to see if we need to flush the cached requests. FIXME
  (when (and
         (string= (plist-get (plist-get packet :initiator) :type)
                  "other")
         (string= (plist-get packet :frameId)
                  (kite--get (kite-session-frame-tree kite-session)
                             :frame :id))
         (string= (kite--get packet
                             :request :url)
                  (kite--get (kite-session-frame-tree kite-session)
                             :frame :url)))
    (setq kite--network-min-time)
    (setq kite--network-max-time)
    (clrhash (kite-session-requests-by-id kite-session)))

  ;; Create new kite-request
  (let ((request
         (make-kite-request
          :id (plist-get packet :requestId)
          :frame-id (plist-get packet :frameId)
          :loader-id (plist-get packet :loaderId)
          :document-url (plist-get packet :documentUrl)
          :request (plist-get packet :request)
          :request-timestamp (plist-get packet :timestamp)
          :initiator (plist-get packet :initiator)
          :redirect-response (plist-get packet :redirectResponse))))
    (puthash (plist-get packet :requestId)
             request
             (kite-session-requests-by-id kite-session))
    (kite--network-invalidate websocket-url request)))

(defun kite--Network-requestServedFromCache (websocket-url packet)
  "Respond to `Network.requestServedFromCache' notifications by
updating the corresponding `kite-request' struct and re-rendering
the network buffer, if available."
  (let ((request (kite-session-request-for-id
                  kite-session
                  (plist-get packet :requestId))))
    (setf (kite-request-was-served-from-cache-p request) t)
    (kite--network-invalidate websocket-url request)))

(defun kite--Network-responseReceived (websocket-url packet)
  "Respond to `Network.responseReceived' notifications by
updating the corresponding `kite-request' struct and re-rendering
the network buffer, if available."
  (let ((request (kite-session-request-for-id
                  kite-session
                  (plist-get packet :requestId))))
    (setf (kite-request-resource-type request)
          (plist-get packet :type))
    (setf (kite-request-response request)
          (plist-get packet :response))
    (setf (kite-request-response-timestamp request)
          (plist-get packet :timestamp))
    (kite--network-invalidate websocket-url request)))

(defun kite--Network-dataReceived (websocket-url packet)
  "Respond to `Network.dataReceived' notifications by updating
the corresponding `kite-request' struct and re-rendering the
network buffer, if available."
  (let ((request (kite-session-request-for-id
                  kite-session
                  (plist-get packet :requestId))))
    (setf (kite-request-encoded-data-length request)
          (plist-get packet :encodedDataLength))
    (setf (kite-request-data-length request)
          (plist-get packet :dataLength))
    (setf (kite-request-data-received-timestamp request)
          (plist-get packet :timestamp))
    (kite--network-invalidate websocket-url request)))

(defun kite--Network-loadingFinished (websocket-url packet)
  "Respond to `Network.loadingFinished' notifications by updating
the corresponding `kite-request' struct and re-rendering the
network buffer, if available."
  (let ((request (kite-session-request-for-id
                  kite-session
                  (plist-get packet :requestId))))
    (setf (kite-request-loading-finished-timestamp request)
          (plist-get packet :timestamp))
    (kite--network-invalidate websocket-url request)))

(defun kite--Network-loadingFailed (websocket-url packet)
  "Respond to `Network.loadingFailed' notifications by updating
the corresponding `kite-request' struct and re-rendering the
network buffer, if available."
  (let ((request (kite-session-request-for-id
                  kite-session
                  (plist-get packet :requestId))))
    (setf (kite-request-data-error-text request)
          (plist-get packet :errorText))
    (setf (kite-request-data-canceled-p request)
          (eq t (plist-get packet :canceled)))
    (setf (kite-request-loading-failed-timestamp request)
          (plist-get packet :timestamp))
    (kite--network-invalidate websocket-url request)))


;;; Hooks setup

(add-hook 'kite-Network-requestWillBeSent-hooks
          'kite--Network-requestWillBeSent)
(add-hook 'kite-Network-requestServedFromCache-hooks
          'kite--Network-requestServedFromCache)
(add-hook 'kite-Network-responseReceived-hooks
          'kite--Network-responseReceived)
(add-hook 'kite-Network-dataReceived-hooks
          'kite--Network-dataReceived)
(add-hook 'kite-Network-loadingFinished-hooks
          'kite--Network-loadingFinished)
(add-hook 'kite-Network-loadingFailed-hooks
          'kite--Network-loadingFailed)

(provide 'kite-net2)

;;; kite-net2.el ends here
