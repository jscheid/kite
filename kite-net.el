;;; kite-net.el --- Kite network module implementation

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
(require 'cl)
(require 'ewoc)
(require 'websocket)

(defvar kite-requests (make-hash-table :test 'equal))

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

(defvar kite-network-mode-map
  (let ((map (make-keymap))
	(menu-map (make-sparse-keymap)))
    (suppress-keymap map t)
    (kite--define-global-mode-keys map)
    (define-key map "r" 'kite-debug-reload)
    (define-key map (kbd "RET") 'kite-show-network-entry)
    (define-key map (kbd "C-c g") 'kite-net-visit-response)
    map)
  "Local keymap for `kite-network-mode' buffers.")

(defvar kite-dom-content-fired-timestamp)
(defvar kite-ewoc)
(defvar kite-min-time)
(defvar kite-max-time)
(defvar kite-header-width)

(defcustom kite--mime-map
  '(("image/*" image-mode)
    ("text/plain" fundamental-mode)
    ("text/enriched" fundamental-mode)
    ("text/richtext" fundamental-mode)
    ("text/x-patch" diff-mode)
    ("text/x-diff" diff-mode)
    ("application/emacs-lisp" emacs-lisp-mode)
    ("application/x-emacs-lisp" emacs-lisp-mode)
    ("application/x-shellscript" sh-mode)
    ("application/x-sh" sh-mode)
    ("text/x-sh" sh-mode)
    ("application/javascript" js-mode)
    ("text/javascript" js-mode)
    ("text/css" css-mode)
    ("text/dns" dns-mode)
    ("text/x-org" org-mode)
    ("text/html" html-mode)
    ("text/x-coffeescript" coffee-mode))
  "Alist of supported MIME types used when visiting remote
files."
  :group 'kite
  :type '(repeat (list (regexp :tag "MIME type")
                       (function :tag "Display function"))))

(define-derived-mode kite-network-mode special-mode "kite-network"
  "Toggle kite network mode."
  :group 'kite
  (set (make-local-variable 'kill-buffer-hook) 'kite--kill-network)
  (set (make-local-variable 'kite-min-time) nil)
  (set (make-local-variable 'kite-max-time) nil)
  (set (make-local-variable 'kite-header-width) 0)

  (setq show-trailing-whitespace nil)
  (setq case-fold-search nil)
  (setq line-spacing (max (or line-spacing 0) 2))

  (let ((inhibit-read-only t))
    (erase-buffer)
    (set (make-local-variable 'kite-ewoc)
         (ewoc-create (symbol-function 'kite--render-network-entry)
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

                      t)))

  (kite-send "Network.enable")
  (run-mode-hooks 'kite-network-mode-hook))

(defun kite--net-Network-loadingFinished (websocket-url packet)
  "Obsolete. FIXME"
  (kite--log "kite--Network-loadingFinished"))

(defun kite--network-barchart-width ()
  "Return barchart width for current buffer, in pixels."
  (/ (* (frame-pixel-width)
        (- (frame-width) kite-header-width 10))
     (frame-width)))

(defun kite--render-network-entry (request-response)
  "EWOC callback, render a single network entry (the entry for an
individual network resource)."
  (kite--log "ewoc called with request-response %s, min-time %s, max-time %s" request-response kite-min-time kite-max-time)
  (let ((request-method (plist-get (plist-get (cdr (assq 'will-be-sent request-response)) :request) :method))
        (request-url (plist-get (plist-get (cdr (assq 'will-be-sent request-response)) :request) :url))
        (status-code (plist-get (plist-get (plist-get request-response :response-received) :response) :status))
        (response-size
         (let ((result 0) (iter request-response))
           (while iter
             (kite--log "dolist, packet is %s" (car iter))
             (when (eq 'data-received (car (car iter)))
               (setq result (+ result (plist-get (cdr (car iter)) :dataLength))))
             (setq iter (cdr iter)))
           result))
        (inhibit-read-only t))

    (let ((barchart-width (kite--network-barchart-width))
          barchart
          times
          (packets request-response))
      (while packets
        (let ((packet (car packets)))
          (cond
           ((eq 'will-be-sent (car packet))
            (setq times (cons (list 'requestStart (plist-get (cdr packet) :timestamp)) times)))
           ((eq 'response-received (car packet))
            (let* ((timing (plist-get (plist-get (cdr packet) :response) :timing))
                   (request-time (plist-get timing :requestTime))
                   (relative-times '(
                                     :sslEnd
                                     :sslStart
                                     :receiveHeadersEnd
                                     :sendEnd
                                     :sendStart
                                     :connectEnd
                                     :connectStart
                                     :dnsEnd
                                     :dnsStart
                                     :proxyEnd
                                     :proxyStart
                                     )))
              (while relative-times
                (let ((relative-time (plist-get timing (car relative-times))))
                  (when (and (not (null relative-time))
                             (>= relative-time 0))
                    (setq times (cons (list (car relative-times) (+ request-time (/ relative-time 1000))) times))))
                (setq relative-times (cdr relative-times)))))
           ((eq 'data-received (car packet))
            (setq times (cons (list 'dataReceived (plist-get (cdr packet) :timestamp)) times))))
          (setq packets (cdr packets))))
      (let ((scaled-times
             (cons
              (cons 'pageStart 0)
              (mapcar (lambda (x)
                        (cons (nth 0 x)
                              (round
                               (* barchart-width
                                  (/ (- (nth 1 x) kite-min-time)
                                     (if (eq kite-max-time kite-min-time)
                                         1
                                       (- kite-max-time kite-min-time)))))))
                      (sort times (lambda (x y) (< (nth 1 x) (nth 1 y))))))))
        (setcar (car (last scaled-times)) 'requestFinished)
        (while scaled-times
          (let ((left (cdr (nth 0 scaled-times)))
                (right (cdr (nth 1 scaled-times))))
            (when (and (not (null right))
                       (< left right))
              (setq barchart (concat barchart
                                     (propertize "x"
                                                 'face (intern (concat "bg:kite-" (symbol-name (car (car scaled-times)))))
                                                 'display (cons 'space (list :height (cons 1 'mm) :width (list (- right left)))))))))
          (setq scaled-times (cdr scaled-times))))

      (insert
       (concat
        (kite--fill-overflow (concat request-method " " request-url) 50)
        "  "
        (kite--fill-overflow 
         (if status-code
             (number-to-string status-code)
           "---") 3)
        "  "
        (kite--fill-overflow 
         (if (not (null response-size))
             (file-size-human-readable response-size)
           "") 10)
        "  "
        barchart
        "\n")))))

(defun kite--frame-inner-width ()
  "Return inner frame width (sans borders) in pixels."
  (if (fboundp 'window-inside-pixel-edges)
      (- (nth 2 (window-inside-pixel-edges))
         (nth 0 (window-inside-pixel-edges)))
    (frame-pixel-width)))

(defun kite--network-update-header ()
  "Update the header line for the kite-network buffer."
  (let ((header-string (propertize
                        (concat
                         (kite--fill-overflow "Method+URL" 50)
                         "  "
                         (kite--fill-overflow "Sta" 3)
                         "  "
                         (kite--fill-overflow "Size" 10)
                         "  ")
                        'face 'kite-table-head)))

    (setq kite-header-width (string-width header-string))

    (let* ((barchart-width (kite--network-barchart-width))
           (hpos (/ (* (kite--frame-inner-width)
                       kite-header-width)
                    (frame-width)))
           (total-time (- kite-max-time kite-min-time))
           (current-tick 0)
           (tick-steps '((1 . ns)
                         (2 . ns)
                         (5 . ns)
                         (10 . ns)
                         (20 . ns)
                         (50 . ns)
                         (100 . ns)
                         (200 . ns)
                         (500 . ns)
                         (1 . ms)
                         (2 . ms)
                         (5 . ms)
                         (10 . ms)
                         (20 . ms)
                         (50 . ms)
                         (100 . ms)
                         (200 . ms)
                         (500 . ms)
                         (1 . s)
                         (2 . s)
                         (5 . s)
                         (10 . s)
                         (15 . s)
                         (30 . s)
                         (1 . m)
                         (2 . m)
                         (5 . m)
                         (10 . m)
                         (15 . m)
                         (30 . m)
                         (1 . h)
                         (2 . h)
                         (5 . h)
                         (12 . h)))
           (units '((ns 1 1000000)
                    (ms 1 1000)
                    (s 1 1)
                    (m 60 1)
                    (h 3600 1)))
           (use-tick-step
            (let ((tick-iter tick-steps)
                  (min-tick-width (* 9 (/ (frame-pixel-width) (frame-width)))))
              (while (and tick-iter
                          (< (/ (* barchart-width (car (car tick-iter)) (nth 1 (assq (cdr (car tick-iter)) units)))
                                (* total-time (nth 2 (assq (cdr (car tick-iter)) units))))
                             min-tick-width))
                (setq tick-iter (cdr tick-iter)))
              (car tick-iter)))
           (tick-step (car use-tick-step))
           (tick-factor-num (nth 1 (assq (cdr use-tick-step) units)))
           (tick-factor-den (nth 2 (assq (cdr use-tick-step) units)))
           (tick-factor-unit (symbol-name (cdr use-tick-step)))
           (header header-string))

      (while (<= (* current-tick tick-factor-num)
                 (* total-time tick-factor-den))
        (setq header (concat header
                             (propertize "x"
                                         'face 'kite-table-head
                                         'display (cons 'space
                                                        (list :align-to
                                                              (list
                                                               (+ hpos (/ (* barchart-width current-tick tick-factor-num)
                                                                          (* total-time tick-factor-den)))))))
                             (propertize "x"
                                         'face 'bg:kite-table-head
                                         'display '(space . (:width (1))))
                             (propertize "x"
                                         'face 'kite-table-head
                                         'display '(space . (:width (3))))
                             (propertize (concat (number-to-string current-tick) tick-factor-unit)
                                         'face 'kite-table-head)))
        (setq current-tick (+ current-tick tick-step)))

      (ewoc-set-hf kite-ewoc
                   (concat header "\n")
                   "\n"))))

(defun kite--network-update-min-max-time ()
  "Recalculate the time range for display based on timing
information received so far and return whether the range has
changed."
  (with-current-buffer (kite--find-buffer (websocket-url (kite-session-websocket kite-session)) 'network)
    (let (min-time)
      (maphash (lambda (key value)
                 (let ((timestamp (plist-get (cdr (assq 'will-be-sent (ewoc-data (car value)))) :timestamp)))
                   (if (null min-time)
                       (setq min-time timestamp)
                     (setq min-time (min min-time timestamp))))) kite-requests)
      (let ((max-time min-time)
            (relative-times '(:receiveHeadersEnd :sendStart :sendEnd :sslStart :sslEnd :connectStart :connectEnd :dnsStart :dnsEnd :proxyStart :proxyEnd)))
        (maphash (lambda (key value)
                   (let ((packets (ewoc-data (car value))))
                     (kite--log "packet cars: %s" (mapcar (symbol-function 'car) packets))
                     (while packets
                       (kite--log "packets car: %s" (car packets))
                       (kite--log "data-received cdr: %s" (cdr (assq 'data-received (car packets))))
                       (let* ((data-timestamp (and (eq 'data-received (car (car packets)))
                                                   (plist-get (cdr (car packets)) :timestamp)))
                              (timing (and (eq 'response-received (car (car packets)))
                                           (plist-get (plist-get (cdr (car packets)) :response) :timing)))
                              (request-time (plist-get timing :requestTime)))
                         (kite--log "timing is %s" timing)
                         (kite--log "request-time is %s" request-time)
                         (when data-timestamp
                           (setq max-time (max max-time data-timestamp)))
                         (while relative-times
                           (let ((relative-time (plist-get timing (car relative-times))))
                             (when (and (not (null relative-time))
                                        (not (eq -1 relative-time)))
                               (setq max-time (max max-time (+ request-time (/ relative-time 1000))))))
                           (setq relative-times (cdr relative-times))))
                       (setq packets (cdr packets)))))
                 kite-requests)
        (if (and (eq kite-min-time min-time)
                 (eq kite-max-time max-time))
            nil
          (setq kite-min-time min-time)
          (setq kite-max-time max-time)
          t)))))

(defun kite--net-Network-requestWillBeSent (websocket-url packet)
  "Callback invoked when a `Network.requestWillBeSent'
notification has been received from the remote debugger.  Update
timing data and redisplay some or all lines as necessary."
  (with-current-buffer (kite--find-buffer websocket-url 'network)
    (save-excursion
      (let ((inhibit-read-only t))
        (when (and (string= (plist-get (plist-get packet :initiator) :type)
                            "other")
                   (string= (plist-get packet :frameId)
                            (plist-get (plist-get (kite-session-frame-tree kite-session) :frame) :id))
                   (string= (plist-get (plist-get packet :request) :url)
                            (plist-get (plist-get (kite-session-frame-tree kite-session) :frame) :url)))
          (clrhash kite-requests)
          (ewoc-filter kite-ewoc (lambda (x) nil)))
        (let ((ewoc-node (ewoc-enter-last kite-ewoc nil)))
          (puthash (plist-get packet :requestId) (list ewoc-node) kite-requests)
          (ewoc-set-data ewoc-node
                         (list (cons 'will-be-sent packet)))
          (if (kite--network-update-min-max-time)
              (progn
                (kite--network-update-header)
                (ewoc-refresh kite-ewoc))
            (ewoc-invalidate kite-ewoc ewoc-node)))))))

(defun kite--net-Network-responseReceived (websocket-url packet)
  "Callback invoked when a `Network.responseReceived'
notification has been received from the remote debugger.  Update
timing data and redisplay some or all lines as necessary."
  (with-current-buffer (kite--find-buffer websocket-url 'network)
    (save-excursion
      (let ((inhibit-read-only t)
            (request-data (gethash (plist-get packet :requestId) kite-requests)))
        (ewoc-set-data (car request-data)
                       (cons (cons 'response-received packet)
                             (ewoc-data (car request-data))))
        (if (kite--network-update-min-max-time)
            (progn
              (kite--network-update-header)
              (ewoc-refresh kite-ewoc))
          (ewoc-invalidate kite-ewoc (car request-data)))))))

(defun kite--net-Network-dataReceived (websocket-url packet)
  "Callback invoked when a `Network.dataReceived' notification
has been received from the remote debugger.  Update timing data
and redisplay some or all lines as necessary."
  (with-current-buffer (kite--find-buffer websocket-url 'network)
    (save-excursion
      (let ((inhibit-read-only t)
            (request-data (gethash (plist-get packet :requestId) kite-requests)))
        (ewoc-set-data (car request-data)
                       (cons (cons 'data-received packet)
                             (ewoc-data (car request-data))))
        (if (kite--network-update-min-max-time)
            (progn
              (kite--network-update-header)
              (ewoc-refresh kite-ewoc))
          (ewoc-invalidate kite-ewoc (car request-data)))))))

(defun kite--kill-network ()
  "Called when a session's network buffer is closed.  Disables
network notifications in the remote debugger by sending the
`Network.disable' message."
  (ignore-errors
    (kite-send "Network.disable")))

(defun kite--net-Page-domContentEventFired (websocket-url packet)
  "Callback invoked when a `Page.domContentEventFired'
notification is received from the remote debugger.  Remember the
time at which the event was fired and redraw the ewoc.

FIXME: the event time isn't actually rendered yet."
  (let ((network-buffer (kite--find-buffer websocket-url 'network)))
    (when network-buffer
      (with-current-buffer network-buffer
        (save-excursion
          (set (make-local-variable 'kite-dom-content-fired-timestamp) (plist-get packet :timestamp))
          (when (and (boundp 'kite-max-time)
                     (or (null kite-max-time)
                         (> kite-dom-content-fired-timestamp kite-max-time)))
            (setq kite-max-time kite-dom-content-fired-timestamp)
            (ewoc-refresh kite-ewoc)))))))

(defun kite-net-visit-response ()
  "Show the response data corresponding to the network request at
point."
  (interactive)

  (lexical-let*
      ((request-data (ewoc-data
                      (or (and (boundp 'kite-ewoc)
                               (ewoc-locate kite-ewoc))
                          (error "Not in a kite network buffer?"))))
       (request-id (plist-get
                    (cdr (assq 'will-be-sent request-data))
                    :requestId))
       (response (plist-get (cdr (assq 'response-received
                                       request-data))
                            :response))
       (mime-type (plist-get response :mimeType))
       (buffer-mode
        (nth 1 (find-if (lambda (candidate)
                          (string-match (car candidate)
                                        mime-type))
                        kite--mime-map)))
       (buffer-name (format "%s (%s)"
                            (plist-get response :url)
                            request-id))
       (existing-buffer (get-buffer buffer-name))
       (current-buf (current-buffer)))

    (if existing-buffer
        (switch-to-buffer existing-buffer)
      (lexical-let ((new-buffer (generate-new-buffer buffer-name)))
        (switch-to-buffer new-buffer)
        (setq buffer-read-only t)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "Loading, please wait...\n"))
        (with-current-buffer current-buf
          (kite-send
           "Network.getResponseBody"
           :params (list :requestId request-id)
           :success-function
           (lambda (result)
             (with-current-buffer new-buffer
               (let ((inhibit-read-only t))
                 (erase-buffer)
                 (save-excursion
                   (insert (funcall
                            (if (eq t
                                    (plist-get result :base64Encoded))
                                'base64-decode-string 'identity)
                            (plist-get result :body)))))
               (if buffer-mode
                   (funcall buffer-mode)
                 (fundamental-mode)
                 (when mime-type
                   (message "\
Kite doesn't know how to display MIME type %s"
                            mime-type)))))))))))

(add-hook 'kite-Page-domContentEventFired-hooks 'kite--net-Page-domContentEventFired)
(add-hook 'kite-Network-dataReceived-hooks 'kite--net-Network-dataReceived)
(add-hook 'kite-Network-loadingFinished-hooks 'kite--net-Network-loadingFinished)
(add-hook 'kite-Network-responseReceived-hooks 'kite--net-Network-responseReceived)
(add-hook 'kite-Network-requestWillBeSent-hooks 'kite--net-Network-requestWillBeSent)

(provide 'kite-net)

;;; kite-net.el ends here
