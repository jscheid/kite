(require 'json)
(require 'websocket);
(eval-when-compile (require 'cl))

(require 'kite-dom)

(setq websocket-debug t)

(defvar kite-tab-history nil)

(defface bg:kite-requestStart
  '((t :background "#f00"))
  "Bar chart for requestStart status"
  :version "24.1"
  :group 'ekwd-faces)

(defface bg:kite-receiveHeadersEnd
  '((t :background "#0f0"))
  "Bar chart for receiveHeadersEnd status"
  :version "24.1"
  :group 'ekwd-faces)

(defface bg:kite-sendEnd
  '((t :background "#0ff"))
  "Bar chart for receiveHeadersEnd status"
  :version "24.1"
  :group 'ekwd-faces)

(defface bg:kite-pageStart
  '((t))
  "Bar chart for receiveHeadersEnd status"
  :version "24.1"
  :group 'ekwd-faces)

(defface bg:kite-dataReceived
  '((t :background "#f0f"))
  "Bar chart for receiveHeadersEnd status"
  :version "24.1"
  :group 'ekwd-faces)

(defface bg:kite-tick
  '((t :background "#fff"))
  "Bar chart tick color"
  :version "24.1"
  :group 'ekwd-faces)

(defface kite-table-head
  '((t :inherit highlight))
  "Basic face used to highlight warnings."
  :version "24.1"
  :group 'ekwd-faces)

(defface bg:kite-table-head
  (list (list t :background (face-attribute 'kite-table-head :foreground nil 'default)))
  "Basic face used to highlight warnings."
  :version "24.1"
  :group 'ekwd-faces)

(defface kite-log-warning
  '((t :inherit warning))
  "Basic face used to highlight warnings."
  :version "24.1"
  :group 'ekwd-faces)

(defface kite-log-error
  '((t :inherit error))
  "Basic face used to highlight warnings."
  :version "24.1"
  :group 'ekwd-faces)

(defface kite-log-debug
  '((t :inherit font-lock-comment))
  "Basic face used to highlight warnings."
  :version "24.1"
  :group 'ekwd-faces)

(defface kite-log-log
  '((t :inherit default))
  "Basic face used to highlight warnings."
  :version "24.1"
  :group 'ekwd-faces)

(defface kite-log-tip
  '((t :inherit underline))
  "Basic face used to highlight warnings."
  :version "24.1"
  :group 'ekwd-faces)

(defun --kite-log (format-string &rest args)
  (with-current-buffer
      (get-buffer-create (format "*kite log*"))
    (insert (concat (apply 'format format-string args) "\n"))))

(defun kite-console ()
  (interactive)
  (--kite-log "opening console")
  (lexical-let*
      ((kite-connection (current-buffer))
       (buf (get-buffer-create (format "*kite console %s*" (cdr (assq 'webSocketDebuggerUrl kite-tab-alist))))))
    (with-current-buffer buf
      (kite-console-mode)
      (set (make-local-variable 'kite-connection) kite-connection))
    (switch-to-buffer buf)
    (erase-buffer)
    (save-excursion
      (with-current-buffer kite-connection
        (--kite-log "sending in buffer %s" (current-buffer))
        (--kite-send "Console.enable" nil
                     (lambda (response) (--kite-log "Console enabled.")))))))


(defun --kite-format-stacktrace (stacktrace)
  (let ((formatted "") (index 0))
    (while (< index (length stacktrace))
      (let ((stackframe (elt stacktrace index)))
        (setq formatted
              (concat formatted
                      (format "%s:%s:%s(%s)"
                              (cdr (assq 'url stackframe))
                              (cdr (assq 'lineNumber stackframe))
                              (cdr (assq 'columnNumber stackframe))
                              (cdr (assq 'functionName stackframe)))))
        (setq index (1+ index))))
    formatted))

(defun kite-clear-console ()
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t))
      (erase-buffer))
    (with-current-buffer kite-connection
      (--kite-send "Console.clearMessages" nil
                   (lambda (response) (--kite-log "Console cleared."))))))

(defun kite-show-log-entry ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((log-message (get-text-property (point) 'log-message)))
      (with-output-to-temp-buffer "*kite log message*"
        (princ (format (concat
                        "Origin: %s:%s\n"
                        "Source: %s\n"
                        "Type: %s\n"
                        "Level: %s\n"
                        "Repeat Count: %s\n"
                        "Message:\n\n%s\n\nStack Trace:\n\n%s")
                       (cdr (assq 'url log-message))
                       (cdr (assq 'line log-message))
                       (cdr (assq 'source log-message))
                       (cdr (assq 'type log-message))
                       (cdr (assq 'level log-message))
                       (cdr (assq 'repeatCount log-message))
                       (cdr (assq 'text log-message))
                       (--kite-format-stacktrace (cdr (assq 'stackTrace log-message)))
                       ))))))

(defvar kite-console-mode-map
  (let ((map (make-keymap))
	(menu-map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "X" 'kite-clear-console)
    (define-key map (kbd "RET") 'kite-show-log-entry)
    map)
  "Local keymap for `kite-console-mode' buffers.")

(defvar kite-network-mode-map
  (let ((map (make-keymap))
	(menu-map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "r" 'kite-debug-reload)
    (define-key map (kbd "RET") 'kite-show-network-entry)
    map)
  "Local keymap for `kite-network-mode' buffers.")

(defvar kite-connection-mode-map
  (let ((map (make-keymap))
	(menu-map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "C" 'kite-console)
    (define-key map "p" 'kite-debug-pause)
    (define-key map "c" 'kite-debug-continue)
    (define-key map "r" 'kite-debug-reload)
    (define-key map "R" 'kite-repl)
    (define-key map "D" 'kite-dom-inspect)
    (define-key map "N" 'kite-network)
    (define-key map "T" 'kite-timeline)
    map)
  "Local keymap for `kite-connection-mode' buffers.")

(define-derived-mode kite-connection-mode special-mode "kite-connection"
  "Toggle kite connection mode."
  (set (make-local-variable 'kill-buffer-hook) '--kite-kill-buffer)
  (make-local-variable 'kite-tab-alist)
  (make-local-variable 'kite-pending-requests)
  (make-local-variable 'kite-next-id)
  (make-local-variable 'kite-websocket)
  (setq case-fold-search nil))

(defun kite-debug-pause ()
  (interactive)
  (--kite-send "Debugger.pause" nil
               (lambda (response) (--kite-log "Execution paused."))))

(defun kite-debug-continue ()
  (interactive)
  (--kite-send "Debugger.resume" nil
               (lambda (response) (--kite-log "Execution resumed."))))

(defun kite-debug-reload ()
  (interactive)
  
  (with-current-buffer (if (boundp 'kite-connection)
                           kite-connection
                         (current-buffer))
    (--kite-send "Page.reload" nil
                 (lambda (response) (--kite-log "Page reloaded.")))))

(define-derived-mode kite-console-mode special-mode "kite-console"
  "Toggle kite console mode."
  (set (make-local-variable 'kill-buffer-hook) '--kite-kill-console)
  (hl-line-mode)
  (setq case-fold-search nil))

(define-derived-mode kite-network-mode special-mode "kite-network"
  "Toggle kite network mode."
  (set (make-local-variable 'kill-buffer-hook) '--kite-kill-network)
  (set (make-local-variable 'kite-min-time) nil)
  (set (make-local-variable 'kite-max-time) nil)
  (set (make-local-variable 'kite-header-width) 0)

  (setq show-trailing-whitespace nil)
  (setq case-fold-search nil)
  (setq line-spacing (max (or line-spacing 0) 2)))

(defun --kite-connection-buffer ()
  (if (boundp 'kite-connection)
      kite-connection
    (current-buffer)))

(defun --kite-send (method &optional params callback callback-args)
  (let ((callback-buffer (current-buffer))
        (request-id (with-current-buffer (--kite-connection-buffer)
                      (setq kite-next-id (1+ kite-next-id))))
        (websocket (with-current-buffer (--kite-connection-buffer)
                     kite-websocket)))
    (with-current-buffer (--kite-connection-buffer)
      (puthash request-id (list (or callback (lambda (response) nil))
                                callback-buffer
                                callback-args) kite-pending-requests))
    (websocket-send-text websocket
                         (json-encode
                          (list
                           (cons :jsonrpc "2.0")
                           (cons :method method)
                           (cons :params params)
                           (cons :id request-id)
                           )))))

(defun --kite-kill-buffer ()
  (ignore-errors
    (websocket-close kite-websocket)))

(defun --kite-kill-console ()
  (ignore-errors
    (with-current-buffer kite-connection
      (--kite-send "Console.disable" nil
                   (lambda (response) (--kite-log "Console disabled."))))))

(defun --kite-kill-network ()
  (ignore-errors
    (with-current-buffer kite-connection
      (--kite-send "Network.disable" nil
                   (lambda (response) (--kite-log "Network disabled."))))))

(defun --kite-Console-messageAdded (websocket-url packet)
  (let ((buf (get-buffer (format "*kite console %s*" websocket-url))))
    (when buf
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (propertize (concat (cdr (assq 'text packet)) "\n")
                              'log-message packet
                              'face (intern (format "kite-log-%s" (cdr (assq 'level packet))))))
          (goto-char (point-max))
          (--kite-log "message added, url is %s, packet is %s" websocket-url packet))))))

(defun --kite-Debugger-paused (websocket-url packet)
  (--kite-log "New state: Paused"))

(defun --kite-Debugger-resumed (websocket-url packet)
  (--kite-log "New state: Resumed"))

(defun --kite-Debugger-scriptParsed (websocket-url packet)
  (--kite-log "Script parsed."))

(defun --kite-Network-loadingFinished (websocket-url packet)
  (--kite-log "--kite-Network-loadingFinished"))

(defun --kite-connect-webservice (tab-alist)
  (lexical-let* ((websocket-url (cdr (assq 'webSocketDebuggerUrl tab-alist)))
                 (faviconUrl (cdr (assq 'faviconUrl tab-alist)))
                 (thumbnailUrl (cdr (assq 'thumbnailUrl tab-alist))))
    (--kite-log "connecting to %s" websocket-url)
    (lexical-let ((buf (get-buffer-create (format "*kite %s*" websocket-url)))
                  (favicon-marker nil))
      (save-excursion
        (with-current-buffer buf
          (kite-connection-mode)
          (switch-to-buffer buf)
          (setq kite-tab-alist tab-alist)
          (setq kite-pending-requests (make-hash-table))
          (setq kite-next-id 0)
          (setq kite-websocket
                (websocket-open websocket-url
                                :on-message (lambda (websocket frame)
                                              (--kite-log "received frame: %s" frame)
                                              (when (and (eq (aref frame 0) 'cl-struct-websocket-frame)
                                                         (eq (aref frame 1) 'text))
                                                (let ((response (json-read-from-string (aref frame 2))))
                                                  (when (listp response)
                                                    (with-current-buffer buf
                                                      (let ((response-id (cdr (assq 'id response))))
                                                        (if response-id
                                                            (let ((callback-info (gethash response-id kite-pending-requests)))
                                                              (remhash response-id kite-pending-requests)
                                                              (with-current-buffer (nth 1 callback-info)
                                                                (apply (nth 0 callback-info) (assq-delete-all 'id response) (nth 2 callback-info))))
                                                          (apply (symbol-function (intern
                                                                                   (concat "--kite-"
                                                                                           (replace-regexp-in-string "\\." "-"
                                                                                                                     (cdr (assq 'method response))))))
                                                                 websocket-url
                                                                 (cdr (assq 'params response))
                                                                 nil))))))))

                                :on-close (lambda (websocket)
                                            (--kite-log "websocket connection closed"))))
          (when (and faviconUrl
                     (not (string= faviconUrl "")))
            (url-retrieve faviconUrl
                          (lambda (status)
                            (goto-char 0)
                            (when (and (looking-at "HTTP/1\\.. 200")
                                       (re-search-forward "\n\n" nil t))
                              (ignore-errors
                                (let* ((favicon-image
                                        (create-image (buffer-substring (point) (buffer-size)) nil t)))
                                  (save-excursion
                                    (with-current-buffer buf
                                      (goto-char (marker-position favicon-marker))
                                      (let ((inhibit-read-only t))
                                        (insert-image favicon-image))))))))))
          (setq favicon-marker (point-marker))
          (let* ((inhibit-read-only t)
                 (ewoc (ewoc-create
                        (lambda (x)

            (insert (concat (propertize (concat " " (cdr (assq 'title kite-tab-alist)) "\n\n") 'face 'info-title-1))
                    (propertize "URL: " 'face 'bold)
                    (cdr (assq 'url kite-tab-alist))
                    "\n"
                    (propertize "Status: " 'face 'bold)
                    (propertize "Running" 'face 'success)
                    "\n\n"
                    "Press ? for help\n")))))

            (set (make-local-variable 'ekwd-connection-ewoc) ewoc)

            (ewoc-enter-last ewoc 0)


            (--kite-send "Page.enable" nil
                         (lambda (response) (--kite-log "Page notifications enabled.")))
            (--kite-send "Inspector.enable" nil
                         (lambda (response) (--kite-log "Inspector enabled.")))
            (--kite-send "Debugger.enable" nil
                         (lambda (response) (--kite-log "Debugger enabled.")))
            (--kite-send "CSS.enable" nil
                         (lambda (response) (--kite-log "CSS enabled.")))
            (--kite-send "Debugger.canSetScriptSource" nil
                         (lambda (response) (--kite-log "got response: %s" response)))
            ))))))

(defun --kite-longest-prefix (strings)
  "Return the longest prefix common to all the given STRINGS,
which should be a sequence of strings.  Naive implementation."
  (if (null strings)
      ""
    (let ((max-length (length (car strings))))
      (while (let ((prefix-candidate (substring (car strings) 0 max-length)))
               (not (every (apply-partially 'string-prefix-p prefix-candidate) strings)))
        (setq max-length (- max-length 1)))
      (substring (car strings) 0 max-length))))

(defun kite-connect (&optional host port)
  (interactive)
  (let ((url-request-method "GET")
        (url
         (url-parse-make-urlobj "http" nil nil (or host "127.0.0.1") (or port 9222) "/json")))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char 0)
      (if (and (looking-at "HTTP/1\\.. 200")
               (re-search-forward "\n\n" nil t))
          (let* ((debugger-tabs (let ((json-array-type 'list)) (json-read)))
                 (available-debuggers (make-hash-table))
                 (available-strings (make-hash-table :test 'equal))
                 (completion-strings (make-hash-table :test 'equal))
                 (completion-candidates nil))

            ; Gather debuggers from server response

            (mapcar (lambda (el)
                      (when (assq 'webSocketDebuggerUrl el)
                        (puthash
                         (cdr (assq 'webSocketDebuggerUrl el))
                         (cons el nil)
                         available-debuggers)))
                    debugger-tabs)

            ; Gather debuggers currently open

            (mapcar (lambda (buf)
                      (with-current-buffer buf
                        (when (and (eq major-mode 'kite-connection-mode)
                                   (websocket-openp kite-websocket))
                          (puthash
                           (cdr (assq 'webSocketDebuggerUrl kite-tab-alist))
                           (cons kite-tab-alist buf)
                           available-debuggers))))
                    (buffer-list))

            ; For each human readable identifier (url or title), see
            ; if it is ambiguous

            (flet ((add-item (item url)
                             (let ((existing (gethash item available-strings '(0))))
                               (puthash item (cons (1+ (car existing)) (cons url (cdr existing))) available-strings))))

              (maphash (lambda (key value)
                         (let ((url (cdr (assq 'url (car value))))
                               (title (cdr (assq 'title (car value))))
                               (websocket-url (cdr (assq 'webSocketDebuggerUrl (car value)))))
                           (add-item url websocket-url)
                           (when (not (equal title url))
                             (add-item title websocket-url))))
                       available-debuggers))

            ; Final pass, disambiguate and rearrange

            (flet ((disambiguate (string websocket-url)
                                 (let ((existing (gethash string available-strings)))
                                   (if (<= (car existing) 1)
                                       string
                                     (concat string " (" (substring websocket-url (length (--kite-longest-prefix (cdr existing))))  ")")))))

              (maphash (lambda (key value)
                         (let ((url (cdr (assq 'url (car value))))
                               (title (cdr (assq 'title (car value))))
                               (websocket-url (cdr (assq 'webSocketDebuggerUrl (car value)))))

                           (puthash (disambiguate url websocket-url) value completion-strings)
                           (puthash (disambiguate title websocket-url) value completion-strings)))
                       available-debuggers))

            ; Map to keys

            (maphash (lambda (key value)
                       (setq completion-candidates (cons key completion-candidates)))
                     completion-strings)

            (let ((selection (completing-read
                              "Choose tab: "
                              completion-candidates
                              nil t nil 'kite-tab-history)))

              (if (cdr (gethash selection completion-strings))
                  (switch-to-buffer (cdr (gethash selection completion-strings)))
                (--kite-connect-webservice (car (gethash selection completion-strings))))))
        (error "Could not contact remote debugger, check host and port: %s" (buffer-string))))))


(defun --kite-Page-loadEventFired (websocket-url packet)
  t)

(defun --kite-Debugger-globalObjectCleared (websocket-url packet)
  t)

(defun* --kite-fill-overflow (string width &key (align 'left) (trim 'right))
  (let ((string-length (length string)))
    (if (> string-length width)
        (if (eq 'right trim)
            (concat (substring string 0 (- width 3)) "...")
          (concat "..." (substring string (- string-length (- width 3)))))
      (let ((fill (- width string-length)))
        (cond
         ((eq 'left align)
          (concat string (make-string fill 32)))
         ((eq 'right align)
          (concat (make-string fill 32) string))
         (t
          (let* ((left-fill (/ fill 2))
                 (right-fill (- fill left-fill)))
            (concat (make-string left-fill 32)
                    string
                    (make-string left-fill 32)))))))))

(defun --kite-network-barchart-width ()
  (/ (* (frame-pixel-width)
        (- (frame-width) kite-header-width 10))
     (frame-width)))

(defun --ekwd-render-network-entry (request-response)
  (--kite-log "ewoc called with request-response %s" request-response)
  (let ((request-method (cdr (assq 'method (cdr (assq 'request (cdr (assq 'will-be-sent request-response)))))))
        (request-url (cdr (assq 'url (cdr (assq 'request (cdr (assq 'will-be-sent request-response)))))))
        (status-code (cdr (assq 'status (cdr (assq 'response (cdr (assq 'response-received request-response)))))))
        (response-size
         (let ((result 0) (iter request-response))
           (while iter
             (--kite-log "dolist, packet is " (car iter))
             (when (eq 'data-received (car (car iter)))
               (setq result (+ result (cdr (assq 'dataLength (cdr (car iter)))))))
             (setq iter (cdr iter)))
           result))
        (inhibit-read-only t))

    (let ((barchart-width (--kite-network-barchart-width))
          barchart
          times
          (packets request-response))
      (while packets
        (let ((packet (car packets)))
          (cond
           ((eq 'will-be-sent (car packet))
            (setq times (cons (list 'requestStart (cdr (assq 'timestamp (cdr packet)))) times)))
           ((eq 'response-received (car packet))
            (let* ((timing (cdr (assq 'timing (cdr (assq 'response (cdr packet))))))
                   (request-time (cdr (assq 'requestTime timing)))
                   (relative-times '(
                                     sslEnd
                                     sslStart
                                     receiveHeadersEnd
                                     sendEnd
                                     sendStart
                                     connectEnd
                                     connectStart
                                     dnsEnd
                                     dnsStart
                                     proxyEnd
                                     proxyStart
                                     )))
              (while relative-times
                (let ((relative-time (cdr (assq (car relative-times) timing))))
                  (when (and (not (null relative-time))
                             (>= relative-time 0))
                    (setq times (cons (list (car relative-times) (+ request-time (/ relative-time 1000))) times))))
                (setq relative-times (cdr relative-times)))))
           ((eq 'data-received (car packet))
            (setq times (cons (list 'dataReceived (cdr (assq 'timestamp (cdr packet)))) times))))
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
        (--kite-fill-overflow (concat request-method " " request-url) 50)
        "  "
        (--kite-fill-overflow 
         (if status-code
             (number-to-string status-code)
           "---") 3)
        "  "
        (--kite-fill-overflow 
         (if (not (null response-size))
             (file-size-human-readable response-size)
           "") 10)
        "  "
        barchart
        "\n")))))

(defun --kite-frame-inner-width ()
  (if (fboundp 'window-inside-pixel-edges)
      (- (nth 2 (window-inside-pixel-edges))
         (nth 0 (window-inside-pixel-edges)))
    (frame-pixel-width)))

(defun --kite-network-update-header ()
  (let ((header-string (propertize
                        (concat
                         (--kite-fill-overflow "Method+URL" 50)
                         "  "
                         (--kite-fill-overflow "Sta" 3)
                         "  "
                         (--kite-fill-overflow "Size" 10)
                         "  ")
                        'face 'kite-table-head)))

    (setq kite-header-width (string-width header-string))

    (let* ((barchart-width (--kite-network-barchart-width))
           (hpos (/ (* (--kite-frame-inner-width)
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

(defun kite-network ()
  (interactive)
  (--kite-log "opening network")
  (lexical-let*
      ((kite-connection (current-buffer))
       (buf (get-buffer-create (format "*kite network %s*" (cdr (assq 'webSocketDebuggerUrl kite-tab-alist))))))
    (with-current-buffer buf
      (kite-network-mode)

      (let ((inhibit-read-only t))
        (erase-buffer)
        (set (make-local-variable 'kite-ewoc)
             (ewoc-create (symbol-function '--ekwd-render-network-entry)
                          ""
                          "\nReload the page to show network information\n" t)))

      (set (make-local-variable 'kite-connection) kite-connection)
      (set (make-local-variable 'kite-requests) (make-hash-table :test 'equal)))
    (switch-to-buffer buf)
    (save-excursion
      (with-current-buffer kite-connection
        (--kite-log "sending in buffer %s" (current-buffer))
        (--kite-send "Network.enable" nil
                     (lambda (response) (--kite-log "Network enabled.")))))))

(defun --kite-network-update-min-max-time ()
  (with-current-buffer (format "*kite network %s*" websocket-url)
    (let (min-time)
      (maphash (lambda (key value)
                 (let ((timestamp (cdr (assq 'timestamp (cdr (assq 'will-be-sent (ewoc-data (car value))))))))
                   (if (null min-time)
                       (setq min-time timestamp)
                     (setq min-time (min min-time timestamp))))) kite-requests)
      (let ((max-time min-time)
            (relative-times '(receiveHeadersEnd sendStart sendEnd sslStart sslEnd connectStart connectEnd dnsStart dnsEnd proxyStart proxyEnd)))
        (maphash (lambda (key value)
                   (let ((packets (ewoc-data (car value))))
                     (--kite-log "packet cars: %s" (mapcar (symbol-function 'car) packets))
                     (while packets
                       (--kite-log "packets car: %s" (car packets))
                       (--kite-log "data-received cdr: %s" (cdr (assq 'data-received (car packets))))
                       (let* ((data-timestamp (and (eq 'data-received (car (car packets)))
                                                   (cdr (assq 'timestamp (cdr (car packets))))))
                              (timing (and (eq 'response-received (car (car packets)))
                                           (cdr (assq 'timing (cdr (assq 'response (cdr (car packets))))))))
                              (request-time (cdr (assq 'requestTime timing))))
                         (when data-timestamp
                           (setq max-time (max max-time data-timestamp)))
                         (while relative-times
                           (let ((relative-time (cdr (assq (car relative-times) timing))))
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

(defun --kite-Network-requestWillBeSent (websocket-url packet)
  (with-current-buffer (format "*kite network %s*" websocket-url)
    (let ((inhibit-read-only t))
      (when (string= (cdr (assq 'url (cdr (assq 'request packet))))
                     (cdr (assq 'documentURL packet)))
        (clrhash kite-requests)
        (ewoc-filter kite-ewoc (lambda (x) nil)))
      (goto-char (point-max))
      (let ((ewoc-node (ewoc-enter-last kite-ewoc nil)))
        (puthash (cdr (assq 'requestId packet)) (list ewoc-node) kite-requests)
        (ewoc-set-data ewoc-node
                       (list (cons 'will-be-sent packet))))
      (if (--kite-network-update-min-max-time)
          (progn
            (--kite-network-update-header)
            (ewoc-refresh kite-ewoc))
        (ewoc-invalidate kite-ewoc (car request-data))))))

(defun --kite-Network-responseReceived (websocket-url packet)
  (with-current-buffer (format "*kite network %s*" websocket-url)
    (let ((inhibit-read-only t)
          (request-data (gethash (cdr (assq 'requestId packet)) kite-requests)))
      (ewoc-set-data (car request-data)
                     (cons (cons 'response-received packet)
                           (ewoc-data (car request-data))))
      (if (--kite-network-update-min-max-time)
          (progn
            (--kite-network-update-header)
            (ewoc-refresh kite-ewoc))
        (ewoc-invalidate kite-ewoc (car request-data))))))

(defun --kite-Network-dataReceived (websocket-url packet)
  (with-current-buffer (format "*kite network %s*" websocket-url)
    (let ((inhibit-read-only t)
          (request-data (gethash (cdr (assq 'requestId packet)) kite-requests)))
      (ewoc-set-data (car request-data)
                     (cons (cons 'data-received packet)
                           (ewoc-data (car request-data))))
      (if (--kite-network-update-min-max-time)
          (progn
            (--kite-network-update-header)
            (ewoc-refresh kite-ewoc))
        (ewoc-invalidate kite-ewoc (car request-data))))))

(defun --kite-Page-domContentEventFired (websocket-url packet) 
  (let ((network-buffer (get-buffer (format "*kite network %s*" websocket-url))))
    (when network-buffer
      (with-current-buffer network-buffer
        (set (make-local-variable 'kite-dom-content-fired-timestamp) (cdr (assq 'timestamp packet)))
        (when (and (boundp 'kite-max-time)
                   (or (null kite-max-time)
                       (> kite-dom-content-fired-timestamp kite-max-time)))
          (setq kite-max-time kite-dom-content-fired-timestamp)
          (ewoc-refresh kite-ewoc))))))

(defun --kite-Page-frameNavigated (websocket-url packet)
  t)

(defun --kite-CSS-mediaQueryResultChanged (websocket-url packet)
  t)

(defun --kite-Inspector-inspect (websocket-url packet)
  (lexical-let ((websocket-url websocket-url))
    (--kite-send "DOM.requestNode" (list (assq 'objectId (cdr (assq 'object packet))))
                 (lambda (response)
                   (with-current-buffer (--kite-dom-buffer websocket-url)
                     (kite-dom-goto-node
                      (cdr (assq 'nodeId (cdr (assq 'result response))))))))))


(defun kite-repl ()
  (interactive)
  (--kite-log "opening repl")
  (lexical-let*
      ((kite-connection (current-buffer))
       (buf (get-buffer-create (format "*kite repl %s*" (cdr (assq 'webSocketDebuggerUrl kite-tab-alist))))))
    (with-current-buffer buf
      (kite-repl-mode)
      (set (make-local-variable 'kite-connection) kite-connection))
    (switch-to-buffer buf)))

(defvar kite-repl-mode-map
  (let ((map (make-keymap))
	(menu-map (make-sparse-keymap)))
    (define-key map (kbd "C-M-x") 'kite-eval-defun)
    (define-key map (kbd "C-c C-c") 'kite-repl-eval)
    map)
  "Local keymap for `kite-repl-mode' buffers.")

(define-derived-mode kite-repl-mode javascript-mode "kite-repl"
  "Toggle kite repl mode."
  t)


(defun kite-eval-defun ()
  (save-excursion
    (let (begin end pstate defun-info temp-name defun-body)
      (js-end-of-defun)
      (setq end (point))
      (js--ensure-cache)
      (js-beginning-of-defun)
      (re-search-forward "\\_<function\\_>")
      (setq begin (match-beginning 0))
      (setq pstate (js--forward-pstate))

      (when (or (null pstate)
                (> (point) end))
        (error "Could not locate function definition"))

      (setq defun-info (js--guess-eval-defun-info pstate))
      (setq defun-body (buffer-substring-no-properties begin end))

      (message "defun-info=%s   defun-body: %s"))))


(defun kite-repl-eval ()
  (interactive)
  (save-excursion

    (lexical-let* ((buf (current-buffer))
                   (begin
                    (progn
                      (if (re-search-backward "^///" nil t)
                          (progn
                            (next-line)
                            (beginning-of-line))
                        (goto-char (point-min)))
                      (point)))

                   (end
                    (progn
                      (if (re-search-forward "^///" nil t)
                          (beginning-of-line)
                        (goto-char (point-max)))
                      (point)))

                   (code (buffer-substring-no-properties begin end)))

      (with-current-buffer kite-connection
        (--kite-send "Runtime.evaluate" (list (cons 'expression code))
                     (lambda (response)
                       (let ((result (cdr (assq 'result response))))
                         (message "result %s" result)
                         (if (eq :json-false (cdr (assq 'wasThrown result)))
                             (with-current-buffer buf
                               (save-excursion
                                 (goto-char end)
                                 (insert (format "/// -> %S\n" (or (cdr (assq 'value (cdr (assq 'result result))))
                                                                   (intern (cdr (assq 'type (cdr (assq 'result result))))))))))

                           (--kite-send "Runtime.getProperties" (list (cons 'objectId (cdr (assq 'objectId (cdr (assq 'result result))))))
                                        (lambda (response)
                                          (mapcar (lambda (x)
                                                    (when (string= "stack" (cdr (assq 'name x)))
                                                      (--kite-send "Runtime.getProperties" (list (cons 'objectId (cdr (assq 'objectId (cdr (assq 'get x))))))
                                                                   (lambda (response)
                                                                     (message "got stack %s" response)))))
                                                  (cdr (assq 'result (cdr (assq 'result response)))))
                                          ))))))))))

                                            ;(insert (format "///! %s\n" (cdr (assq 'description (cdr (assq 'result result))))))))))))))))


(provide 'kite)
