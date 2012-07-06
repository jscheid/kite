(require 'json)
(require 'websocket);
(eval-when-compile (require 'cl))

(require 'kite-dom)
(require 'kite-net)
(require 'kite-repl)

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
  (let* ((url-request-method "GET")
         (use-host (or host "127.0.0.1"))
         (use-port (or port 9222))
         (url
          (url-parse-make-urlobj "http" nil nil use-host use-port "/json")))
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
        (error "Could not contact remote debugger at %s:%s, check host and port%s" use-host use-port
               (if (> (length (buffer-string)) 0)
                 (concat ": " (buffer-string)) ""))))))


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


(provide 'kite)
