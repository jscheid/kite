(eval-when-compile
  (add-to-list 'load-path (file-name-directory (buffer-file-name)))
  (add-to-list 'load-path (expand-file-name "misc" (file-name-directory (buffer-file-name)))))

(require 'json)
(require 'websocket)
(require 'url-parse)
(eval-when-compile (require 'cl))

(setq websocket-debug t)

(defvar kite-tab-history nil)

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

(defun kite--define-global-mode-keys (map)
  (define-key map "!" 'kite-reload-page))

(require 'kite-dom)
(require 'kite-memory)
(require 'kite-net)
(require 'kite-repl)
(require 'kite-console)
(require 'kite-breakpoint)

(defconst kite--debugger-state-resumed
  (propertize "Resumed" 'face 'success))

(defconst kite--debugger-state-paused
  (propertize "Paused" 'face 'warning))

(defstruct (kite-session)
  websocket
  page-favicon-url
  page-thumbnail-url
  page-url
  page-title
  breakpoint-ewoc
  (script-infos (make-hash-table :test 'equal))
  (debugger-state kite--debugger-state-resumed)
  (next-request-id 0)
  (pending-requests (make-hash-table))
  (buffers nil))

(defstruct (kite-script-info)
  url
  start-line
  start-column
  end-line
  end-column)

(defvar kite-active-sessions
  (make-hash-table :test 'equal :weakness 'value))

(defun kite--format-stacktrace (stacktrace)
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

(defvar kite-connection-mode-map
  (let ((map (make-keymap))
	(ctl-c-b-map (make-keymap))
	(menu-map (make-sparse-keymap)))
    (suppress-keymap map t)
    (kite--define-global-mode-keys map)
    (define-key map "C" 'kite-console)
    (define-key map "p" 'kite-toggle-next-instruction-breakpoint)
    (define-key map "b" 'kite-toggle-exception-breakpoint)
    (define-key map "c" 'kite-debug-continue)
    (define-key map "r" 'kite-debug-reload)
    (define-key map "R" 'kite-repl)
    (define-key map "D" 'kite-dom-inspect)
    (define-key map "N" 'kite-network)
    (define-key map "T" 'kite-timeline)
    (define-key map "M" 'kite-memory)
    (define-key mode-specific-map "b" ctl-c-b-map)
    (define-key ctl-c-b-map "x" 'kite-set-xhr-breakpoint)
    (define-key ctl-c-b-map "d" 'kite-set-dom-event-breakpoint)
    (define-key ctl-c-b-map "i" 'kite-set-instrumentation-breakpoint)
    (define-key ctl-c-b-map "b" 'kite-toggle-exception-breakpoint)
    (define-key ctl-c-b-map "p" 'kite-toggle-next-instruction-breakpoint)
    map)
  "Local keymap for `kite-connection-mode' buffers.")

(defvar kite-debugging-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-ci" 'kite-step-into)
    (define-key map "\C-co" 'kite-step-over)
    (define-key map "\C-cu" 'kite-step-out)
    (define-key map "\C-cp" 'kite-debug-pause)
    map)
  "Local keymap for the `kite-debugging-mode' minor mode")

(defun kite-step-into ()
  (interactive)
  (kite-send "Debugger.pause" nil
             (lambda (response)
               (kite--log "Response to Debugger.pause is %s" response)
               (kite-send "Debugger.stepInto" nil
                          (lambda (response)
                            (kite--log "Response to Debugger.stepInto is %s" response))))))

(defun kite-step-over ()
  (interactive)
  (kite-send "Debugger.stepOver"))

(defun kite-step-out ()
  (interactive)
  (kite-send "Debugger.stepOut"))

(define-minor-mode kite-debugging-mode
  "Toggle kite JavaScript debugging in this buffer."
  :lighter (:eval (kite--debug-stats-mode-line-indicator))
  :keymap 'kite-debugging-mode-map)

(define-derived-mode kite-connection-mode special-mode "kite-connection"
  "Toggle kite connection mode."
  (set (make-local-variable 'kill-buffer-hook) 'kite--kill-buffer)
  (make-local-variable 'kite-tab-alist)
  (setq case-fold-search nil))

(defun kite-debug-pause ()
  (interactive)
  (kite-send "Debugger.pause" nil
             (lambda (response) (kite--log "Execution paused."))))

(defun kite-debug-continue ()
  (interactive)
  (kite-send "Debugger.resume" nil
             (lambda (response) (kite--log "Execution resumed."))))

(defun kite-debug-reload ()
  (interactive)

  (with-current-buffer (if (boundp 'kite-connection)
                           kite-connection
                         (current-buffer))
    (kite-send "Page.reload" nil
               (lambda (response) (kite--log "Page reloaded.")))))

(defun kite-send (method &optional params callback callback-args)
  (let ((callback-buffer (current-buffer))
        (request-id (incf (kite-session-next-request-id kite-session))))
    (puthash request-id (list (or callback (lambda (response) nil))
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
                           json-request))))

(defun kite--session-remove-current-buffer ()
  (setf (kite-session-buffers kite-session)
        (delete (current-buffer) (kite-session-buffers kite-session)))
  (when (null (kite-session-buffers kite-session))
    (websocket-close (kite-session-websocket kite-session))
    (remhash (websocket-url (kite-session-websocket kite-session))
             kite-active-sessions)))

(defun kite--kill-buffer ()
  (ignore-errors
    (kite--session-remove-current-buffer)))

(defun kite--on-message (websocket frame)
  (kite--log "received frame: %s" frame)
  (let ((buf (current-buffer))
        (kite-session (gethash (websocket-url websocket)
                               kite-active-sessions)))
    (when (and (eq (aref frame 0) 'cl-struct-websocket-frame)
               (eq (aref frame 1) 'text))
      (let* ((json-object-type 'plist)
             (response (json-read-from-string (aref frame 2))))

        (kite--log "received response: %s (type %s)"
                   response
                   (type-of response))
        (when (listp response)
          (with-current-buffer buf
            (let ((response-id (plist-get response :id)))
              (if response-id
                  (let ((callback-info
                         (gethash response-id
                                  (kite-session-pending-requests
                                   kite-session))))
                    (remhash response-id (kite-session-pending-requests
                                          kite-session))
                    (when (buffer-live-p (nth 1 callback-info))
                      (with-current-buffer (nth 1 callback-info)
                        (apply (nth 0 callback-info)
                               response
                               (nth 2 callback-info)))))

                (run-hook-with-args
                 (intern
                  (concat "kite-"
                          (replace-regexp-in-string
                           "\\."
                           "-"
                           (plist-get response :method))
                          "-hooks"))
                 (websocket-url websocket)
                 (plist-get response :params))))))))))

(defun kite--on-close (websocket)
  (kite--log "websocket connection closed"))

(defun kite--insert-favicon-async (favicon-url)
  (let ((favicon-marker (point-marker)))
    (url-retrieve
     favicon-url
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
                   (insert-image favicon-image)))))))))))

(defun kite--connect-buffer-insert ()

  (let ((favicon-url (kite-session-page-favicon-url kite-session)))
    (when (and favicon-url
               (not (string= favicon-url "")))
      (kite--insert-favicon-async favicon-url))

    (let* ((inhibit-read-only t)
           (ewoc (ewoc-create
                  (lambda (session)
                    (insert (concat (propertize (concat " " (kite-session-page-title kite-session) "\n\n")
                                                'face 'info-title-1))
                            (propertize "URL: " 'face 'bold)
                            (kite-session-page-url kite-session)
                            "\n"
                            (propertize "Status: " 'face 'bold)
                            (kite-session-debugger-state session)
                            "\n\n"
                            "Press ? for help\n")))))

      (set (make-local-variable 'kite-connection-ewoc) ewoc)

      (ewoc-enter-last ewoc kite-session)

      (goto-char (point-max))
      (setf (kite-session-breakpoint-ewoc kite-session)
            (kite--make-breakpoint-ewoc)))))

(defun kite--connect-webservice (tab-alist)
  (let ((websocket-url (plist-get tab-alist :webSocketDebuggerUrl)))
    (switch-to-buffer
     (get-buffer-create
      (format "*kite %s*" websocket-url)))
    (kite-connection-mode)
    (setq kite-tab-alist tab-alist)

    (set (make-local-variable 'kite-session)
         (make-kite-session
          :websocket (websocket-open
                      websocket-url
                      :on-message (function kite--on-message)
                      :on-close (function kite--on-close))
          :page-favicon-url (plist-get tab-alist :faviconUrl)
          :page-thumbnail-url (plist-get tab-alist :thumbnailUrl)
          :page-url (plist-get tab-alist :url)
          :page-title (plist-get tab-alist :title)))

    (puthash websocket-url kite-session kite-active-sessions)

    (setf (kite-session-buffers kite-session)
          (cons (current-buffer)
                (kite-session-buffers kite-session)))

    (kite--connect-buffer-insert)

    (kite-send "Page.enable" nil
               (lambda (response) (kite--log "Page notifications enabled.")))
    (kite-send "Inspector.enable" nil
               (lambda (response) (kite--log "Inspector enabled.")))
    (kite-send "Debugger.enable" nil
               (lambda (response) (kite--log "Debugger enabled.")))
    (kite-send "CSS.enable" nil
               (lambda (response) (kite--log "CSS enabled.")))
    (kite-send "Debugger.canSetScriptSource" nil
               (lambda (response) (kite--log "got response: %s" response)))))

(defun kite--longest-prefix (strings)
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
          (let* ((debugger-tabs (let ((json-array-type 'list)
                                      (json-object-type 'plist))
                                  (json-read)))
                 (available-debuggers (make-hash-table))
                 (available-strings (make-hash-table :test 'equal))
                 (completion-strings (make-hash-table :test 'equal))
                 (completion-candidates nil))

            ;; Gather debuggers from server response

            (mapcar (lambda (el)
                      (when (plist-member el :webSocketDebuggerUrl)
                        (puthash
                         (plist-get el :webSocketDebuggerUrl)
                         (cons el nil)
                         available-debuggers)))
                    debugger-tabs)

            ;; Gather debuggers currently open

            (maphash (lambda (websocket-url kite-session)
                       (puthash
                        websocket-url
                        `((:webSocketDebuggerUrl
                           ,websocket-url
                           :thumbnailUrl ,(kite-session-page-thumbnail-url kite-session)
                           :faviconUrl ,(kite-session-page-favicon-url kite-session)
                           :title ,(kite-session-page-title kite-session)
                           :url ,(kite-session-page-url kite-session)) . ,kite-session)
                        available-debuggers))
                     kite-active-sessions)

            ;; For each human readable identifier (url or title), see
            ;; if it is ambiguous

            (flet ((add-item (item url)
                             (let ((existing (gethash item available-strings '(0))))
                               (puthash item
                                        (cons (1+ (car existing))
                                              (cons url (cdr existing)))
                                        available-strings))))

              (maphash (lambda (key value)
                         (let ((url (plist-get (car value) :url))
                               (title (plist-get (car value) :title))
                               (websocket-url (plist-get (car value)
                                                         :webSocketDebuggerUrl)))
                           (add-item url websocket-url)
                           (when (not (equal title url))
                             (add-item title websocket-url))))
                       available-debuggers))

            ;; Final pass, disambiguate and rearrange

            (flet ((disambiguate (string websocket-url)
                                 (let ((existing
                                        (gethash string available-strings)))
                                   (if (<= (car existing) 1)
                                       string
                                     (concat string
                                             " ("
                                             (substring websocket-url
                                                        (length (kite--longest-prefix
                                                                 (cdr existing))))
                                             ")")))))

              (maphash (lambda (key value)
                         (let ((url (plist-get (car value) :url))
                               (title (plist-get (car value) :title))
                               (websocket-url (plist-get
                                               (car value)
                                               :webSocketDebuggerUrl)))

                           (puthash (disambiguate url websocket-url)
                                    value
                                    completion-strings)
                           (puthash (disambiguate title websocket-url)
                                    value
                                    completion-strings)))
                       available-debuggers))

            ;; Map to keys

            (maphash (lambda (key value)
                       (setq completion-candidates
                             (cons key completion-candidates)))
                     completion-strings)

            (let ((selection (completing-read
                              "Choose tab: "
                              completion-candidates
                              nil t nil 'kite-tab-history)))
              (when (not (eq 0 (length selection)))
                (if (cdr (gethash selection completion-strings))
                    (switch-to-buffer
                     (car (kite-session-buffers
                           (cdr (gethash selection completion-strings)))))
                  (kite--connect-webservice
                   (car (gethash selection completion-strings)))))))
        (error "Could not contact remote debugger at %s:%s, check host and port%s"
               use-host
               use-port
               (if (> (length (buffer-string)) 0)
                   (concat ": " (buffer-string)) ""))))))


(defun* kite--fill-overflow (string width &key (align 'left) (trim 'right))
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

(defun kite--connection-buffer (websocket-url)
  (format "*kite %s*" websocket-url))

(defun kite--Debugger-resumed (websocket-url packet)
  (with-current-buffer (kite--connection-buffer websocket-url)
    (setf (kite-session-debugger-state kite-session) kite--debugger-state-resumed)))

(defun kite--Debugger-paused (websocket-url packet)
  (with-current-buffer (kite--connection-buffer websocket-url)
    (setf (kite-session-debugger-state kite-session) kite--debugger-state-paused)
    (ewoc-refresh kite-connection-ewoc)
    (let* ((call-frames (plist-get packet :callFrames))
           (first-call-frame (elt call-frames 0))
           (location (plist-get first-call-frame :location))
           (script-info (gethash (plist-get location :scriptId)
                                 (kite-session-script-infos kite-session))))
      (lexical-let ((line-number (- (plist-get location :lineNumber)))
                    (column-number (plist-get location :columnNumber))
                    (kite-session kite-session))
        (kite-visit-script
         script-info
         (lambda ()
           (kite-debugging-mode)
           (set (make-local-variable 'kite-session) kite-session)
           (goto-line line-number)
           (beginning-of-line)
           (forward-char column-number))))
      (message "Debugger paused"))))

(defun kite--create-remote-script-buffer (script-info after-load-function)
  (lexical-let* ((url (kite-script-info-url script-info))
                 (url-parts (url-generic-parse-url url))
                 (after-load-function after-load-function)
                 (new-buffer (generate-new-buffer url)))
    (kite-send "Debugger.getScriptSource" (list (cons 'scriptId (plist-get location :scriptId)))
               (lambda (response)
                 (with-current-buffer new-buffer
                   (setq buffer-file-name (url-filename url-parts))
                   (insert (plist-get (plist-get response :result) :scriptSource))
                   (setq buffer-read-only t)
                   (set-buffer-modified-p nil)
                   (normal-mode)
                   (funcall after-load-function))))
    new-buffer))

(defun kite-visit-script (script-info after-load-function)
  (interactive)
  (let* ((url (kite-script-info-url script-info))
         (url-parts (url-generic-parse-url url)))
    (cond
     ((string= (url-type url-parts) "file")
      (find-file (url-filename url-parts))
      (funcall after-load-function))
     (t
      (switch-to-buffer (or (get-buffer url)
                            (kite--create-remote-script-buffer
                             script-info after-load-function)))))))

(defun kite--Debugger-scriptParsed (websocket-url packet)
  (puthash
   (plist-get packet :scriptId)
   (make-kite-script-info
    :url (plist-get packet :url)
    :start-line (plist-get packet :startLine)
    :start-column (plist-get packet :startColumn)
    :end-line (plist-get packet :endLine)
    :end-column (plist-get packet :endColumn))
   (kite-session-script-infos kite-session)))

(defun kite--debug-stats-mode-line-indicator ()
  "Returns a string to be displayed in the mode line"
  (concat " (" (kite-session-debugger-state kite-session) ")"))

(defun kite-reload-page (&optional arg)
  "Reload the page associated with the current buffer.  With a
prefix argument ARG, ignore (force-refresh) the browser cache."
  (interactive "P")
  (lexical-let ((bool-prefix (not (null arg))))
    (kite-send "Page.reload"
               `((ignoreCache . ,(if bool-prefix t :json-false)))
               (lambda (response)
                 (if bool-prefix
                     (message "Page reloaded (with cache ignored)")
                   (message "Page reloaded"))))))

(add-hook 'kite-Debugger-paused-hooks 'kite--Debugger-paused)
(add-hook 'kite-Debugger-resumed-hooks 'kite--Debugger-resumed)
(add-hook 'kite-Debugger-scriptParsed-hooks 'kite--Debugger-scriptParsed)

(provide 'kite)
