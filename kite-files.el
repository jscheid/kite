;;; kite-files.el --- Kite support function relating to resources

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

;; This package provides various helper functions used throughout Kite
;; for purposes of accessing remote resources and opening them in a
;; buffer, similar to what `find-file' does but with additional
;; functionality for making these buffers as useful as possible.
;;
;; It is part of Kite, a WebKit inspector front-end.



;; -------------------------------------------------------------------
;; NOTE: THIS CODE IS UNFINISHED AND NOT USABLE YET AS-IS.
;; -------------------------------------------------------------------




;;; Code:

(require 'url-http)

(defcustom kite-resolve-url-file-function
  'kite-resolve-url-file
  "Callback function invoked with two parameters, KITE-SESSION
and URL.  It should return the full path to a file on the local
file system corresponding to URL in the context of KITE-SESSION,
if known, or nil otherwise.

Kite knowing the local file corresponding to a URL is useful for
purposes of live-editing: any changes you make can both be sent
to the remote debugger and written to your local file system.

The default value `kite-resolve-url-file' only knows how to
resolve URLs using the `file' protocol.  You should override this
with a function that knows about your local project structures as
it relates to your development web server setup.

FIXME: it would probably be useful if there was a mapping from
URL regex to local project base directory, and the default
function would (perhaps recursively) search the project directory
for a file with the same name as the URL file name.  In this day
and age the performance impact of a recursive search would
probably be neglible and this way, most project layouts could be
accommodated without requiring the user to implement the logic
herself."
  :group 'kite)

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

(defun kite-session-script-info-for-url (url)
  "Return the script-info entry for the given URL in the session
bound to `kite-session', or nil if not found."
  (let (result)
    (maphash (lambda (key value)
               (when (string= url (kite-script-info-url value))
                 (setq result value)))
             (kite-session-script-infos kite-session))
    result))

(defun kite-resolve-url-file (kite-session url)
  "Default for `kite-find-file-for-url-function'.  Returns the
file name part of URLs with the `file' protocol, otherwise
returns nil."
  (let ((url-parts (url-generic-parse-url url)))
    (when (string= (url-type url-parts) "file")
      (url-filename url-parts))))

(defun kite-visit ()
  "Visit a resource associated with the current or most recent
Kite session.  Provides completion for all known resources, which
includes resources directly or indirectly loaded by the page, and
resources associated via source maps.

Resources are loaded from the remote debugger if possible, which
means that the buffer contents is how it is seen by the debugged
session (this is normally only relevant for dynamically generated
resources.)  Source mapped files, however, are normally not
requested by the debugged session and therefore will usually have
to be loaded afresh.

If Kite can determine that the resource corresponds to a file on
the local filesystem (per `kite-find-file-for-url-function'), the
local file is opened instead.  If the local file (or the contents
of a buffer visiting that file) differs from the resource as seen
by the debugging session you will be prompted for which action to
take.

A good effort is made to choose the right major mode for the
resulting buffer.  First an attempt is made to choose the major
mode based on the MIME type associated with the resource, then
Kite falls back to guessing the mode based on the file name.

The resulting buffer may be further decorated by Kite based on
its contents:

- Script resource buffers get the minor mode `kite-script-mode'
  enabled so that breakpoints can be shown and manipulated and
  debugger commands (e.g. `kite-continue-to-location') work.  If
  the script is source mapped, the buffer is also decorated so
  that point movement moves the point accordingly in linked
  buffers.

- CSS resource buffers get the minor mode `kite-css-mode' enabled
  so that changes to the CSS code can be sent to the server."
  (interactive)
  (let ((kite-session (kite--select-session)))
    (let (urls
          (url-map (make-hash-table :test 'equal)))
      ;; Resources
      (maphash
       (lambda (request-id request)
         (push (kite-request-url request) urls))
       (kite-session-requests-by-id kite-session))
      ;; Scripts
      (maphash
       (lambda (key script-info)
         (let ((url (kite-script-info-url script-info)))
           (when (and url
                      (> (length url) 0) )
             (push url urls)
             (puthash url (cons script-info nil) url-map)

             (let ((source-map
                    (kite-script-info-maybe-source-map script-info)))
               (when source-map
                 (dolist (source (kite-source-map-sources
                                  source-map))
                   (let ((source-url
                          (kite--url-expand-file-name
                           source
                           url)))
                     (push source-url urls)
                     (puthash source-url
                              (cons script-info source)
                              url-map))))))))
       (kite-session-script-infos kite-session))

      (let ((chosen-url (completing-read "Visit resource: "
                                         urls
                                         nil
                                         t
                                         (kite--longest-prefix
                                          (mapcar #'url-basepath urls))
                                         'kite-visit-history)))
        (when (> (length chosen-url) 0)
          (let ((script-info-and-source
                 (gethash chosen-url url-map)))
          (kite-visit-script
           (car script-info-and-source)
           nil                          ; line
           nil                          ; column
           nil                          ; after-load-function
           (cdr script-info-and-source))))))))


(defun kite-visit-script (script-info
                          &optional
                          line
                          column
                          after-load-function
                          source)
  "Visit the script described by the given SCRIPT-INFO and, once
loaded, move point to LINE and COLUMN and execute
AFTER-LOAD-FUNCTION with the new buffer current.  If a source map
is available, go to the original location instead."
  (interactive)
  (let* ((source-map (kite-script-info-maybe-source-map script-info))
         (original-source (kite-script-info--original-source
                           script-info
                           (or line 1)
                           (or column 0))))
    (let ((after-load
           (lambda ()
             (when target-line
               (goto-char (point-min))
               (forward-line
                (1- target-line))
               (beginning-of-line)
               (when target-column
                 (forward-char target-column)))
             (set (make-local-variable 'kite--buffer-source)
                  (plist-get original-source :source))
             (set (make-local-variable 'kite--buffer-source-map)
                  (plist-get original-source :source-map))
             (set (make-local-variable 'kite--buffer-url)
                  file-url)
             (when (plist-get original-source :source-map)
               (kite--do-buffer-source-mapping
                (plist-get original-source :source-map)
                (kite-script-info-url script-info)
                (not (null source))))
             (when after-load-function
               (funcall after-load-function)))))
      (if (null source)
          (let ((target-line line)
                (target-column column)
                (file-url (kite-script-info-url script-info)))
            (kite-visit-url (kite-script-info-url script-info)
                            after-load))
        (let* ((original-source (kite-script-info--original-source
                                 script-info
                                 (or line 1)
                                 (or column 0)))
               (file-url (plist-get original-source :url))
               (url-parts (url-generic-parse-url file-url))
               (target-line (and line (plist-get original-source :line)))
               (target-column (and column (plist-get original-source :column))))
          (kite-visit-url file-url after-load))))))


(defun kite--do-buffer-source-mapping (source-map
                                       generated-url
                                       originalp)
  "Set text properties in current buffer that allow quick lookup
of generated (or original) source."
  (save-excursion
    (save-restriction
      (flet ((make-point-entered-fun
              (mapping)
              (if originalp
                  `(lambda (old new)
                     (kite--source-map-point-motion
                      ,generated-url
                      ,(kite-source-mapping-generated-line
                        mapping)
                      ,(kite-source-mapping-generated-column
                        mapping)))
                (let* ((relative-url
                        (kite--url-expand-file-name
                         (kite-source-mapping-source prev-mapping)
                         generated-url)))
                  `(lambda (old new)
                     (kite--source-map-point-motion
                      ,relative-url
                      ,(kite-source-mapping-original-line
                        mapping)
                      ,(kite-source-mapping-original-column
                        mapping)))))))
        (let ((was-modified-p (buffer-modified-p))
              (line-function
               (if source
                   #'kite-source-mapping-original-line
                 #'kite-source-mapping-generated-line))
              (column-function
               (if source
                   #'kite-source-mapping-original-column
                 #'kite-source-mapping-generated-column)))
          (widen)
          (goto-char (point-min))
          (let ((line 1)
                (column 0)
                prev-mapping
                prev-point
                (inhibit-modification-hooks t)
                (inhibit-point-motion-hooks t))
            (mapc
             (lambda (mapping)
               (let ((next-line
                      (funcall line-function mapping))
                     (next-column
                      (funcall column-function mapping)))
                 (if (eq next-line line)
                     (forward-char (- next-column column))
                   (backward-char column)
                   (forward-line (- next-line line))
                   (forward-char next-column))
                 (setq line next-line)
                 (setq column next-column)
                 (when prev-mapping
                   (put-text-property
                    prev-point (point)
                    'point-entered
                    (make-point-entered-fun prev-mapping)))
                 (setq prev-mapping mapping)
                 (setq prev-point (point))))
             (if (null source)
                 (kite-source-map-generated-mappings source-map)
               (remove-if
                (lambda (mapping)
                  (not (string= (kite-source-mapping-source mapping)
                                source)))
                (kite-source-map-generated-mappings source-map))))
            (goto-char (point-max))
            (put-text-property prev-point (point)
                               'point-entered
                               (make-point-entered-fun prev-mapping)))
          (set-buffer-modified-p was-modified-p))))))

(defun kite--create-url-buffer (url)
  "Create a buffer suitable for showing the contents URL.  Picks
a name and sets initial buffer-local variables."
  (get-buffer-create url))

(defun kite--network-request-for-url (url)
  nil)

(defun kite-visit-url (url &optional after-load-url-function)
  "Create a buffer showing the contents of URL, then invoke
AFTER-LOAD-URL-FUNCTION with the buffer current.  If there is
already a buffer showing the contents of that URL, just switch to
the buffer (instead of creating a second buffer for the same URL)
but also invoke AFTER-LOAD-URL-FUNCTION.  This implies that
AFTER-LOAD-URL-FUNCTION should be idempotent.

The resource contents are fetched from the debugging session if
possible.  This is accomplished in the following way:

- If the URL corresponds to a `kite-script-info' that Kite knows
  about, use the `Debugger.getScriptSource' RPC request to fetch
  the script contents.

- Otherwise, if the URL corresponds to a network resource that
  Kite knows about, use the `Network.getResponseBody' RPC request
  to fetch the resource contents.

- Otherwise, use `url-retrieve' to fetch the resource.

Further action is taken depending on the resource type: for
scripts, `kite-script-mode' is enabled, and for CSS stylesheets
`kite-css-mode' is enabled.  For source mapped files, text
properties are added to enable tracking point in related buffers.

Also, `kite-resolve-url-file-function' is invoked and
`buffer-file-name' is set accordingly.  If the resource is known
to correspond to a local file, the resource contents is compared
to the file contents: if they differ, the user is asked whether
she wants to use the local file contents instead."
  (lexical-let*
      ((lex-after-load-url-function after-load-url-function)
       (post-initialize
        (lambda (mime-type)
          (let ((buffer-mode
                 (nth 1 (find-if (lambda (candidate)
                                   (string-match (car candidate)
                                                 mime-type))
                                 kite--mime-map))))
            (when buffer-mode
              (funcall buffer-mode)))
          (when lex-after-load-url-function
            (funcall lex-after-load-url-function)))))
    (let ((existing-buffer (kite--find-buffer-visiting-url url)))
      (if existing-buffer
          ;; Have an existing buffer for the URL, switch to it and
          ;; invoke after-load-url-function
          (progn
            (switch-to-buffer existing-buffer)
            (post-initialize))
        (lexical-let ((buffer (kite--create-url-buffer url))
                      (script-info
                       (kite-session-script-info-for-url url)))
          (switch-to-buffer buffer)
          (if script-info
              ;; URL corresponds to a script
              (kite-send "Debugger.getScriptSource"
                         :params
                         `(:scriptId ,(kite-script-info-id
                                       script-info))
                         :success-function
                         (lambda (result)
                           (save-excursion
                             (insert (plist-get result
                                                :scriptSource)))
                           (set-buffer-modified-p nil)
                           (post-initialize "text/javascript")))
            (let ((request (kite--network-request-for-url url)))
              (if request
                  ;; URL corresponds to a network resource
                  (kite-send "Network.getResponseBody"
                             :params
                             `(requestId ,kite-request-id request)
                             :success-function
                             (lambda (result)
                               (save-excursion
                                 (insert
                                  (funcall
                                   (if (eq t
                                           (plist-get result
                                                      :base64Encoded))
                                       'base64-decode-string
                                     'identity)
                                   (plist-get result :body)))
                                 (set-buffer-modified-p nil)
                                 (post-initialize
                                  (kite-request-mime-type)))))
                ;; URL doesn't correspond to either script or network
                ;; request
                (let ((url-http-attempt-keepalives t))
                  (url-retrieve
                   url
                   (lambda (&rest ignore)
                     (re-search-forward "\n\n")
                     (let* ((contents (buffer-substring (point)
                                                        (point-max)))
                            (headers-end (point))
                            (mime-type
                             (progn
                               (goto-char (point-min))
                               (when (re-search-forward
                                      "^Content-Type: \\([^;]*\\)")
                                 (match-string 1)))))
                       (with-current-buffer buffer
                         (save-excursion
                           (insert contents))
                         (set-buffer-modified-p nil)
                         (funcall post-initialize mime-type))))
                   nil                  ; cbargs
                   t                    ; silent
                   t                    ; inhibit-cookies
                   ))))))))))

(defun kite--find-buffer-visiting-url (url)
  "Return the Kite buffer visiting URL."
  (find-if (lambda (buffer)
             (with-current-buffer buffer
               (and (boundp 'kite--buffer-url)
                    (string= kite--buffer-url url))))
           (buffer-list)))

(defun kite--source-map-point-motion (source line column)
  "Invoked via the `point-entered' text property in buffers that
are source mapped.  Moves the point in the 'opposite' buffer if
that buffer is open in any other windows or frames, where the
'opposite' buffer is the generated source code (if the current
buffer shows the original source code), or the original source
code (if the current buffer shows the generated source code).

FIXME: should make the exact behaviour with regard to frames and
windows configurable, by introducing
`kite-source-map-move-point-function' or something."
  (ignore-errors
    (unless (boundp 'kite-source-map-moving-point)
      (let ((kite-source-map-moving-point t)
            (buffer (kite--find-buffer-visiting-url source)))
        (when buffer
          (let ((buffer-point
                 (with-current-buffer buffer
                   (save-excursion
                     (goto-char (point-min))
                     (forward-line (1- line))
                     (forward-char (1- column))
                     (point)))))
            (dolist (window (get-buffer-window-list buffer))
              (set-window-point window buffer-point))))))))


(defun kite-script-info-maybe-source-map (script-info &optional silentp)
  (condition-case err
      (kite-script-info--source-map-cached script-info)
    (error
     ;; In case of error, display error and fall back to
     ;; generated source
     (unless silentp
       (message (cdr err)))
     nil)))

(defun kite-script-info--generated-source (script-info
                                           source
                                           line
                                           column)
  "Return generated line and column corresponding to the given
SCRIPT-INFO, SOURCE, LINE, and COLUMN.  The generated location is
returned as a plist with keys `:line' and `:column'."
  (let ((source-map
         (condition-case err
             (kite-script-info--source-map-cached script-info)
           (error
            ;; In case of error, display error and fall back to
            ;; generated source
            (message (cdr err))
            nil))))
    (if source-map
        (let ((original-pos
               (kite-source-map-generated-position-for
                source-map
                source
                line
                column)))
          (list :line (plist-get original-pos :line)
                :column (plist-get original-pos :column)))
      (list :line line
            :column column))))


(defun kite--create-remote-script-buffer (script-info
                                          after-load-function)
  (lexical-let* ((url (kite-script-info-url script-info))
                 (url-parts (url-generic-parse-url url))
                 (after-load-function after-load-function)
                 (new-buffer (generate-new-buffer url)))
    (kite-send "Debugger.getScriptSource"
               :params
               (list :scriptId (plist-get script-info :scriptId))
               :success-function
               (lambda (result)
                 (with-current-buffer new-buffer
                   (setq buffer-file-name (url-filename url-parts))
                   (insert (plist-get result :scriptSource))
                   (setq buffer-read-only t)
                   (set-buffer-modified-p nil)
                   (normal-mode)
                   (funcall after-load-function))))
    new-buffer))

(defun kite-script-info--source-map (script-info)
  "Return the parsed source map for the given SCRIPT-INFO as a
`kite-source-map' struct, or nil if there is no source map for
the SCRIPT-INFO.  Raise an error if the source map can't be
loaded or parsed."
  (when (kite-script-info-source-map-url script-info)
    (with-current-buffer
        (url-retrieve-synchronously
         (kite--url-expand-file-name
          (kite-script-info-source-map-url script-info)
          (kite-script-info-url script-info)))
      (goto-char 0)
      (if (and (or (looking-at "HTTP/1\\.. 200")
                   (not (looking-at "HTTP/")))
               (re-search-forward "\n\n" nil t))
          (kite--source-map-decode
           (let ((json-array-type 'list)
                 (json-object-type 'plist))
             (json-read)))
        (error "Could not retrieve source map: %s"
               (buffer-substring-no-properties
                (point-min) (point-max)))))))

(defun kite-script-info--source-map-cached (script-info)
  "Return the parsed source map for the given SCRIPT-INFO as a
`kite-source-map' struct, or nil if there is no source map for
the SCRIPT-INFO.  Raise an error if the source map can't be
loaded or parsed.  Uses a cache in the session so that each
source map is loaded and parsed only once."
  (when (kite-script-info-source-map-url script-info)
    (let ((cached-entry
           (gethash (kite-script-info-source-map-url script-info)
                    (kite-session-source-map-cache kite-session))))
      (cond
       ((kite-source-map-p cached-entry)
        cached-entry)
       ((consp cached-entry)
        (signal (car cached-entry) (cdr cached-entry)))
       (t
        (condition-case err
            (puthash (kite-script-info-source-map-url script-info)
                     (kite-script-info--source-map script-info)
                     (kite-session-source-map-cache kite-session))
          (error
           (puthash (kite-script-info-source-map-url script-info)
                    err
                    (kite-session-source-map-cache kite-session))
           (signal (car err) (cdr err)))))))))

(defun kite-script-info--original-source (script-info
                                          line
                                          column
                                          &optional
                                          ignore-source-map-p)
  "Return original URL, line, and column corresponding to the
given SCRIPT-INFO, LINE, and COLUMN.  The original location is
returned as a plist with keys `:url', `:source', `:line' and
`:column'."
  (let ((source-map
         (and (not ignore-source-map-p)
              (condition-case err
                  (kite-script-info--source-map-cached script-info)
                (error
                 ;; In case of error, display error and fall back to
                 ;; generated source
                 (message (cdr err))
                 nil)))))
    (if source-map
        (let ((original-pos
               (kite-source-map-original-position-for
                source-map
                line
                column)))
          (list :url
                (kite--url-expand-file-name
                 (plist-get original-pos :source)
                 (kite-script-info-url script-info))
                :source (plist-get original-pos :source)
                :source-map source-map
                :line (plist-get original-pos :line)
                :column (plist-get original-pos :column)))
      (list :url (kite-script-info-url script-info)
            :source nil
            :source-map nil
            :line line
            :column column))))

(provide 'kite-files)

;;; kite-files.el ends here
