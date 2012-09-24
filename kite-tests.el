;;; kite-tests.el --- Kite test suite

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

;; Top-level package for the Kite test suite.
;;
;; It is part of Kite, a WebKit inspector front-end.

(require 'kite)
(require 'kite-cl)
(require 'kite-dom-tests)
(require 'kite-breakpoint-tests)
(require 'kite-console-tests)
(require 'kite-color-tests)

(ert-deftest kite-test-find-frame-by-id ()
  "kite--frame-by-id works"

  (flet
   ((websocket-open (&rest ignore) (websocket-inner-create :conn t :url t))
    (kite--console-update-mode-line ())
    (kite--find-buffer (&rest ignore))
    (kite-send (method &rest keyword-args)
               (when (string= method "Page.getResourceTree")
                 (funcall (plist-get keyword-args :success-function)
                          '(:frameTree (:childFrames [(:resources [] :frame (:securityOrigin null :name "" :parentId "12583.1" :mimeType "text/html" :url "file:///Users/julians/src/kite/misc/page1.html" :loaderId "12583.4" :id "12583.2"))] :resources [] :frame (:securityOrigin null :mimeType "text/html" :url "file:///Users/julians/src/kite/misc/twoframes.html" :loaderId "12583.3" :id "12583.1")))))))
   
   (kite--connect-webservice (list :webSocketDebuggerUrl "dummy")))

  (should (kite--equal-wildcard
           (kite--frame-by-id "12583.1")
           '(:securityOrigin null :mimeType "text/html" :url "file:///Users/julians/src/kite/misc/twoframes.html" :loaderId "12583.3" :id "12583.1")))

  (should (kite--equal-wildcard
           (kite--frame-by-id "12583.2")
           '(:securityOrigin null :name "" :parentId "12583.1" :mimeType "text/html" :url "file:///Users/julians/src/kite/misc/page1.html" :loaderId "12583.4" :id "12583.2"))))

(provide 'kite-tests)

;;; kite-tests.el ends here
