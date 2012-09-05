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

(provide 'kite-global)

;;; kite-global.el ends here
