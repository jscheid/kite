;;; kite-console-tests.el --- Kite test suite for console module

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

;; Kite test suite for the console module.
;;
;; It is part of Kite, a WebKit inspector front-end.


;;; Code:

(defconst kite--console-test-simple-message-1
  '(:message
    (:parameters
     [(:value "test1" :type "string")]
     :repeatCount 1
     :source "console-api"
     :type "log"
     :level "log"
     :text "test1")))

(defconst kite--console-test-simple-message-2
  '(:message
    (:parameters
     [(:value "test1" :type "string")]
     :repeatCount 1
     :source "console-api"
     :type "log"
     :level "log"
     :text "test2")))

(defmacro with--kite-console-test-buffer (&rest body)
  `(with-temp-buffer
     (let (kite-session (inhibit-read-only t))
       (flet ((kite--find-buffer (websocket-url type) (current-buffer))
              (kite--console-update-mode-line ())
              (kite--mode-line-update ())
              (kite-send (&rest ignore)))
         (kite-console-mode)
         ,@body))))

(ert-deftest kite-console-insert-message ()
  "kite-console prints a simple message from the server"

  (with--kite-console-test-buffer
   (kite--console-messageAdded
    nil
    kite--console-test-simple-message-1)
   (kite--console-messageAdded
    nil
    kite--console-test-simple-message-2)
   (kite--console-messageAdded
    nil
    kite--console-test-simple-message-1)
   (kite--console-messageAdded
    nil
    kite--console-test-simple-message-2)

   ;; Text in buffer
   (should (string= (buffer-substring-no-properties
                     (point-min) (point-max))
                    (concat kite-console-header
                            "test1\ntest2\ntest1\ntest2\n"
                            kite-console-prompt)))))

(ert-deftest kite-console-group-nesting ()
  "Nested messages are indented"

  (with--kite-console-test-buffer

   (kite--console-messageAdded
    nil
    kite--console-test-simple-message-1)

   (kite--console-messageAdded
    nil
    (list :message
          (list :type "startGroup")))

   (kite--console-messageAdded
    nil
    kite--console-test-simple-message-2)

   (kite--console-messageAdded
    nil
    (list :message
          (list :type "endGroup")))

   (kite--console-messageAdded
    nil
    kite--console-test-simple-message-1)

   ;; Text in buffer
   (should (string= (buffer-substring-no-properties
                     (point-min) (point-max))
                    (concat kite-console-header
                            "test1\n  test2\ntest1\n"
                            kite-console-prompt)))))

(ert-deftest kite-console-repeat-count ()
  "Repeat count is included in message display"

  (with--kite-console-test-buffer

   (kite--console-messageAdded
    nil
    '(:message
      (:parameters
       [(:value "foo" :type "string")]
       :type "log"
       :level "log"
       :text "foo"
       :repeatCount 1)))

   (kite--console-messageAdded
    nil
    '(:message
      (:parameters
       [(:value "foo" :type "string")]
       :type "log"
       :level "log"
       :text "bar"
       :repeatCount 3)))

   ;; Text in buffer
   (should (string= (buffer-substring-no-properties
                     (point-min) (point-max))
                    (concat kite-console-header
                            "foo\n"
                            "bar [message repeated 3 times]\n"
                            kite-console-prompt)))))

(ert-deftest kite-console-repeat-update ()
  "Repeat count in message display can be updated"

  (with--kite-console-test-buffer

   (kite--console-messageAdded
    nil
    '(:message
      (:parameters
       [(:value "test" :type "string")]
       :type "log"
       :level "log"
       :text "test")))

   (kite--console-messageRepeatCountUpdated
    nil
    (list :count 4))

   (should (string= (buffer-substring-no-properties
                     (point-min) (point-max))
                    (concat kite-console-header
                            "test [message repeated 4 times]\n"
                            kite-console-prompt)))

   (kite--console-messageRepeatCountUpdated
    nil
    (list :count 5))

   (should (string= (buffer-substring-no-properties
                     (point-min) (point-max))
                    (concat kite-console-header
                            "test [message repeated 5 times]\n"
                            kite-console-prompt)))))

(provide 'kite-console-tests)

;;; kite-console-tests.el ends here
