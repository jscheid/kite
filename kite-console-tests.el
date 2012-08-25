;;; kite-console-tests.el --- Kite test suite for console module

;; Copyright (C) 2012 Julian Scheid

;; Author: Julian Scheid
;; Keywords: tools, WWW

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

(defvar kite--console-test-simple-message-1
  (list :message
        (list :type "log"
              :level "log"
              :text "test1")))

(defvar kite--console-test-simple-message-2
  (list :message
        (list :type "log"
              :level "log"
              :text "test2")))

(defmacro with--kite-console-test-buffer (&rest body)
  `(with-temp-buffer
     (let (kite-session (inhibit-read-only t))
       (kite-console-mode)
       (flet ((kite--console-buffer (websocket-url) (current-buffer)))
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
                    "test1\ntest2\ntest1\ntest2\n"))
   ;; Point still at start of buffer
   (should (eq (point) (point-min)))))

(ert-deftest kite-console-follow ()
  "kite-console does tail-follow if point is at end of buffer,
but not when buffer is empty"

  (with--kite-console-test-buffer

   (kite--console-messageAdded
    nil
    kite--console-test-simple-message-1)

   (should (string= (buffer-substring-no-properties
                     (point-min) (point-max))
                    "test1\n"))

   ;; Point still at start of buffer
   (should (eq (point) (point-min)))

   (kite--console-messageAdded
    nil
    kite--console-test-simple-message-2)

   (should (string= (buffer-substring-no-properties
                     (point-min) (point-max))
                    "test1\ntest2\n"))

   ;; Point still at start of buffer
   (should (eq (point) (point-min)))

   (goto-char (point-max))

   ;; Point now at end of buffer
   (should (eq (point) (point-max)))

   (kite--console-messageAdded
    nil
    kite--console-test-simple-message-1)

   (should (string= (buffer-substring-no-properties
                     (point-min) (point-max))
                    "test1\ntest2\ntest1\n"))

   ;; Point still at end of buffer
   (should (eq (point) (point-max)))))

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
                    "test1\n  test2\ntest1\n"))

   ;; Point still at start of buffer
   (should (eq (point) (point-min)))))

(ert-deftest kite-console-repeat-count ()
  "Repeat count is included in message display"

  (with--kite-console-test-buffer

   (kite--console-messageAdded
    nil
    (list :message
          (list :type "log"
                :level "log"
                :text "foo"
                :repeatCount 1)))

   (kite--console-messageAdded
    nil
    (list :message
          (list :type "log"
                :level "log"
                :text "bar"
                :repeatCount 3)))

   ;; Text in buffer
   (should (string= (buffer-substring-no-properties
                     (point-min) (point-max))
                    (concat
                     "foo\n"
                     "bar [message repeated 3 times]\n")))))

(ert-deftest kite-console-repeat-update ()
  "Repeat count in message display can be updated"

  (with--kite-console-test-buffer

   (kite--console-messageAdded
    nil
    (list :message
          (list :type "log"
                :level "log"
                :text "test")))

   (kite--console-messageRepeatCountUpdated
    nil
    (list :count 4))

   (should (string= (buffer-substring-no-properties
                     (point-min) (point-max))
                    "test [message repeated 4 times]\n"))

   (kite--console-messageRepeatCountUpdated
    nil
    (list :count 5))

   (should (string= (buffer-substring-no-properties
                     (point-min) (point-max))
                    "test [message repeated 5 times]\n"))))

(provide 'kite-console-tests)
