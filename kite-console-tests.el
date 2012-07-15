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

(ert-deftest kite-console-insert-message ()
  "kite-console prints a simple message from the server"

  (with-temp-buffer
    (let (kite-session (inhibit-read-only t))
      (kite-console-mode)
      (flet ((kite--console-buffer (websocket-url) (current-buffer)))
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
         kite--console-test-simple-message-2)))

    ;; Text in buffer
    (should (string= (buffer-substring-no-properties
                      (point-min) (point-max))
                     "test1\ntest2\ntest1\ntest2\n"))
    ;; Point still at start of buffer
    (should (eq (point) (point-min)))))

(ert-deftest kite-console-follow ()
  "kite-console does tail-follow if point is at end of buffer,
but not when buffer is empty"

  (with-temp-buffer
    (let (kite-session (inhibit-read-only t))
      (kite-console-mode)
      (flet ((kite--console-buffer (websocket-url) (current-buffer)))

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
        (should (eq (point) (point-max)))))))

(ert-deftest kite-console-group-nesting ()
  "Nested messages are indented"

  (with-temp-buffer
    (let (kite-session (inhibit-read-only t))
      (kite-console-mode)
      (flet ((kite--console-buffer (websocket-url) (current-buffer)))

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
         kite--console-test-simple-message-1)))

    ;; Text in buffer
    (should (string= (buffer-substring-no-properties
                      (point-min) (point-max))
                     "test1\n  test2\ntest1\n"))
    ;; Point still at start of buffer
    (should (eq (point) (point-min)))))


(provide 'kite-console-tests)
