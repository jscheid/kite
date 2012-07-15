(ert-deftest kite-console-insert-message ()
  "kite-console prints a simple message from the server"

  (with-temp-buffer
    (let (kite-session (inhibit-read-only t))
      (kite-console-mode)
      (flet ((kite--console-buffer (websocket-url) (current-buffer)))
        (kite--console-Console-messageAdded
         "dummy"
         (list :message
               (list :type "log"
                     :level "log"
                     :text "test")))))
    (should (string= (buffer-substring-no-properties
                      (point-min) (point-max))
                     "test\n"))))

