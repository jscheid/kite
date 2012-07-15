(eval-when-compile
  (add-to-list 'load-path (file-name-directory (buffer-file-name)))
  (add-to-list 'load-path (expand-file-name "misc" (file-name-directory (buffer-file-name)))))

(require 'kite)
(require 'kite-dom-tests)
(require 'kite-breakpoint-tests)
(require 'kite-console-tests)

(provide 'kite-tests)
