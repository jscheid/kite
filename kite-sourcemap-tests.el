;;; kite-sourcemap-tests.el --- Kite test suite for source map decoding

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

;; Kite test suite for source map decoding.
;;
;; It is part of Kite, a WebKit inspector front-end.


;;; Code:

(ert-deftest kite-test-base64-decoding ()
  "Base64 decoding works"

  (should (eq (kite--base64-decode ?A) 0))
  (should (eq (kite--base64-decode ?Z) 25))
  (should (eq (kite--base64-decode ?a) 26))
  (should (eq (kite--base64-decode ?z) 51))
  (should (eq (kite--base64-decode ?0) 52))
  (should (eq (kite--base64-decode ?9) 61))
  (should (eq (kite--base64-decode ?+) 62))
  (should (eq (kite--base64-decode ?/) 63))
  (should-error (kite--base64-decode ?%)))

(ert-deftest kite-test-from-vlq-signed ()
  "VLQ sign conversion works"
  (should (eq (kite--from-vlq-signed 2) 1))
  (should (eq (kite--from-vlq-signed 3) -1))
  (should (eq (kite--from-vlq-signed 4) 2))
  (should (eq (kite--from-vlq-signed 5) -2)))

(ert-deftest kite-test-vlq-decoding ()
  "VLQ decoding works"

  (should (equal (kite--base64-vlq-decode (string-to-list "A"))
                 `(0 ,(string-to-list ""))))

  (should (equal (kite--base64-vlq-decode (string-to-list "zA"))
                 `(-9 ,(string-to-list ""))))

  (should (equal (kite--base64-vlq-decode (string-to-list "zza"))
                 `(-13625 ,(string-to-list ""))))

  (should (equal (kite--base64-vlq-decode (string-to-list "gzX"))
                 `(12080 ,(string-to-list "")))))

(defconst kite-test-source-map-json
  (list :version 3
        :file "test.coffee"
        :sources '["test.coffee"]
        :names []
        :mappings "\
AAAC;;;EAAA,eAAK,IAAL,GAAa,SAAA,CAAA,QAAA,CAAA;;;MACV,2BAAmB,YAAnB,aAAA,\
CAAA,KAAA,CAAA;QAAI,OAAe;QAAT;oBACR,QAAQ,KAAR,CAAc,IAAd,EAAoB,KAApB;;;;;\
EAED,GAAA,GAAM,CAAA;AAAA,IAAC,CAAD;AAAA,IAAG,CAAH;AAAA,IAAK,CAAL;AAAA,\
EAAA;EACV,MAAA,GAAS,GAAG,IAAH,CAAQ,SAAA,CAAA,IAAA,CAAA;WAAU,IAAA,CAAA,\
CAAA,CAAO;GAAzB;EAET,OAAO,IAAP,CAAY,MAAZ"))

(ert-deftest kite-test-source-map ()
  "Source map decoding works"
  (let ((source-map (kite--source-map-decode
                     kite-test-source-map-json)))

    (should (equal (kite-source-map-sources source-map) '["test.coffee"]))
    (should (equal (kite-source-map-names source-map) '[]))
    (should (eq (length (kite-source-map-generated-mappings source-map)) 62))
    (let ((mapping (elt (kite-source-map-generated-mappings source-map) 20)))
      (should (kite-source-mapping-p mapping))
      (should (eq (kite-source-mapping-generated-column mapping) 28))
      (should (eq (kite-source-mapping-generated-line mapping) 10))
      (should (string= (kite-source-mapping-source mapping) "test.coffee"))
      (should (eq (kite-source-mapping-original-column mapping) 14))
      (should (eq (kite-source-mapping-original-line mapping) 3))
      (should (null (kite-source-mapping-name mapping))))))

(ert-deftest kite-test-source-map-lookup ()
  "Source map lookup works"
  (let ((source-map (kite--source-map-decode
                     kite-test-source-map-json)))

    (should (equal (kite-source-map-original-position-for source-map 23 10)
                   (list :source "test.coffee"
                         :line 8
                         :column 8
                         :name nil)))))

(ert-deftest kite-test-source-map-reverse-lookup ()
  "Source map reverse lookup works"
  (let ((source-map (kite--source-map-decode
                     kite-test-source-map-json)))

    (should (equal (kite-source-map-generated-position-for source-map "test.coffee" 8 8)
                   (list :line 23
                         :column 9
                         :name nil)))))

(provide 'kite-sourcemap-tests)

;;; kite-sourcemap-tests.el ends here
