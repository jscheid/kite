;;; kite-color-tests.el --- Kite test suite for color helpers

;; Copyright (C) 2012 Julian Scheid

;; Author: Julian Scheid <julians37@gmail.com>
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

;; Kite test suite for color helpers.
;;
;; It is part of Kite, a WebKit inspector front-end.


;;; Code:

(ert-deftest kite-test-color-regex ()
  (dolist (color-spec
           '(;; 4.1. Example I
             "black"
             "white"
             "maroon"
             "olive"

             ;; 4.2.1 Example I
             "#f00"
             "#ff0000"
             "rgb(255,0,0)"
             "rgb(100%, 0%, 0%)"

             ;; 4.2.1 Example I (sic)
             "rgb(255,0,0)"
             "rgb(300,0,0)"
             "rgb(255,-10,0)"
             "rgb(110%, 0%, 0%)"

             ;; 4.2.2 Example I
             "rgb(255,0,0)"
             "rgba(255,0,0,1)"
             "rgb(100%,0%,0%)"
             "rgba(100%,0%,0%,1)"

             ;; 4.2.2 Example I (sic)
             "rgba(0,0,255,0.5)"
             "rgba(100%, 50%, 0%, 0.1)"

             ;; 4.2.4 Example I
             "hsl(0, 100%, 50%)"
             "hsl(120, 100%, 50%)"
             "hsl(120, 100%, 25%)"
             "hsl(120, 100%, 75%)"
             "hsl(120, 50%, 50%)"

             ;; 4.2.5 Example I
             "hsl(120, 100%, 50%)"
             "hsla(120, 100%, 50%, 1)"

             ;; 4.2.5 Example I (sic)
             "hsla(240, 100%, 50%, 0.5)"
             "hsla(30, 100%, 50%, 0.1)"
             ))
    (should (eq 0 (string-match
                   kite-color-regexp
                   color-spec)))))

(ert-deftest kite-test-color-keywords ()
  (should (equal '(0.0 0.0 0.0)
                 (kite-parse-color "black"))))

(ert-deftest kite-test-hex3 ()
  (should (equal '(0.0 0.0 0.0)
                 (kite-parse-color "#000")))
  (should (equal '(1.0 0.0 0.0)
                 (kite-parse-color "#f00")))
  (should (equal '(0.0 1.0 0.0)
                 (kite-parse-color "#0f0")))
  (should (equal '(0.0 0.0 1.0)
                 (kite-parse-color "#00f")))
  (should (equal '(1.0 1.0 1.0)
                 (kite-parse-color "#fff")))
  (should (equal '(1.0 1.0 1.0)
                 (kite-parse-color "#FFF")))
  (should (equal `(0.0 0.0 ,(/ 1.0 15.0))
                 (kite-parse-color "#001"))))

(ert-deftest kite-test-hex6 ()
  (should (equal '(0.0 0.0 0.0)
                 (kite-parse-color "#000000")))
  (should (equal '(1.0 0.0 0.0)
                 (kite-parse-color "#ff0000")))
  (should (equal '(0.0 1.0 0.0)
                 (kite-parse-color "#00ff00")))
  (should (equal '(0.0 0.0 1.0)
                 (kite-parse-color "#0000ff")))
  (should (equal '(1.0 1.0 1.0)
                 (kite-parse-color "#ffffff")))
  (should (equal '(1.0 1.0 1.0)
                 (kite-parse-color "#FfFfFf"))))

(ert-deftest kite-test-rgb-integer ()
  ;; basic tests
  (should (equal '(0.0 0.0 0.0)
                 (kite-parse-color "rgb(0,0,0)")))
  (should (equal '(1.0 0.0 0.0)
                 (kite-parse-color "rgb(255,0,0)")))
  (should (equal '(0.0 1.0 0.0)
                 (kite-parse-color "rgb(0,255,0)")))
  (should (equal '(0.0 0.0 1.0)
                 (kite-parse-color "rgb(0,0,255)")))
  (should (equal '(1.0 1.0 1.0)
                 (kite-parse-color "rgb(255,255,255)")))

  ;; float division
  (should (equal `(0.0 0.0 ,(/ 1.0 255.0))
                 (kite-parse-color "rgb(0,0,1)")))

  ;; whitespace
  (should (equal '(0.0 0.0 0.0)
                 (kite-parse-color "rgb(  0  ,  0  ,  0  )")))

  ;; negative values
  (should (equal '(0.0 0.0 0.0)
                 (kite-parse-color "rgb(-0, -0, -0)")))

  ;; clamping
  (should (equal '(0.0 1.0 1.0)
                 (kite-parse-color "rgb(-100, 500, 123456)"))))

(ert-deftest kite-test-rgb-percent ()
  (should (equal '(0.0 0.0 0.0)
                 (kite-parse-color "rgb(0%,0%,0%)")))
  (should (equal '(1.0 0.0 0.0)
                 (kite-parse-color "rgb(100%,0%,0%)")))
  (should (equal '(0.0 1.0 0.0)
                 (kite-parse-color "rgb(0%,100%,0%)")))
  (should (equal '(0.0 0.0 1.0)
                 (kite-parse-color "rgb(0%,0%,100%)")))
  (should (equal '(1.0 1.0 1.0)
                 (kite-parse-color "rgb(100%,100%,100%)")))
  (should (equal `(0.0 0.0 ,(/ 1.0 100.0))
                 (kite-parse-color "rgb(0%,0%,1%)")))
  (should (equal '(0.0 0.0 0.0)
                 (kite-parse-color "rgb(  0%  ,  0%  ,  0%  )")))
  (should (equal '(0.0 1.0 1.0)
                 (kite-parse-color "rgb(-20%, 101%, 1000%)"))))

(ert-deftest kite-test-rgba-integer ()
  (should (equal '(0.0 0.0 0.0 0.0)
                 (kite-parse-color "rgba(0,0,0,-0.2)")))
  (should (equal '(1.0 0.0 0.0 0.4)
                 (kite-parse-color "rgba(255,0,0,0.4)")))
  (should (equal '(0.0 1.0 0.0 0.6)
                 (kite-parse-color "rgba(0,255,0,0.6)")))
  (should (equal '(0.0 0.0 1.0 0.8)
                 (kite-parse-color "rgba(0,0,255,.8)")))
  (should (equal '(1.0 1.0 1.0 1.0)
                 (kite-parse-color "rgba(255,255,255,1)")))
  (should (equal `(0.0 0.0 ,(/ 1.0 255.0) 0.01)
                 (kite-parse-color "rgba(0,0,1,0.01)")))
  (should (equal '(0.0 0.0 0.0 1.0)
                 (kite-parse-color "rgba(  0  ,  0  ,  0,  1.   )"))))

(ert-deftest kite-test-rgba-percent ()
  (should (equal '(0.0 0.0 0.0 0.0)
                 (kite-parse-color "rgba(0%,0%,0%,-0.2)")))
  (should (equal '(1.0 0.0 0.0 0.4)
                 (kite-parse-color "rgba(100%,0%,0%,0.4)")))
  (should (equal '(0.0 1.0 0.0 0.6)
                 (kite-parse-color "rgba(0%,100%,0%,0.6)")))
  (should (equal '(0.0 0.0 1.0 0.8)
                 (kite-parse-color "rgba(0%,0%,100%,.8)")))
  (should (equal '(1.0 1.0 1.0 1.0)
                 (kite-parse-color "rgba(100%,100%,100%,1)")))
  (should (equal `(0.0 0.0 ,(/ 1.0 100.0) 0.01)
                 (kite-parse-color "rgba(0%,0%,1%,0.01)")))
  (should (equal '(0.0 0.0 0.0 1.0)
                 (kite-parse-color "rgba(  0%  ,  0%  ,  0%,  1.   )"))))

(ert-deftest kite-test-hsl ()
  (should (equal '(1.0 0.0 0.0)
                 (kite-parse-color "hsl(0, 100%, 50%)")))
  (should (equal '(0.0 1.0 0.0)
                 (kite-parse-color "hsl(120, 100%, 50%)")))
  (should (equal '(0.5 1.0 0.5)
                 (kite-parse-color "hsl(120, 100%, 75%)")))
  (should (equal '(0.25 0.75 0.25)
                 (kite-parse-color "hsl(120, 50%, 50%)")))
  (should (equal '(0.0 0.0 1.0)
                 (kite-parse-color "hsl(240, 200%, 50%)")))
  (should (equal '(0.5 0.5 0.5)
                 (kite-parse-color "hsl(0, -50%, 50%)")))
  (should (equal '(0.0 0.0 1.0)
                 (kite-parse-color "hsl(600, 200%, 50%)")))
  (should (equal '(0.0 0.0 1.0)
                 (kite-parse-color "hsl(-120, 200%, 50%)"))))

(ert-deftest kite-test-hsla ()
  (should (equal '(1.0 0.0 0.0 0.0)
                 (kite-parse-color "hsla(0, 100%, 50%, 0.)")))
  (should (equal '(0.0 1.0 0.0 1.0)
                 (kite-parse-color "hsla(120, 100%, 50%, 2.0)")))
  (should (equal '(0.5 1.0 0.5 0.0)
                 (kite-parse-color "hsla(120, 100%, 75%, -10)")))
  (should (equal '(0.25 0.75 0.25 0.5)
                 (kite-parse-color "hsla(120, 50%, 50%, .5)")))
  (should (equal '(0.0 0.0 1.0 0.0)
                 (kite-parse-color "hsla(240, 200%, 50%, 0)")))
  (should (equal '(0.5 0.5 0.5 0.0)
                 (kite-parse-color "hsla(0, -50%, 50%, -0)")))
  (should (equal '(0.0 0.0 1.0 1.0)
                 (kite-parse-color "hsla(600, 200%, 50% , 5.)")))
  (should (equal '(0.0 0.0 1.0 1.0)
                 (kite-parse-color "hsla(-120, 200%, 50%  , 10.00 )"))))

(ert-deftest kite-test-invalid-hex ()
  ;; hex, too short
  (should (null (kite-parse-color "#1")))
  (should (null (kite-parse-color "#12")))

  ;; hex, invalid length
  (should (null (kite-parse-color "#1234")))
  (should (null (kite-parse-color "#12345")))

  ;; hex, too long
  (should (null (kite-parse-color "#1234567")))
  (should (null (kite-parse-color "#12345678")))

  ;; hex, invalid characters
  (should (null (kite-parse-color "#00g")))
  (should (null (kite-parse-color "#00HH00"))))

(ert-deftest kite-test-invalid-rgb ()
  ;; rgb, mix percent and integers
  (should (null (kite-parse-color "rgb(0,0%,0)")))
  (should (null (kite-parse-color "rgb(100%, 100%, 0)")))

  ;; rgb, comma missing
  (should (null (kite-parse-color "rgb(100%, 100% 0)")))

  ;; rgb, not enough components
  (should (null (kite-parse-color "rgb(0)")))
  (should (null (kite-parse-color "rgb(0,0)")))
  (should (null (kite-parse-color "rgb(0%)")))

  ;; rgb, too many components
  (should (null (kite-parse-color "rgb(0,0,0,0)")))
  (should (null (kite-parse-color "rgb(0%,0%,0%,0%)")))

  ;; rgb, invalid whitespace
  (should (null (kite-parse-color "rgb (0,0,0)")))
  (should (null (kite-parse-color " rgb(0,0,0)")))
  (should (null (kite-parse-color "rgb(0,0,0) ")))
  (should (null (kite-parse-color "rgb(0 %, 0 %, 0 %) ")))
  (should (null (kite-parse-color "rgb(- 0%, - 0%, - 0%) ")))
  (should (null (kite-parse-color "rgb(- 0, - 0, - 0)"))))

  ;; rgb, invalid capitalization
  (should (null (kite-parse-color "RGB(0,0,0)")))
  (should (null (kite-parse-color "Rgb(0,0,0)")))

(ert-deftest kite-test-invalid-rgba ()

  ;; rgba, mix percent and integers
  (should (null (kite-parse-color "rgba(0,0%,0,0)")))
  (should (null (kite-parse-color "rgba(100%, 100%, 0, 0)")))
  (should (null (kite-parse-color "rgba(0, 0, 0, 100%)")))
  (should (null (kite-parse-color "rgba(0%, 0%, 0%, 100%)")))

  ;; rgba, not enough components
  (should (null (kite-parse-color "rgba(0,0,0)")))
  (should (null (kite-parse-color "rgba(0%,0%,0%)")))

  ;; rgba, too many components
  (should (null (kite-parse-color "rgba(0,0,0,0,0)")))
  (should (null (kite-parse-color "rgb(0%,0%,0%,0%,0%)")))

  ;; rgba, invalid whitespace
  (should (null (kite-parse-color "rgba (0,0,0,0)")))
  (should (null (kite-parse-color " rgba(0,0,0,0)")))
  (should (null (kite-parse-color "rgba(0,0,0,0) ")))
  (should (null (kite-parse-color "rgba(0 %, 0 %, 0 %, 0) ")))
  (should (null (kite-parse-color "rgba(- 0%, - 0%, - 0%, 0) ")))
  (should (null (kite-parse-color "rgba(- 0, - 0, - 0, 0)"))))

(ert-deftest kite-test-invalid-hsl ()
  ;; hsl, mix percent and integers
  (should (null (kite-parse-color "hsl(0%,0%,0%)")))
  (should (null (kite-parse-color "hsl(100, 100%, 0)")))

  ;; hsl, comma missing
  (should (null (kite-parse-color "hsl(100, 100% 0%)")))

  ;; hsl, not enough components
  (should (null (kite-parse-color "hsl(0)")))
  (should (null (kite-parse-color "hsl(0,0%)")))

  ;; hsl, too many components
  (should (null (kite-parse-color "hsl(0,0%,0%,0%)")))
  (should (null (kite-parse-color "hsl(0,0%,0%,0)")))

  ;; hsl, invalid whitespace
  (should (null (kite-parse-color "hsl (0,0%,0%)")))
  (should (null (kite-parse-color " hsl(0,0%,0%)")))
  (should (null (kite-parse-color "hsl(0,0%,0%) ")))
  (should (null (kite-parse-color "hsl(0, 0 %, 0 %) ")))
  (should (null (kite-parse-color "hsl(- 0, - 0%, - 0%) ")))

  ;; hsl, invalid capitalization
  (should (null (kite-parse-color "HSL(0,0%,0%)")))
  (should (null (kite-parse-color "Hsl(0,0%,0%)"))))

(ert-deftest kite-test-invalid-hsla ()
  ;; hsla, mix percent and integers
  (should (null (kite-parse-color "hsla(0%,0%,0%,0)")))
  (should (null (kite-parse-color "hsla(100, 100%, 0%, 0%)")))

  ;; hsla, comma missing
  (should (null (kite-parse-color "hsla(100, 100% 0%, 0)")))

  ;; hsla, not enough components
  (should (null (kite-parse-color "hsla(0)")))
  (should (null (kite-parse-color "hsla(0,0%,0%)")))

  ;; hsla, too many components
  (should (null (kite-parse-color "hsla(0,0%,0%,0,0)")))

  ;; hsla, invalid whitespace
  (should (null (kite-parse-color "hsla (0,0%,0%,0)")))
  (should (null (kite-parse-color " hsla(0,0%,0%,0)")))
  (should (null (kite-parse-color "hsla(0,0%,0%,0) ")))
  (should (null (kite-parse-color "hsla(0, 0 %, 0 %, 0) ")))
  (should (null (kite-parse-color "hsla(- 0, - 0%, - 0%, - 0) ")))

  ;; hsla, invalid capitalization
  (should (null (kite-parse-color "HSLA(0,0%,0%,0)")))
  (should (null (kite-parse-color "Hsla(0,0%,0%,0)"))))

(provide 'kite-color-tests)
