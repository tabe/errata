#!r6rs

(import (rnrs (6))
        (errata font)
        (xunit))

(define-syntax assert-face->style
  (syntax-rules ()
    ((_ style face)
     (assert-string=? style (face->style face)))))

(assert-boolean=? #f (face->style "unspecified"))
(assert-face->style "font-style: bold" "bold")
(assert-face->style "font-style: italic" "italic")
(assert-face->style "font-style: oblique" "oblique")
(assert-face->style "font-family: monospace; font-style: bold" "constant-width-bold")
(assert-face->style "font-family: monospace; font-style: italic" "constant-width-italic")
(assert-face->style "font-family: monospace; font-style: oblique" "constant-width-oblique")

(report)
