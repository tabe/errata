#!/usr/bin/env ypsilon
#!r6rs

(import (rnrs) (errata isbn) (xunit))

(define-syntax assert-valid-isbn
  (syntax-rules (not)
    ((_ proc)
     #t)
    ((_ proc (not s0) s1 ...)
     (begin
       (assert-boolean=? #f (proc s0))
       (assert-valid-isbn proc s1 ...)))
    ((_ proc s0 s1 ...)
     (begin
       (assert-boolean=? #t (proc s0))
       (assert-valid-isbn proc s1 ...)))))

(define-syntax assert-valid-isbn10
  (syntax-rules ()
    ((_ s0 ...)
     (assert-valid-isbn valid-isbn10? s0 ...))))

(define-syntax assert-valid-isbn13
  (syntax-rules ()
    ((_ s0 ...)
     (assert-valid-isbn valid-isbn13? s0 ...))))

(assert-valid-isbn10
 "0000000000"
 "1111111111"
 "2222222222"
 "3333333333"
 "4444444444"
 "5555555555"
 "6666666666"
 "7777777777"
 "8888888888"
 "9999999999"
 "4894713195"
 "4874084141"
 )

(assert-valid-isbn13
 "0000000000000"
 (not "1111111111111")
 "2222222222222"
 (not "3333333333333")
 "4444444444444"
 (not "5555555555555")
 "6666666666666"
 (not "7777777777777")
 "8888888888888"
 (not "9999999999999")
 "9784894713192"
 "9784874084144")

(assert-boolean=? #f (valid-isbn? ""))
(assert-= 10 (valid-isbn? "2222222222"))
(assert-= 10 (valid-isbn? "475614084X"))
(assert-= 13 (valid-isbn? "9784873114040"))

(report)
