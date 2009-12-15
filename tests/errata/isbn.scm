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

(define-syntax assert-tolerant-isbn10
  (syntax-rules ()
    ((_ s0 ...)
     (assert-valid-isbn tolerant-isbn10? s0 ...))))

(define-syntax assert-tolerant-isbn13
  (syntax-rules ()
    ((_ s0 ...)
     (assert-valid-isbn tolerant-isbn13? s0 ...))))

(define-syntax assert-valid-isbn10
  (syntax-rules ()
    ((_ s0 ...)
     (assert-valid-isbn valid-isbn10? s0 ...))))

(define-syntax assert-valid-isbn13
  (syntax-rules ()
    ((_ s0 ...)
     (assert-valid-isbn valid-isbn13? s0 ...))))

(assert-tolerant-isbn10
 "4874084141"
 "4-874-08414-1"
 "4-8-9-4-7-1-3-1-9-5"
 "4-7-8-1-9-0-5-8-6-2"
 "---------4894713195"
 "4781905862---------"
 (not "----------4781905862")
 (not "4781905862----------")
)

(assert-tolerant-isbn13
 "9784894713192"
 "9784874084144"
 "978-4894713192"
 "978-4874084144"
 "9-7-8-4-8-9-4-7-1-3-1-9-2"
 "9-7-8-4-8-7-4-0-8-4-1-4-4"
 "------------9784894713192"
 "9784874084144------------"
 (not "-------------9784874084144")
 (not "9784874084144-------------")
)

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
 "0123456789"
 "9876543210"
 "4894713195"
 "4874084141"
 "4781905862"
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

(assert-boolean=? #f (tolerant-isbn? "-"))
(assert-= 10 (tolerant-isbn? "2-222-22222-2"))
(assert-= 10 (tolerant-isbn? "4-756-14084-X"))
(assert-= 13 (tolerant-isbn? "978-4873114040"))

(report)
