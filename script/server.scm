#!/usr/bin/env ypsilon
#!r6rs

(import (rnrs)
        (srfi :48)
        (match)
        (errata))

(define *port-number* 3000)
(define *password* "")

(define (with-command-parameters proc)
  (match (command-line)
    ((_ port-number password)
     (proc port-number password))
    ((_ port-number)
     (proc port-number *password*))
    (else
     (proc (number->string *port-number*)
           *password*))))

(with-command-parameters
 (lambda (port-number password)
   (dynamic-wind
       (lambda () (connect "localhost" "root" password "errata"))
       (lambda () (start port-number))
       close)))
