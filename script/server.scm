#!/usr/bin/env ypsilon
#!r6rs

(import (rnrs)
        (srfi :48)
        (typo))

(define *port-number* 3000)

(define (port-number)
  (let ((args (command-line)))
    (cond ((< (length args) 2)
           (number->string *port-number*))
          (else
           (cadr args)))))

(dynamic-wind
    (lambda ()
      (connect "localhost" "root" "yoursql" "typo"))
    (lambda ()
      (start (port-number)))
    close)
