#!/usr/bin/env ypsilon
#!r6rs

(import (rnrs)
        (match)
        (errata rss))

(define *port-number* 3002)
(define *password* "")

(define (with-command-parameters proc)
  (match (command-line)
    ((_ port-number password)
     (proc port-number password))
    ((_ port-number)
     (proc port-number *password*))
    (_
     (proc (number->string *port-number*)
           *password*))))

(with-command-parameters
 (lambda (port-number password)
   (start port-number "root" password "errata")))
