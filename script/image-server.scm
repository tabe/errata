#!/usr/bin/env ypsilon
#!r6rs

(import (rnrs)
        (errata query))

(define *port-number* 3001)

(start (number->string *port-number*))
