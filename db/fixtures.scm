#!/usr/bin/env ypsilon
#!r6rs

(import (lunula session)
        (lunula mysql)
        (errata))

(connect "localhost" "root" "yoursql" "errata")

(save (make-account 1 "x" "X" "xxxxxxxx" "x@example.com" "http://www.example.com/"))

(close)
