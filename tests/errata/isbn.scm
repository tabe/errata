#!/usr/bin/env ypsilon
#!r6rs

(import (errata isbn) (xunit))

(assert-boolean=? #f (valid-isbn? ""))
(assert-= 10 (valid-isbn? "2222222222"))
(assert-= 10 (valid-isbn? "475614084X"))
(assert-= 13 (valid-isbn? "9784873114040"))

(report)
