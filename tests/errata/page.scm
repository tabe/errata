#!/usr/bin/env ypsilon
#!r6rs

(import (errata page) (xunit))

(assert-boolean=? #t (page<? "1" "2"))
(assert-boolean=? #t (page<? "2" "100"))
(assert-boolean=? #f (page<? "101" "101"))
(assert-boolean=? #t (page<? "iv" "1"))
(assert-boolean=? #t (page<? "ii" "2"))
(assert-boolean=? #f (page<? "1" "iv"))
(assert-boolean=? #f (page<? "2" "ii"))
(assert-boolean=? #t (page<? "i" "ii"))
(assert-boolean=? #t (page<? "ii" "iii"))
(assert-boolean=? #t (page<? "ii" "iv"))
(assert-boolean=? #t (page<? "v" "vi"))

(report)
