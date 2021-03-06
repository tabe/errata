#!/usr/bin/env ypsilon
#!r6rs

(import (errata page) (xunit))

(assert-= 1 (roman-numeral? "i"))
(assert-= 2 (roman-numeral? "ii"))
(assert-= 3 (roman-numeral? "iii"))
(assert-= 4 (roman-numeral? "iv"))
(assert-= 5 (roman-numeral? "v"))
(assert-= 6 (roman-numeral? "vi"))
(assert-= 7 (roman-numeral? "vii"))
(assert-= 8 (roman-numeral? "viii"))
(assert-= 9 (roman-numeral? "ix"))
(assert-= 10 (roman-numeral? "x"))
(assert-= 11 (roman-numeral? "xi"))
(assert-= 40 (roman-numeral? "xl"))
(assert-= 50 (roman-numeral? "l"))
(assert-= 60 (roman-numeral? "lx"))
(assert-= 90 (roman-numeral? "xc"))
(assert-= 100 (roman-numeral? "c"))
(assert-= 400 (roman-numeral? "cd"))
(assert-= 500 (roman-numeral? "d"))
(assert-= 600 (roman-numeral? "dc"))
(assert-= 900 (roman-numeral? "cm"))
(assert-= 1000 (roman-numeral? "m"))
(assert-= 2009 (roman-numeral? "mmix"))

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
