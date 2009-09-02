#!/usr/bin/env ypsilon
#!r6rs

(import (rnrs) (errata calendar) (xunit))

(define-syntax assert-era
  (syntax-rules ()
    ((_ era ad)
     (assert-string=? era (ad->japanese-era ad)))))

(assert-era "平成21年" 2009)
(assert-era "平成元年" 1989)
(assert-era "昭和55年" 1980)

(report)
