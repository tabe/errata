#!/usr/bin/env ypsilon
#!r6rs

(import (rnrs)
        (only (srfi :19) date->time-utc make-date time=?)
        (errata calendar)
        (xunit))

(define-syntax assert-era
  (syntax-rules ()
    ((_ era ad)
     (assert-string=? era (ad->japanese-era ad)))))

(define-assert-equivalence time=?)

(define-syntax assert-datetime->date
  (syntax-rules ()
    ((_ year month day hour minute second datetime)
     (assert-time=? (date->time-utc (make-date 0 second minute hour day month year (* 9 60 60)))
                    (date->time-utc (datetime->date datetime))))))

(define-syntax assert-datetime->y/m/d
  (syntax-rules ()
    ((_ expected datetime)
     (assert-string=? expected (datetime->y/m/d datetime)))))

(assert-era "平成21年" 2009)
(assert-era "平成元年" 1989)
(assert-era "昭和63年" 1988)
(assert-era "昭和55年" 1980)
(assert-era "昭和2年" 1927)
(assert-era "昭和元年" 1926)
(assert-era "大正14年" 1925)
(assert-era "大正2年" 1913)
(assert-era "大正元年" 1912)
(assert-era "明治44年" 1911)
(assert-era "明治元年" 1868)

(assert-datetime->date 2009 10 16 12 45 15 "2009-10-16 12:45:15")

(assert-datetime->y/m/d "2009/12/31" "2009-12-31 23:59:59")
(assert-datetime->y/m/d "2009/10/16" "2009-10-16 12:45:15")
(assert-datetime->y/m/d "2009/01/01" "2009-01-01 00:00:00")

(report)
