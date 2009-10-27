#!r6rs

(import (rnrs)
        (only (lunula persistent-record) id-set!)
        (errata notification)
        (except (xunit) report)
        (prefix (only (xunit) report) xunit:))

(define nt-100
  (let ((nt (make-notification #f #f "これは題名です。" "これは本文です。" "/a/path.html" "a_fragment")))
    (id-set! nt 100)
    nt))

(assert-string=? "/a/path.html#a_fragment" (notification->url nt-100))
(assert-string=? "/a/path.html#a_fragment" (notification->url nt-100 #f))
(assert-string=? "/a/path.html?to-be-inserted#a_fragment" (notification->url nt-100 "to-be-inserted"))

(xunit:report)

