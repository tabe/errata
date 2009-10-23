#!r6rs

(import (rnrs)
        (only (lunula persistent-record) id-set!)
        (only (errata model) make-acknowledgement make-report)
        (errata url)
        (rename (xunit) (report xunit:report)))

(define ack-100
  (let ((ack (make-acknowledgement #f #f #f #f)))
    (id-set! ack 100)
    ack))

(define rep-b224 (make-report "b224c158-598d-4897-8db5-875d167daee2" #f #f #f #f #f))

(assert-string=? "acknowledgement100" (record-fragment ack-100))

(assert-string=? "/report/b224c158-598d-4897-8db5-875d167daee2.html"
                 (report->url rep-b224))
(assert-string=? "/report/b224c158-598d-4897-8db5-875d167daee2.html?f5cef01f-ad87-4d91-9a49-62d010081bee"
                 (report->url rep-b224 "f5cef01f-ad87-4d91-9a49-62d010081bee"))
(assert-string=? "/report/b224c158-598d-4897-8db5-875d167daee2.html#acknowledgement100"
                 (report->url rep-b224 #f ack-100))
(assert-string=? "/report/b224c158-598d-4897-8db5-875d167daee2.html?f5cef01f-ad87-4d91-9a49-62d010081bee#acknowledgement100"
                 (report->url rep-b224 "f5cef01f-ad87-4d91-9a49-62d010081bee" ack-100))

(xunit:report)
