#!r6rs

(import (rnrs)
        (only (lunula persistent-record) id-set!)
        (only (errata model) make-acknowledgement make-bib make-report make-revision)
        (errata url)
        (rename (xunit) (report xunit:report)))

(define bib-a (make-bib #f #f #f "0123456789" #f))

(define bib-b (make-bib "7706689d-8069-4189-8d97-d4d21b5f4d4f" #f #f #f #f))

(define rev-a (make-revision #f "初版第2刷" "2009-10-23 15:40:00"))

(define rep-b224
  (let ((rep (make-report "b224c158-598d-4897-8db5-875d167daee2" #f #f #f #f #f)))
    (id-set! rep 10)
    rep))

(define ack-100
  (let ((ack (make-acknowledgement #f #f #f #f)))
    (id-set! ack 100)
    ack))

(assert-string=? "acknowledgement100" (record->fragment ack-100))

(assert-string=? "/r/0123456789/%E5%88%9D%E7%89%88%E7%AC%AC2%E5%88%B7/2009/10/23.html"
                 (bib&revision->url bib-a rev-a))
(assert-string=? "/r/0123456789/%E5%88%9D%E7%89%88%E7%AC%AC2%E5%88%B7/2009/10/23.html?ad1f57fb-1871-42b7-92e2-e19d7bde287b"
                 (bib&revision->url bib-a rev-a "ad1f57fb-1871-42b7-92e2-e19d7bde287b"))
(assert-string=? "/r/0123456789/%E5%88%9D%E7%89%88%E7%AC%AC2%E5%88%B7/2009/10/23.html#report10"
                 (bib&revision->url bib-a rev-a #f rep-b224))
(assert-string=? "/r/0123456789/%E5%88%9D%E7%89%88%E7%AC%AC2%E5%88%B7/2009/10/23.html?ad1f57fb-1871-42b7-92e2-e19d7bde287b#report10"
                 (bib&revision->url bib-a rev-a "ad1f57fb-1871-42b7-92e2-e19d7bde287b" rep-b224))
(assert-string=? "/revision/7706689d-8069-4189-8d97-d4d21b5f4d4f/%E5%88%9D%E7%89%88%E7%AC%AC2%E5%88%B7/2009/10/23.html"
                 (bib&revision->url bib-b rev-a))

(assert-string=? "/report/b224c158-598d-4897-8db5-875d167daee2.html"
                 (report->url rep-b224))
(assert-string=? "/report/b224c158-598d-4897-8db5-875d167daee2.html?f5cef01f-ad87-4d91-9a49-62d010081bee"
                 (report->url rep-b224 "f5cef01f-ad87-4d91-9a49-62d010081bee"))
(assert-string=? "/report/b224c158-598d-4897-8db5-875d167daee2.html#acknowledgement100"
                 (report->url rep-b224 #f ack-100))
(assert-string=? "/report/b224c158-598d-4897-8db5-875d167daee2.html?f5cef01f-ad87-4d91-9a49-62d010081bee#acknowledgement100"
                 (report->url rep-b224 "f5cef01f-ad87-4d91-9a49-62d010081bee" ack-100))

(xunit:report)
