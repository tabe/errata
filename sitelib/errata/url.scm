(library (errata url)
  (export record->fragment
          bib&revision->url
          report->path
          report->url)
  (import (rnrs)
          (only (uri) encode-string)
          (only (lunula persistent-record) id-of)
          (only (lunula string) string-underscore)
          (only (lunula path) build-api-path)
          (only (errata calendar) datetime->y/m/d)
          (only (errata model)
                bib-isbn10
                bib-uuid
                report-uuid
                revision-name
                revision-revised-at))

  (define-syntax record->fragment
    (syntax-rules ()
      ((_ record)
       (string-append
        (symbol->string (record-type-name (record-rtd record)))
        (number->string (id-of record))))))

  (define-syntax define-x->url
    (syntax-rules ()
      ((_ name x->path arg0 arg1 ...)
       (define name
         (case-lambda
          ((arg0 arg1 ...)
           (x->path arg0 arg1 ... #f))
          ((arg0 arg1 ... uuid)
           (x->path arg0 arg1 ... uuid))
          ((arg0 arg1 ... uuid record)
           (string-append (x->path arg0 arg1 ... uuid) "#" (record->fragment record))))))))

  (define-syntax bib&revision->path
    (syntax-rules ()
      ((_ b r uuid)
       (cond ((bib-isbn10 b)
              => (lambda (isbn10)
                   (build-api-path 'r
                                   uuid
                                   isbn10
                                   (encode-string (revision-name r))
                                   (datetime->y/m/d (revision-revised-at r)))))
             (else
              (build-api-path 'revision
                              uuid
                              (bib-uuid b)
                              (encode-string (revision-name r))
                              (datetime->y/m/d (revision-revised-at r))))))
      ((_ b r)
       (bib&revision->path b r #f))))

  (define-x->url bib&revision->url bib&revision->path b r)

  (define-syntax report->path
    (syntax-rules ()
      ((_ rep uuid)
       (build-api-path 'report uuid (report-uuid rep)))
      ((_ rep)
       (report->path rep #f))))

  (define-x->url report->url report->path rep)

)
