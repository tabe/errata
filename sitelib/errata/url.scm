(library (errata url)
  (export record-fragment
          report->url)
  (import (rnrs)
          (only (lunula persistent-record) id-of)
          (only (lunula string) string-underscore)
          (only (lunula path) build-api-path)
          (only (errata model) report-uuid))

  (define-syntax record-fragment
    (syntax-rules ()
      ((_ record)
       (string-append
        (symbol->string (record-type-name (record-rtd record)))
        (number->string (id-of record))))))

  (define-syntax report->path
    (syntax-rules ()
      ((_ rep uuid)
       (build-api-path 'report uuid (report-uuid rep)))))

  (define report->url
    (case-lambda
     ((rep)
      (report->path rep #f))
     ((rep uuid)
      (report->path rep uuid))
     ((rep uuid record)
      (string-append (report->path rep uuid) "#" (record-fragment record)))))

)
