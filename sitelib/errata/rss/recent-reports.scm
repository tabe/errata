(library (errata rss recent-reports)
  (export feed-set
          feed-entry
          feed-item)
  (import (rnrs)
          (match)
          (prefix (only (uri) encode-string) uri:)
          (only (lunula mod_lisp) build-api-path)
          (only (lunula mysql) close connect lookup-all)
          (only (lunula persistent-record) id-of created-at-of)
          (only (lunula tree) tree->string)
          (only (lunula xml) escape-string)
          (lunula rss)
          (only (lunula session) account)
          (only (errata calendar) datetime->y/m/d)
          (only (errata helper) revision-report-tr)
          (errata model))

  (define (report&revision->url rep r isbn10)
    (string-append
     "http://errata.fixedpoint.jp"
     (build-api-path 'r
                     #f
                     isbn10
                     (uri:encode-string (revision-name r))
                     (datetime->y/m/d (revision-revised-at r)))
     "#report"
     (number->string (id-of rep))))

  (define (feed-set user password database)
    (call/cc
     (lambda (cont)
       (dynamic-wind
           (lambda ()
             (let ((r (connect "localhost" user password database)))
               (when (string? r)
                 (cont '()))))
           (lambda ()
             (lookup-all (report
                          (account report)
                          (revision report)
                          (bib revision)
                          (quotation report)
                          (correction report))
                         ((exists (publicity (exlibris publicity))
                                  ((exlibris (revision))
                                   (exlibris (account)))))
                         ((order-by (report (created-at desc)))
                          (limit 10))))
           close))))

  (define (feed-entry tuple)
    (match tuple
      ((rep a r b q c)
       (cond ((bib-isbn10 b)
              => (lambda (isbn10)
                   (rdf:li ((rdf:resource (report&revision->url rep r isbn10))))))
             (else '())))
      (_ '())))

  (define (feed-item tuple)
    (match tuple
      ((rep a r b q c)
       (cond ((bib-isbn10 b)
              => (lambda (isbn10)
                   (let* ((url (report&revision->url rep r isbn10))
                          (t (string-append (report-subject rep) " -- " (bib-title b)))
                          (et (escape-string t)))
                     (item
                      ((rdf:about url))
                      (title et)
                      (link url)
                      (dc:subject et)
                      (dc:date (created-at-of rep))
                      (description
                       (escape-string
                        (tree->string
                         (revision-report-tr #f rep a q c '() '()))))))))
             (else '())))
      (_ '())))

)
