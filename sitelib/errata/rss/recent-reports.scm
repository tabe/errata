(library (errata rss recent-reports)
  (export feed-set
          feed-entry
          feed-item)
  (import (rnrs)
          (match)
          (prefix (only (uri) encode-string) uri:)
          (prefix (only (lunula html) table tbody) html:)
          (only (lunula mysql) close connect)
          (only (lunula path) build-api-path)
          (only (lunula persistent-record) id-of created-at-of)
          (only (lunula tree) tree->string)
          (only (lunula xml) escape-string)
          (lunula rss)
          (only (lunula session) account)
          (only (errata calendar) datetime->y/m/d)
          (only (errata configuration) url-base)
          (only (errata helper) revision-report-tr)
          (errata model))

  (define *limit* 10)

  (define (report&revision->url rep r isbn10)
    (string-append
     url-base
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
           (lambda () (recent-reports *limit*))
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
                         (html:table
                          (html:tbody
                           (revision-report-tr #f rep a q c '() '()))))))))))
             (else '())))
      (_ '())))

)
