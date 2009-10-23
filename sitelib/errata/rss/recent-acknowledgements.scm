(library (errata rss recent-acknowledgements)
  (export feed-set
          feed-entry
          feed-item)
  (import (rnrs)
          (match)
          (only (lunula mysql) close connect)
          (only (lunula persistent-record) created-at-of)
          (only (lunula rss) dc:date dc:subject description item link title rdf:li)
          (only (lunula string) string-truncate)
          (only (lunula tree) tree->string)
          (only (lunula xml) escape-string)
          (only (errata configuration) url-base)
          (only (errata helper) acknowledgement-view)
          (only (errata model) acknowledgement->caption recent-acknowledgements)
          (only (errata url) report->url))

  (define *limit* 10)

  (define (feed-set user password database)
    (call/cc
     (lambda (cont)
       (dynamic-wind
           (lambda ()
             (let ((r (connect "localhost" user password database)))
               (when (string? r) (cont '()))))
           (lambda () (recent-acknowledgements *limit*))
           close))))

  (define (feed-entry tuple)
    (match tuple
      ((ack a q rep r b)
       (rdf:li ((rdf:resource (string-append url-base (report->url rep #f ack))))))
      (_ '())))

  (define (feed-item tuple)
    (match tuple
      ((ack a q rep r b)
       (let ((url (string-append url-base (report->url rep #f ack)))
             (et (escape-string (acknowledgement->caption ack))))
         (item
          ((rdf:about url))
          (title et)
          (link url)
          (dc:subject et)
          (dc:date (created-at-of ack))
          (description
           (escape-string
            (tree->string
             (acknowledgement-view ack a)))))))
      (_ '())))

)
