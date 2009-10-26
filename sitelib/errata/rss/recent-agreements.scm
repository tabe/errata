(library (errata rss recent-agreements)
  (export feed-set
          feed-entry
          feed-item)
  (import (rnrs)
          (match)
          (only (lunula mysql) close connect)
          (only (lunula persistent-record) created-at-of)
          (only (lunula rss) description dc:date dc:subject item link rdf:li title)
          (only (lunula tree) tree->string)
          (only (lunula xml) escape-string)
          (only (errata configuration) url-base)
          (only (errata helper) agreement-view)
          (only (errata model) agreement->caption recent-agreements)
          (only (errata url) report->url))

  (define *limit* 10)

  (define (feed-set user password database)
    (call/cc
     (lambda (cont)
       (dynamic-wind
           (lambda ()
             (let ((r (connect "localhost" user password database)))
               (when (string? r) (cont '()))))
           (lambda () (recent-agreements *limit*))
           close))))

  (define (feed-entry tuple)
    (match tuple
      ((agr a pref c q rep r b)
       (rdf:li ((rdf:resource (string-append url-base (report->url rep #f agr))))))
      (_ '())))

  (define (feed-item tuple)
    (match tuple
      ((agr a pref c q rep r b)
       (let ((url (string-append url-base (report->url rep #f agr)))
             (et (escape-string (agreement->caption agr))))
         (item
          ((rdf:about url))
          (title et)
          (link url)
          (dc:subject et)
          (dc:date (created-at-of agr))
          (description
           (escape-string
            (tree->string
             (agreement-view agr a pref)))))))
      (_ '())))

)
