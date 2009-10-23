(library (errata rss recent-revisions)
  (export feed-set
          feed-entry
          feed-item)
  (import (rnrs)
          (match)
          (only (lunula mysql) close connect)
          (only (lunula persistent-record) created-at-of)
          (only (lunula xml) escape-string)
          (only (lunula rss) dc:date dc:subject description item link title rdf:li)
          (only (errata configuration) url-base)
          (only (errata model) bib-title recent-revisions)
          (only (errata url) bib&revision->url))

  (define *limit* 10)

  (define (feed-set user password database)
    (call/cc
     (lambda (cont)
       (dynamic-wind
           (lambda ()
             (let ((r (connect "localhost" user password database)))
               (when (string? r)
                 (cont '()))))
           (lambda () (recent-revisions *limit*))
           close))))

  (define (feed-entry tuple)
    (match tuple
      ((pub ex a r b)
       (rdf:li ((rdf:resource (string-append url-base (bib&revision->url b r))))))
      (_ '())))

  (define (feed-item tuple)
    (match tuple
      ((pub ex a r b)
       (let ((url (string-append url-base (bib&revision->url b r)))
             (et (escape-string (bib-title b))))
         (item
          ((rdf:about url))
          (title et)
          (link url)
          (dc:subject et)
          (dc:date (created-at-of pub))
          (description et))))
      (_ '())))

)
