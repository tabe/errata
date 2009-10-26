(library (errata rss recent-reviews)
  (export feed-set
          feed-entry
          feed-item)
  (import (rnrs)
          (match)
          (only (lunula mysql) close connect)
          (only (lunula persistent-record) updated-at-of)
          (only (lunula tree) tree->string)
          (only (lunula xml) escape-string)
          (only (lunula rss) dc:date dc:subject description item link title rdf:li)
          (only (errata configuration) url-base)
          (only (errata helper) review-div)
          (only (errata model) bib-title recent-reviews review->caption)
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
           (lambda () (recent-reviews *limit*))
           close))))

  (define (feed-entry tuple)
    (match tuple
      ((rvw ex a pref r b)
       (rdf:li ((rdf:resource (string-append url-base (bib&revision->url b r #f rvw))))))
      (_ '())))

  (define (feed-item tuple)
    (match tuple
      ((rvw ex a pref r b)
       (let ((url (string-append url-base (bib&revision->url b r #f rvw)))
             (et (escape-string (string-append (review->caption rvw) " -- " (bib-title b)))))
         (item
          ((rdf:about url))
          (title et)
          (link url)
          (dc:subject et)
          (dc:date (updated-at-of rvw))
          (description
           (escape-string
            (tree->string
             (review-div (list rvw ex a pref))))))))
      (_ '())))

)
