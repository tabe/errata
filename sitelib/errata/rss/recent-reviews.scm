(library (errata rss recent-reviews)
  (export feed-set
          feed-entry
          feed-item)
  (import (rnrs)
          (match)
          (prefix (only (uri) encode-string) uri:)
          (only (lunula mod_lisp) build-api-path)
          (only (lunula mysql) close connect lookup-all)
          (only (lunula persistent-record) id-of updated-at-of)
          (only (lunula tree) tree->string)
          (only (lunula xml) escape-string)
          (lunula rss)
          (only (lunula session) account)
          (only (errata calendar) datetime->y/m/d)
          (only (errata helper) review-div)
          (errata model))

  (define *limit* 10)

  (define (review&revision->url rvw r isbn10)
    (string-append
     "http://errata.fixedpoint.jp"
     (build-api-path 'r
                     #f
                     isbn10
                     (uri:encode-string (revision-name r))
                     (datetime->y/m/d (revision-revised-at r)))
     "#review"
     (number->string (id-of rvw))))

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
      ((rvw ex a r b)
       (cond ((bib-isbn10 b)
              => (lambda (isbn10)
                   (rdf:li ((rdf:resource (review&revision->url rvw r isbn10))))))
             (else '())))
      (_ '())))

  (define (feed-item tuple)
    (match tuple
      ((rvw ex a r b)
       (cond ((bib-isbn10 b)
              => (lambda (isbn10)
                   (let* ((url (review&revision->url rvw r isbn10))
                          (t (bib-title b))
                          (et (escape-string t)))
                     (item
                      ((rdf:about url))
                      (title et)
                      (link url)
                      (dc:subject et)
                      (dc:date (updated-at-of rvw))
                      (description
                       (escape-string
                        (tree->string
                         (review-div (list rvw ex a)))))))))
             (else '())))
      (_ '())))

)
