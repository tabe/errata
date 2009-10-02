(library (errata rss recent-public-revisions)
  (export feed-set
          feed-entry
          feed-item)
  (import (rnrs)
          (match)
          (prefix (only (uri) encode-string) uri:)
          (only (lunula mod_lisp) build-api-path)
          (only (lunula mysql) close connect lookup-all)
          (only (lunula persistent-record) created-at-of)
          (only (lunula xml) escape-string)
          (lunula rss)
          (only (lunula session) account)
          (only (errata calendar) datetime->y/m/d)
          (errata model))

  (define (revision->url r isbn10)
    (string-append
     "http://errata.fixedpoint.jp"
     (build-api-path 'r
                     #f
                     isbn10
                     (uri:encode-string (revision-name r))
                     (datetime->y/m/d (revision-revised-at r)))))

  (define (feed-set user password database)
    (call/cc
     (lambda (cont)
       (dynamic-wind
           (lambda ()
             (let ((r (connect "localhost" user password database)))
               (when (string? r)
                 (cont '()))))
           (lambda ()
             (lookup-all (publicity
                          (exlibris publicity)
                          (account exlibris)
                          (revision exlibris)
                          (bib revision))
                         ()
                         ((order-by (publicity (created-at desc)))
                          (limit 10))))
           close))))

  (define (feed-entry tuple)
    (match tuple
      ((pub ex a r b)
       (cond ((bib-isbn10 b)
              => (lambda (isbn10)
                   (rdf:li ((rdf:resource (revision->url r isbn10))))))
             (else '())))
      (_ '())))

  (define (feed-item tuple)
    (match tuple
      ((pub ex a r b)
       (cond ((bib-isbn10 b)
              => (lambda (isbn10)
                   (let* ((url (revision->url r isbn10))
                          (t (bib-title b))
                          (et (escape-string t)))
                     (item
                      ((rdf:about url))
                      (title et)
                      (link url)
                      (dc:subject et)
                      (dc:date (created-at-of pub))
                      (description et)))))
             (else '())))
      (_ '())))

)
