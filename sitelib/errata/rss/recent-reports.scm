(library (errata rss recent-reports)
  (export feed-set
          feed-entry
          feed-item)
  (import (rnrs)
          (match)
          (prefix (only (lunula html) table tbody) html:)
          (only (lunula mysql) close connect)
          (only (lunula persistent-record) created-at-of)
          (only (lunula tree) tree->string)
          (only (lunula xml) escape-string)
          (only (lunula rss) dc:date dc:subject description item link title rdf:li)
          (only (errata configuration) url-base)
          (only (errata helper) revision-report-tr)
          (only (errata model) bib-title recent-reports report->caption)
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
           (lambda () (recent-reports *limit*))
           close))))

  (define (feed-entry tuple)
    (match tuple
      ((rep a pref r b q c)
       (rdf:li ((rdf:resource (string-append url-base (bib&revision->url b r #f rep))))))
      (_ '())))

  (define (feed-item tuple)
    (match tuple
      ((rep a pref r b q c)
       (let* ((url (string-append url-base (bib&revision->url b r #f rep)))
              (t (string-append (report->caption rep) " -- " (bib-title b)))
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
               (revision-report-tr #f rep a pref q c '() '())))))))))
      (_ '())))

)
