(library (errata helper)
  (export preload-script
          links
          public-revisions
          report-window
          revision-window
          shelf-window
          exlibris-window)
  (import (except (rnrs) div)
          (only (core) format)
          (match)
          (only (srfi :13) string-tokenize)
          (only (srfi :19) date->string string->date date-year)
          (only (lcs) lcs-fold)
          (only (lunula gettext) __)
          (only (lunula mod_lisp) entry-paths build-entry-path)
          (only (lunula mysql) lookup lookup-all)
          (prefix (lunula html) html:)
          (only (lunula session) account account-nick account-name)
          (only (errata calendar) ad->japanese-era)
          (only (errata isbn) isbn10->amazon)
          (errata model)
          (errata helper pagination))

  (define preload-script 
    (lambda _
      '("$(document).ready(function() {"
        "$('div#links').corner();"
        "$('#private').corner();"
        "$('#public').corner();"
        "$('.dog').corner('dog tr 15px');"
        "});")))

  (define (links uuid . _)
    (html:ul
     (vector-map
      (lambda (path)
        (if (string? uuid)
            (html:li (html:a ((href (string-append path "?" uuid))) path))
            (html:li (html:a ((href path)) path))))
      (entry-paths))))

  (define (hidden-field name value)
    (html:input ((type "hidden") (name name) (value value))))

  (define (datetime->date str)
    (guard (e (else #f))
      (string->date str "~Y-~m-~d ~H:~M:~S")))

  (define (date->ymd date)
    (let ((year (date-year date)))
      (guard (e
              (else (date->string date "~Y-~m-~d")))
        (append (html:span ((title (ad->japanese-era year))) year)
                (date->string date "-~m-~d")))))

  (define (datetime->ymd str)
    (cond ((datetime->date str) => date->ymd)
          (else #f)))

  (define (signature a)
    (html:span ((title (html:escape-string (account-name a))))
               (html:escape-string (account-nick a))))

  (define-syntax with-uuid
    (syntax-rules ()
      ((_ uuid thunk)
       (if (string? uuid)
           thunk
           '()))))

  (define-syntax revision-skeleton
    (syntax-rules ()
      ((_ b r x y z)
       (append
        (html:h3 (html:escape-string (bib-title b)))
        (html:table
         (html:tr
          (html:th
           ((rowspan 4))
           (html:a ((href (isbn10->amazon (bib-isbn10 b))) (target "_blank"))
                   (html:image ((src (bib-image b)) (alt (html:escape-string (bib-title b)))))))
          (html:th ((rowspan 2) (style "text-align:left;")) (__ ISBN))
          (html:td ((style "color:#555555;")) (bib-isbn13 b)))
         (html:tr
          (html:td ((style "color:#555555;")) (bib-isbn10 b)))
         (html:tr
          (html:th ((style "text-align:left;")) (__ Revision))
          (html:td ((style "color:#555555;"))
                   (cons* (html:escape-string (revision-name r))
                          "(" (datetime->ymd (revision-revised-at r)) ")" x)))
         (html:tr
          (html:td y)
          (html:td z)))))))

  (define (go-to-board uuid)
    (html:form ((action (build-entry-path 'board uuid)))
               (html:input  ((type "submit") (value (__ to-board))))))

  (define (go-to-table uuid r)
    (html:form ((action (build-entry-path 'table uuid)))
               (hidden-field "id" (revision-id r))
               (html:input ((type "submit") (value (__ to-table))))))

  (define (public-revisions uuid page)
    (with-pagination
     (board uuid page)
     (lookup-all revision "EXISTS (SELECT * FROM publicity p, exlibris ex WHERE ex.id = p.exlibris_id and revision.id = ex.revision_id)")
     (lambda (r)
       (cond ((lookup bib (revision-bib-id r))
              => (lambda (b)
                   (append
                    (revision-skeleton b r
                                       '()
                                       (go-to-table uuid r)
                                       '())
                    (revision-reviews r)
                    (html:hr ((style "color:#999999;"))))))
             (else '())))))

  (define (revision-window uuid id)
    (assert (integer? id))
    (let ((r (lookup revision id)))
      (if (revision? r)
          (let ((b (lookup bib (revision-bib-id r))))
            (if (bib? b)
                (revision-frame uuid b r)
                "???"))
          "??")))

  (define (report-window uuid id)
    (assert (integer? id))
    (let ((rep (lookup report id)))
      (if (report? rep)
          (let ((r (lookup revision (report-revision-id rep))))
            (if (revision? r)
                (let ((b (lookup bib (revision-bib-id r))))
                  (if (bib? b)
                      (report-frame uuid b r rep)
                      "???"))
                "??"))
          "?")))

  (define (report-frame uuid b r rep)
    (html:div
     (go-to-table uuid r)
     (revision-skeleton b r '() '() '())
     (html:h4 (__ Detail))
     (diff-table
      (revision-report-tr uuid r rep
                          (with-uuid
                           uuid
                           (html:form ((action (build-entry-path 'disagree uuid)))
                                      (hidden-field "id" (report-correction-id rep))
                                      (html:input ((type "submit") (value (__ disagree))))))
                          (lambda (q c) (ack/nak-tr uuid rep q c))))))

  (define (diff-table x)
    (html:table
     ((class "diff"))
     (html:tr
      (html:th ((class "title")) (__ Quotation))
      (html:th ((class "title")) (__ Correction))
      (html:td))
     x))

  (define (revision-report-tr uuid r rep x proc)
    (let ((a (lookup account (report-account-id rep)))
          (q (lookup quotation (report-quotation-id rep)))
          (c (lookup correction (report-correction-id rep))))
      (cons
       (html:tr
        (html:td ((style "font-size:small;"))
                 "pp." (quotation-page q) "/" (quotation-position q) "&nbsp;"
                 (report-subject rep) "&nbsp;"
                 "("
                 (html:span ((style "font-size:x-small;")) "reported by ")
                 (signature a)
                 ")")
        (html:td))
       (cond ((and (quotation? q)
                   (correction? c))
              (append (diff-tr uuid rep q c x)
                      (proc q c)))
             (else '())))))

  (define (review-div rvw)
    (html:div ((class "dog") (style "background-color:#c7ff6f;")) (html:pre (html:escape-string (review-body rvw)))))

  (define (revision-reviews r)
    (html:div
     (html:h4 (__ Review))
     (let ((ls (lookup-all review (format "EXISTS (SELECT * FROM exlibris ex WHERE ex.id = review.exlibris_id AND ex.revision_id = '~d')" (revision-id r)))))
       (if (null? ls)
           "(なし)"
           (map review-div ls)))))

  (define (diff-tr uuid rep q c forms)
    (let ((a (string->list (quotation-body q)))
          (b (string->list (correction-body c))))
      (match (lcs-fold
              (lambda (x pair)
                (match pair
                  ((xa . xb)
                   (case x
                     ((#\linefeed)
                      `(,(cons (html:br) xa) . ,xb))
                     (else
                      `(,(cons (html:span ((class "minus")) (html:escape-char x)) xa) . ,xb))))))
              (lambda (x pair)
                (match pair
                  ((xa . xb)
                   (case x
                     ((#\linefeed)
                      `(,(cons (html:br) xa) . ,xb))
                     (else
                      `(,xa . ,(cons (html:span ((class "plus")) (html:escape-char x)) xb)))))))
              (lambda (x pair)
                (match pair
                  ((xa . xb)
                   (case x
                     ((#\linefeed)
                      `(,(cons (html:br) xa) . ,(cons (html:br) xb)))
                     (else
                      `(,(cons (html:escape-char x) xa) . ,(cons (html:escape-char x) xb)))))))
              '(() . ())
              a
              b)
        ((xa . xb)
         (html:tr
          (html:td ((class "width:49%;"))
                   (html:div ((class "dog")) (html:blockquote (reverse xa))))
          (html:td ((class "width:49%;"))
                   (html:div ((class "dog")) (html:blockquote (reverse xb))))
          (html:td forms))))))

  (define (ack/nak-tr uuid rep q c)
    (append
     (with-uuid
      uuid
      (html:tr
       (html:td 
        (html:form ((action (build-entry-path 'acknowledge uuid)))
                   (hidden-field "report" (report-id rep))
                   (hidden-field "quotation" (quotation-id q))
                   (html:input ((type "submit") (value (__ acknowledge))))))
       (html:td
        (html:form ((action (build-entry-path 'agree uuid)))
                   (hidden-field "report" (report-id rep))
                   (hidden-field "correction" (correction-id c))
                   (html:input ((type "submit") (value (__ agree))))))))
     (html:tr
      (html:td
       (let ((acks (reverse (lookup-all acknowledgement `((quotation-id ,(quotation-id q)))))))
         (if (null? acks)
             '()
             (html:div
              ((class "acknowledgement"))
              (map
               (lambda (a)
                 (html:div
                  (html:span ((class "credit"))
                             (cond ((lookup account (acknowledgement-account-id a)) => signature)
                                   (else "?"))
                             ":&nbsp;")
                  (html:span ((class (if (acknowledgement-positive? a) "ack" "nak")))
                             (html:escape-string (acknowledgement-comment a)))))
               acks))))
       (html:td
        (let ((agms (reverse (lookup-all agreement `((correction-id ,(correction-id c)))))))
          (if (null? agms)
              '()
              (html:div
               ((class "agreement"))
               (map
                (lambda (a)
                  (html:div
                   (html:span ((class "credit"))
                              (cond ((lookup account (agreement-account-id a)) => signature)
                                    (else "?"))
                              ":&nbsp;")
                   (html:span (html:escape-string (agreement-comment a)))))
                agms)))))))))

  (define (revision-reports uuid r proc)
    (append
     (html:h4 (__ Table))
     (diff-table
      (map
       (lambda (rep)
         (revision-report-tr uuid r rep
                             (proc rep)
                             (lambda (q c) '())))
       (lookup-all report `((revision-id ,(revision-id r))))))))

  (define (revision-frame uuid b r)
    (html:div
     (go-to-board uuid)
     (revision-skeleton b r '() '() '())
     (revision-reports uuid r (lambda (rep)
                                (html:form ((action (build-entry-path 'detail uuid)))
                                           (hidden-field "id" (report-id rep))
                                           (html:input ((type "submit") (value (__ to-detail)))))))))

  (define (exlibris-panel uuid b r ex)
    (html:div
     (revision-skeleton b r
                        '()
                        (html:form ((action (build-entry-path 'desk uuid)))
                                   (hidden-field "id" (exlibris-id ex))
                                   (html:input ((type "submit") (value (__ to-desk)))))
                        '())
     (html:hr ((style "color:#999999;")))))

  (define (exlibris-frame uuid b r ex)
    (let ((id (exlibris-id ex)))
      (html:div
       (revision-skeleton b r
                          (html:form ((action (build-entry-path 'modify-revision uuid)))
                                     (hidden-field "id" id)
                                     (html:input ((type "submit") (value (__ modify-revision)))))
                          (cond ((lookup publicity `((exlibris-id ,id)))
                                 => (lambda (pub)
                                      (html:form ((action (build-entry-path 'hide-exlibris uuid)))
                                                 (hidden-field "id" (publicity-id pub))
                                                 (html:input ((type "submit") (value (__ hide-exlibris)))))))
                                (else
                                 (html:form ((action (build-entry-path 'share-exlibris uuid)))
                                            (hidden-field "id" id)
                                            (html:input ((type "submit") (value (__ share-exlibris)))))))
                          (html:form ((action (build-entry-path 'put-off uuid)))
                                     (hidden-field "id" id)
                                     (html:input ((type "submit") (value (__ put-off))))))
       (html:form ((action (build-entry-path 'edit-review uuid)))
                  (html:div "レビュー:&nbsp;"
                            (hidden-field "id" id)
                            (html:input ((type "submit") (value (__ edit-review))))
                            (cond ((lookup review `((exlibris-id ,id))) => review-div)
                                  (else "(なし)"))))

       (html:div
        (html:form ((action (build-entry-path 'new-report uuid)))
                   (hidden-field "id" (exlibris-id ex))
                   (html:input ((type "submit") (value (__ new-report))))))
       (revision-reports uuid r (lambda (rep)
                                  (append
                                   (html:form ((action (build-entry-path 'modify-report uuid)))
                                              (hidden-field "report" (report-id rep))
                                              (hidden-field "exlibris" (exlibris-id ex))
                                              (html:input ((type "submit") (value (__ modify-report)))))
                                   (html:form ((action (build-entry-path 'drop-report uuid)))
                                              (hidden-field "report" (report-id rep))
                                              (hidden-field "exlibris" (exlibris-id ex))
                                              (html:input ((type "submit") (value (__ drop-report)))))))))))

  (define (shelf-window uuid body)
    (match body
      ((id page)
       (with-pagination
        (shelf uuid page)
        (lookup-all exlibris `((account-id ,id)))
        (lambda (ex)
          (let ((r (lookup revision (exlibris-revision-id ex))))
            (if r
                (let ((b (lookup bib (revision-bib-id r))))
                  (if b (exlibris-panel uuid b r ex) "??"))
                "?")))))))

  (define (exlibris-window uuid id)
    (let ((ex (lookup exlibris id)))
      (if (exlibris? ex)
          (let ((r (lookup revision (exlibris-revision-id ex))))
            (if (revision? r)
                (let ((b (lookup bib (revision-bib-id r))))
                  (if (bib? b)
                      (exlibris-frame uuid b r ex)
                      "???"))
                "??"))
          "?")))

  )
