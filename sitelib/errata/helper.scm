(library (errata helper)
  (export links
          public-exlibris
          shelf-window
          exlibris-window)
  (import (except (rnrs) div)
          (only (core) format)
          (match)
          (only (srfi :1) take)
          (only (lcs) lcs-fold)
          (only (lunula gettext) __)
          (only (lunula mod_lisp) entry-paths build-entry-path)
          (lunula mysql)
          (prefix (lunula html) html:)
          (only (errata isbn) isbn10->amazon)
          (errata model))

  (define *exlibris-per-page* 5)

  (define (links uuid . _)
    (html:ul
     (map
      (lambda (path)
        (if (string? uuid)
            (html:li (html:a ((href (string-append path "?" uuid))) path))
            (html:li (html:a ((href path)) path))))
      (entry-paths))))

  (define (public-exlibris . args)
    (map
     (lambda (pub)
       (cond ((lookup exlibris (publicity-exlibris-id pub))
              => (lambda (ex)
                   (cond ((lookup revision (exlibris-revision-id ex))
                          => (lambda (rev)
                               (cond ((lookup bib (revision-bib-id rev))
                                      => (lambda (b)
                                           (html:p (html:escape-string (bib-title b)))))
                                     (else '()))))
                         (else '()))))
             (else '())))
     (lookup-all publicity '())))

  (define (exlibris-skeleton b r ex x y z)
    (let ((id (exlibris-id ex)))
      (append
       (html:h3 (html:escape-string (bib-title b)))
       (html:table
        (html:tr
         (html:th
          ((rowspan 4))
          (html:a ((href (isbn10->amazon (bib-isbn10 b))) (target "_blank")) (html:image ((src (bib-image b)) (alt (html:escape-string (bib-title b)))))))
         (html:th ((rowspan 2) (style "text-align:left;")) "ISBN")
         (html:td ((style "color:#555555;")) (bib-isbn13 b)))
        (html:tr
         (html:td ((style "color:#555555;")) (bib-isbn10 b)))
        (html:tr
         (html:th ((style "text-align:left;")) "リビジョン情報")
         (html:td ((style "color:#555555;")) (html:escape-string (revision-name r)) "(" (revision-revised-at r) ")" x))
        (html:tr
         (html:td y)
         (html:td z))))))

  (define (exlibris-panel uuid b r ex)
    (html:div
     (exlibris-skeleton b r ex
                        '()
                        (html:form ((action (build-entry-path 'desk uuid))
                                    (method "POST"))
                                   (html:input ((type "hidden") (name "id") (value (exlibris-id ex))))
                                   (html:input ((type "submit") (value (__ desk)))))
                        '())
     (html:hr ((style "color:#999999;")))))

  (define (exlibris-frame uuid b r ex)
    (let ((id (exlibris-id ex)))
      (html:div
       (exlibris-skeleton b r ex
                          (html:form ((action (build-entry-path 'modify-exlibris uuid))
                                      (method "POST"))
                                     (html:input ((type "hidden") (name "id") (value id)))
                                     (html:input ((type "submit") (value (__ modify-exlibris)))))
                          (cond ((lookup publicity `((exlibris-id ,id)))
                                 => (lambda (pub)
                                      (html:form ((action (build-entry-path 'hide-exlibris uuid))
                                                  (method "POST"))
                                                 (html:input ((type "hidden") (name "id") (value (publicity-id pub))))
                                                 (html:input ((type "submit") (value (__ hide-exlibris)))))))
                                (else
                                 (html:form ((action (build-entry-path 'share-exlibris uuid))
                                             (method "POST"))
                                            (html:input ((type "hidden") (name "id") (value id)))
                                            (html:input ((type "submit") (value (__ share-exlibris)))))))
                          (html:form ((action (build-entry-path 'put-off uuid))
                                      (method "POST"))
                                     (html:input ((type "hidden") (name "id") (value id)))
                                     (html:input ((type "submit") (value (__ put-off))))))
       (html:form ((action (build-entry-path 'edit-review uuid))
                   (method "POST"))
                  (html:div "レビュー:&nbsp;"
                            (html:input ((type "hidden") (name "id") (value id)))
                            (html:input ((type "submit") (value (__ edit-review))))
                            (cond ((lookup review `((exlibris-id ,id)))
                                   => (lambda (rvw) (html:div ((class "corner") (style "background-color: #ecfc71;")) (html:pre (html:escape-string (review-body rvw))))))
                                  (else "(なし)"))))

       (html:div
        (html:form ((action (build-entry-path 'new-report uuid))
                    (method "POST"))
                   (html:input ((type "hidden") (name "id") (value (revision-id r))))
                   (html:input ((type "submit") (value (__ new-report))))))
       (let ((reports (lookup-all report `((revision-id ,(revision-id r))))))
         (cond ((null? reports) '())
               (else
                (html:table
                 ((class "diff"))
                 (html:tr
                  (html:th (__ Quotation))
                  (html:th (__ Correction))
                  (html:td))
                 (map
                  (lambda (rep)
                    (let ((q (lookup quotation (report-quotation-id rep)))
                          (c (lookup correction (report-correction-id rep))))
                      (cons
                       (html:tr
                        (html:td ((style "font-size:small;"))
                                 "pp." (quotation-page q) "/" (quotation-position q) "&nbsp;"
                                 (report-subject rep) "&nbsp;")
                        (html:td))
                       (cond ((and (quotation? q)
                                   (correction? c))
                              (diff-tr uuid rep q c))
                             (else
                              '())))))
                  reports))))))))

  (define (diff-tr uuid rep q c)
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
                   (html:div ((class "corner")) (html:blockquote (reverse xa))))
          (html:td ((class "width:49%;"))
                   (html:div ((class "corner")) (html:blockquote (reverse xb))))
          (html:td
           (html:form ((action (build-entry-path 'modify-report uuid))
                       (method "POST"))
                      (html:input ((type "hidden") (name "id") (value (report-id rep))))
                      (html:input ((type "submit") (value (__ modify-report)))))
           (html:form ((action (build-entry-path 'drop-report uuid))
                       (method "POST"))
                      (html:input ((type "hidden") (name "id") (value (report-id rep))))
                      (html:input ((type "submit") (value (__ drop-report))))))
          )))))

  (define (shelf-window uuid body)
    (match body
      ((id page)
       (let ((ls (lookup-all exlibris
                             `((account-id ,id))
                             (format " ORDER BY id DESC LIMIT ~d OFFSET ~d" (+ *exlibris-per-page* 1) (* *exlibris-per-page* page)))))
         (append
          (map
           (lambda (ex)
             (let ((r (lookup revision (exlibris-revision-id ex))))
               (if r
                   (let ((b (lookup bib (revision-bib-id r))))
                     (if b (exlibris-panel uuid b r ex) "??"))
                   "?")))
           (if (< *exlibris-per-page* (length ls))
               (take ls *exlibris-per-page*)
               ls))
          (if (< 0 page)
              (html:form ((action (build-entry-path 'shelf uuid))
                          (method "POST"))
                         (html:input ((type "hidden") (name "page") (value (- page 1))))
                         (html:input ((type "submit") (value (html:escape-string "<<")))))
              '())
          (if (< *exlibris-per-page* (length ls))
              (html:form ((action (build-entry-path 'shelf uuid))
                          (method "POST"))
                         (html:input ((type "hidden") (name "page") (value (+ page 1))))
                         (html:input ((type "submit") (value (html:escape-string ">>")))))
              '()))))))

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
