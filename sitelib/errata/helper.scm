(library (errata helper)
  (export errata-description
          errata-keywords
          errata-logo
          errata-rss-links
          powered-by-lunula
          creativecommons-attribution-logo
          creativecommons-attribution
          preload-script
          menu
          links
          belt
          public-revisions
          bib-window
          acknowledgement-view
          agreement-view
          report-window
          review-div
          revision-report-tr
          revision-window
          shelf-window
          exlibris-window)
  (import (except (rnrs) div)
          (only (core) format)
          (match)
          (only (srfi :13) string-tokenize)
          (only (srfi :19) date->string date-year)
          (only (lcs) lcs-fold)
          (prefix (only (uri) encode-string) uri:)
          (only (lunula gettext) __)
          (only (lunula mysql) lookup lookup-all)
          (prefix (lunula html) html:)
          (only (lunula path) build-entry-path build-api-path)
          (only (lunula persistent-record) id-of created-at-of)
          (only (lunula session) account account-nick account-name)
          (only (lunula string) blank? string-truncate)
          (only (errata calendar) ad->japanese-era datetime->date datetime->y/m/d)
          (only (errata configuration) url-base)
          (only (errata isbn) isbn10->amazon)
          (errata model)
          (errata helper pagination)
          (errata page)
          (only (errata url) record-fragment report->url))

  (define errata-description
    (html:meta ((name "description") (content "書籍などの正誤表を共有するためのサービス。"))))

  (define errata-keywords
    (html:meta ((name "keywords") (content "errata,typo,proofreading,正誤表,共有,誤植,タイポ,誤訳,校正"))))

  (define (errata-logo uuid . _)
    (html:h1 ((id "logo") (title "えらった べーた"))
             (html:a ((href (build-entry-path 'index uuid)))
                     (cons "Errata" (html:span ((style "color:red;")) "&#x03B2;")))))

  (define errata-rss-links
    (map
     (lambda (name title)
       (html:link ((href (format "/~a.rss" name)) (rel "alternate") (type "application/rss+xml") (title title))))
     '(recent-revisions
       recent-reports
       recent-reviews
       recent-acknowledgements
       recent-agreements)
     '("Recent revisions"
       "Recent reports"
       "Recent reviews"
       "Recent acknowledgements"
       "Recent agreements")))

  (define powered-by-lunula
    (html:div ((id "bottom")) "powered by "
              (html:a ((href "http://fixedpoint.jp/lunula/") (target "_blank")) 'Lunula)))

  (define creativecommons-attribution
    (html:a ((href "http://creativecommons.org/licenses/by/2.1/jp/")
             (target "_blank"))
            "\"CreativeCommons 表示(Attribution)\""))

  (define creativecommons-attribution-logo
    (html:a ((rel "license") (href "http://creativecommons.org/licenses/by/2.1/jp/") (target "_blank"))
            (html:img ((alt "Creative Commons License") (style "border-width:0;") (src "http://i.creativecommons.org/l/by/2.1/jp/80x15.png")))))

  (define preload-script
    '("$(document).ready(function() {"
      "$('.links').corner();"
      "$('#private').corner();"
      "$('#public').corner();"
      "$('.corner').corner();"
      "$('.dog').corner('dog tr 15px');"
      "$('.parmalink').focus(function() {$(this).select();});"
      "});"))

  (define (menu uuid . _)
    (html:p
     ((id "menu"))
     (cond ((string? uuid)
            (html:span (html:a ((href (build-entry-path 'logout uuid))) "ログアウト")))
           (else
            (list
             (html:span (html:a ((href (build-entry-path 'login))) "ログイン"))
             "&nbsp;"
             (html:span (html:a ((href (build-entry-path 'sign-up))) "サインアップ")))))))

  (define *public-links* '((board . "書誌一覧")))

  (define *private-links*
    '((shelf . "書棚の閲覧")
      (put-on . "蔵書の登録")
      (edit-preference . "設定の編集")
      (modify-account . "アカウントの編集")
      (cancel . "アカウントの解除")
      ))

  (define (recent-acknowledgement uuid tuple)
    (match tuple
      ((ack a q rep r b)
       (html:p
        (html:a ((href (report->url rep uuid ack)))
                (html:escape-string (acknowledgement->caption ack)))))
      (_ "?")))

  (define (recent-agreement uuid tuple)
    (match tuple
      ((agr a c q rep r b)
       (html:p
        (html:a ((href (report->url rep uuid agr)))
                (html:escape-string (agreement->caption agr)))))
      (_ "?")))

  (define (recent-revision uuid tuple)
    (match tuple
      ((pub ex a r b)
       (cond ((bib-isbn10 b)
              => (lambda (isbn10)
                   (html:p (html:a ((href (build-api-path 'r
                                                          uuid
                                                          isbn10
                                                          (uri:encode-string (revision-name r))
                                                          (datetime->y/m/d (revision-revised-at r)))))
                                   (bib-title b)))))
             (else '())))
      (_ "?")))

  (define (recent-review uuid tuple)
    (match tuple
      ((rvw ex a r b)
       (cond ((bib-isbn10 b)
              => (lambda (isbn10)
                   (html:p
                    (html:a ((href (string-append
                                    (build-api-path 'r
                                                    uuid
                                                    isbn10
                                                    (uri:encode-string (revision-name r))
                                                    (datetime->y/m/d (revision-revised-at r)))
                                    "#review"
                                    (number->string (id-of rvw)))))
                            (html:escape-string
                             (string-truncate
                              (review-body rvw)
                              32))))))
             (else '())))
      (_ "?")))

  (define (recent-report uuid tuple)
    (match tuple
      ((rep a r b q c)
       (cond ((bib-isbn10 b)
              => (lambda (isbn10)
                   (html:p
                    (html:a ((href (string-append
                                    (build-api-path 'r
                                                    uuid
                                                    isbn10
                                                    (uri:encode-string (revision-name r))
                                                    (datetime->y/m/d (revision-revised-at r)))
                                    "#report"
                                    (number->string (id-of rep)))))
                            (html:escape-string
                             (string-truncate
                              (report-subject rep)
                              32))))))
             (else '())))
      (_ "?")))

  (define (links uuid . _)
    (define (p-link pair)
      (html:p (html:a ((href (build-entry-path (car pair) uuid))) (cdr pair))))
    (append
     (html:div
      (html:form ((action (build-entry-path 'find-bib uuid)))
                 "ISBN:"
                 (html:input ((type "text") (name "isbn")))
                 (html:input ((type "submit") (value (__ find-bib))))))
     (html:div
      ((class "links"))
      (html:div (map p-link *public-links*)))
     (with-uuid
      uuid
      (html:div
       ((class "links"))
       (html:div (map p-link *private-links*))))
     (html:div
      ((id "recent-revisions") (class "corner"))
      (html:h3 (__ recent-revisions))
      (map
       (lambda (tuple) (recent-revision uuid tuple))
       (recent-revisions 3)))
     (html:div
      ((id "recent-reviews") (class "corner"))
      (html:h3 (__ recent-reviews))
      (map
       (lambda (tuple) (recent-review uuid tuple))
       (recent-reviews 3)))
     (html:div
      ((id "recent-reports") (class "corner"))
      (html:h3 (__ recent-reports))
      (map
       (lambda (tuple) (recent-report uuid tuple))
       (recent-reports 3)))
     (html:div
      ((id "recent-acknowledgements") (class "corner"))
      (html:h3 (__ recent-acknowledgements))
      (map
       (lambda (tuple) (recent-acknowledgement uuid tuple))
       (recent-acknowledgements 3)))
     (html:div
      ((id "recent-agreements") (class "corner"))
      (html:h3 (__ recent-agreements))
      (map
       (lambda (tuple) (recent-agreement uuid tuple))
       (recent-agreements 3)))
     ))

  (define (belt uuid . _)
    (html:div
     ((id "belt"))
     (map
      (lambda (pair)
        (list
         (html:a ((href (build-entry-path (car pair) uuid))) (cdr pair))
         " | "))
      '((faq . "FAQ")
        (terms-of-service . "利用規約")
        (privacy-policy . "プライバシーポリシー")
        (feedback . "フィードバック")))
     " &copy; 2009 "
     (html:a ((href "http://fixedpoint.jp/")) "fixedpoint.jp")))

  (define (hidden-field name value)
    (html:input ((type "hidden") (name name) (value value))))

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
               (account-nick a)))

  (define-syntax with-uuid
    (syntax-rules ()
      ((_ uuid thunk)
       (if (string? uuid)
           thunk
           '()))))

  (define (bib->image b)
    (let ((image (bib-image b)))
      (cond ((blank? image)
             (html:img ((src "/image/no-image.png") (alt "No Image"))))
            ((isbn10->amazon (bib-isbn10 b))
             => (lambda (url)
                  (html:a ((href url) (target "_blank"))
                          (html:img ((src image) (alt (html:escape-string (bib-title b))) (style "border-width:0px;"))))))
            (else
             (html:img ((src image) (alt (html:escape-string (bib-title b))) (style "border-width:0px;")))))))

  (define-syntax revision-skeleton
    (syntax-rules ()
      ((_ b r x y z)
       (append
        (html:h3 (html:escape-string (bib-title b)))
        (html:table
         (html:tr
          (html:th
           ((rowspan 4))
           (bib->image b))
          (html:th ((rowspan 2) (style "text-align:left;")) (__ ISBN))
          (html:td ((style "color:#555555;")) (or (bib-isbn13 b) "-")))
         (html:tr
          (html:td ((style "color:#555555;")) (or (bib-isbn10 b) "-")))
         (html:tr
          (html:th ((style "text-align:left;")) (__ Revision))
          (html:td ((style "color:#555555;"))
                   (cons* (html:escape-string (revision-name r))
                          "(" (datetime->ymd (revision-revised-at r)) ")" x)))
         (html:tr
          (html:th y)
          (html:td z)))))))

  (define (go-to-board uuid)
    (html:form ((action (build-entry-path 'board uuid)))
               (html:input  ((type "submit") (value (__ to-board))))))

  (define (go-to-table uuid r)
    (html:form ((action (build-entry-path 'table uuid)))
               (hidden-field "id" (id-of r))
               (html:input ((type "submit") (value (__ to-table))))))

  (define (go-to-shelf uuid)
    (html:form ((action (build-entry-path 'shelf uuid)))
               (html:input ((type "submit") (value (__ to-shelf))))))

  (define (public-revisions uuid page)
    (with-pagination
     (board uuid page)
     (publicity (exlibris publicity) (account exlibris) (revision exlibris) (bib revision))
     ()
     ((order-by (publicity (created-at desc))))
     (lambda (tuple)
       (match tuple
         ((pub ex a r b)
          (append
           (revision-skeleton b r
                              '()
                              (go-to-table uuid r)
                              '())
           (revision-reviews r)
           (html:hr ((style "color:#999999;")))))
         (_ "?")))))

  (define (bib-window uuid id)
    (assert (integer? id))
    (let ((tuples (lookup-all (publicity
                               (exlibris publicity)
                               (account exlibris)
                               (revision exlibris)
                               (bib revision))
                              ((bib (id id)))
                              ((order-by (revision (revised-at desc)))))))
      (cond ((null? tuples)
             (__ bib-not-found))
            (else
             (let ((title (bib-title (list-ref (car tuples) 4))))
               (html:div
                (html:h3 (html:escape-string title))
                (html:p (__ following-revisions-found))
                (html:table
                 (html:tbody
                  (map
                   (lambda (tuple)
                     (match tuple
                       ((pub ex a r b)
                        (html:tr
                         (html:td (html:escape-string (revision-name r))
                                  "("
                                  (datetime->ymd (revision-revised-at r))
                                  ")")
                         (html:td (html:a ((href (build-api-path 'r
                                                                 uuid
                                                                 (bib-isbn10 b)
                                                                 (uri:encode-string (revision-name r))
                                                                 (datetime->y/m/d (revision-revised-at r)))))
                                          (__ permanent-link)))))
                       (_ '())))
                   tuples)))))))))

  (define (revision-window uuid id)
    (assert (integer? id))
    (match (lookup (revision (bib revision)) ((revision (id id))))
      ((r b) (revision-frame uuid b r))
      (_ "?")))

  (define (report-window uuid id)
    (assert (integer? id))
    (match (lookup (report (account report) (quotation report) (correction report) (revision report) (bib revision))
                   ((report (id id))))
      ((rep a q c r b) (report-frame uuid rep a q c r b))
      (_ "?")))

  (define (report-frame uuid rep a q c r b)
    (html:div
     (go-to-table uuid r)
     (revision-skeleton b r '() '() '())
     (html:h4 (__ Detail) "&nbsp;" creativecommons-attribution-logo)
     (diff-table
      (revision-report-tr uuid rep a q c '() (ack/nak-tr uuid rep q c)))))

  (define (diff-table x)
    (html:table
     ((class "diff"))
     (html:tr
      (html:th ((class "title")) (__ Quotation))
      (html:th ((class "title")) (__ Correction))
      (html:td))
     x))

  (define-syntax anchor
    (syntax-rules ()
      ((_ (record a0 ...) e0 ...)
       (html:a ((name (record-fragment record))
                a0 ...)
               e0 ...))))

  (define (revision-report-tr uuid rep a q c x y)
    (append
     (html:tr
      (html:td ((colspan 2) (style "font-size:small;"))
               (html:span ((class "pp")) "pp." (quotation-page q) "/" (quotation-position q)) "&nbsp;"
               (anchor (rep (class "subject")) (report-subject rep)) "&nbsp;"
               "("
               (html:span ((style "font-size:x-small;")) "reported by ")
               (signature a)
               (cond ((created-at-of rep) => (lambda (t) (cons "&nbsp;@&nbsp;" (html:span ((style "text-align:right;")) t))))
                     (else '()))
               ")"))
     (diff-tr uuid rep q c x)
     y))

  (define (review-div tuple)
    (match tuple
      ((rvw ex a)
       (html:div ((class "dog") (style "background-color:#c7ff6f;"))
                 (anchor (rvw) (signature a) ":")
                 (html:pre (html:escape-string (review-body rvw)))
                 ))
      (_ "?")))

  (define (revision-reviews r)
    (html:div
     (html:h4 (__ Review) "&nbsp;" creativecommons-attribution-logo)
     (let ((ls (lookup-all (review (exlibris review) (account exlibris)) ((exlibris (revision-id (id-of r)))))))
       (if (null? ls)
           "(なし)"
           (map review-div ls)))))

  (define (lcs-decoration class c rest)
    (case c
      ((#\linefeed)
       (cons (html:br) rest))
      ((#\space)
       (cons (html:span ((class class)) "&nbsp;") rest))
      (else
       (cons (html:span ((class class)) (html:escape-char c)) rest))))

  (define (lcs-filter-minus x pair)
    (match pair
      ((xa . xb)
       `(,(lcs-decoration "minus" x xa) . ,xb))))

  (define (lcs-filter-plus x pair)
    (match pair
      ((xa . xb)
       `(,xa . ,(lcs-decoration "plus" x xb)))))

  (define (lcs-filter-both x pair)
    (match pair
      ((xa . xb)
       (case x
         ((#\linefeed)
          `(,(cons (html:br) xa) . ,(cons (html:br) xb)))
         ((#\space)
          `(,(cons "&nbsp;" xa) . ,(cons "&nbsp;" xb)))
         (else
          `(,(cons (html:escape-char x) xa) . ,(cons (html:escape-char x) xb)))))))

  (define (diff-tr uuid rep q c forms)
    (let ((a (string->list (quotation-body q)))
          (b (string->list (correction-body c))))
      (match (lcs-fold
              lcs-filter-minus
              lcs-filter-plus
              lcs-filter-both
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

  (define (acknowledgement-view ack a)
    (html:div
     (anchor (ack (class "credit")) (signature a) ":&nbsp;")
     (html:span ((class (if (acknowledgement-positive? ack) "ack" "nak")))
                (html:escape-string (acknowledgement-comment ack)))))

  (define (agreement-view agr a)
    (html:div
     (anchor (agr (class "credit")) (signature a) ":&nbsp;")
     (html:span (html:escape-string (agreement-comment agr)))))    

  (define (ack/nak-tr uuid rep q c)
    (append
     (with-uuid
      uuid
      (html:tr
       (html:td
        (html:form ((action (build-entry-path 'acknowledge uuid)))
                   (hidden-field "report" (id-of rep))
                   (hidden-field "quotation" (id-of q))
                   (html:input ((type "submit") (value (__ acknowledge))))))
       (html:td
        (html:form ((action (build-entry-path 'agree uuid)))
                   (hidden-field "report" (id-of rep))
                   (hidden-field "correction" (id-of c))
                   (html:input ((type "submit") (value (__ agree))))))))
     (html:tr
      (html:td
       (let ((tuples (lookup-all (acknowledgement (account acknowledgement))
                                 ((acknowledgement (quotation-id (id-of q))))
                                 ((order-by (acknowledgement (created-at desc)))))))
         (if (null? tuples)
             '()
             (html:div
              ((class "acknowledgement"))
              (map
               (lambda (tuple)
                 (match tuple
                   ((ack a) (acknowledgement-view ack a))
                   (_ "?")))
               tuples))))
       (html:td
        (let ((tuples (lookup-all (agreement (account agreement))
                                  ((agreement (correction-id (id-of c))))
                                  ((order-by (agreement (created-at desc)))))))
          (if (null? tuples)
              '()
              (html:div
               ((class "agreement"))
               (map
                (lambda (tuple)
                  (match tuple
                    ((agr a) (agreement-view agr a))
                    (_ "?")))
                tuples)))))))))

  (define (revision-reports uuid r proc)
    (diff-table
     (map
      (lambda (tuple)
        (match tuple
          ((rep a q c) (revision-report-tr uuid rep a q c (proc rep) '()))))
      (list-sort
       (lambda (t0 t1)
         (page<? (quotation-page (caddr t0))
                 (quotation-page (caddr t1))))
       (lookup-all (report (account report) (quotation report) (correction report))
                   ((report (revision-id (id-of r)))))))))

  (define (revision-frame uuid b r)
    (html:div
     (go-to-board uuid)
     (revision-skeleton b r
                        '()
                        (__ parmalink)
                        (cond ((bib-isbn10 b)
                               => (lambda (isbn10)
                                    (html:input ((class "parmalink")
                                                 (type "text")
                                                 (readonly #t)
                                                 (size 64)
                                                 (value
                                                  (string-append
                                                   url-base
                                                   (build-api-path 'r
                                                                   #f
                                                                   isbn10
                                                                   (uri:encode-string (revision-name r))
                                                                   (datetime->y/m/d (revision-revised-at r)))))))))
                              (else '())))
     (html:h4 (__ Table) "&nbsp;" creativecommons-attribution-logo)
     (revision-reports uuid r (lambda (rep)
                                (html:form ((action (build-entry-path 'detail uuid)))
                                           (hidden-field "id" (id-of rep))
                                           (html:input ((type "submit") (value (__ to-detail)))))))))

  (define (exlibris-panel uuid b r ex)
    (html:div
     (revision-skeleton b r
                        '()
                        (html:form ((action (build-entry-path 'desk uuid)))
                                   (hidden-field "id" (id-of ex))
                                   (html:input ((type "submit") (value (__ to-desk)))))
                        (html:form ((action (build-entry-path 'put-at-top uuid)))
                                   (hidden-field "id" (id-of ex))
                                   (html:input ((type "submit") (value (__ put-at-top))))))
     (html:hr ((style "color:#999999;")))))

  (define (exlibris-frame uuid b r ex)
    (let ((id (id-of ex)))
      (html:div
       (go-to-shelf uuid)
       (revision-skeleton b r
                          (html:form ((action (build-entry-path 'modify-revision uuid)))
                                     (hidden-field "id" id)
                                     (html:input ((type "submit") (value (__ modify-revision)))))
                          (cond ((lookup publicity ((exlibris-id id)))
                                 => (lambda (pub)
                                      (html:form ((action (build-entry-path 'hide-exlibris uuid)))
                                                 (hidden-field "id" id)
                                                 (html:input ((type "submit") (value (__ hide-exlibris)))))))
                                (else
                                 (html:form ((action (build-entry-path 'share-exlibris uuid)))
                                            (hidden-field "id" id)
                                            (html:input ((type "submit") (value (__ share-exlibris)))))))
                          (html:form ((action (build-entry-path 'put-off uuid)))
                                     (hidden-field "id" id)
                                     (html:input ((type "submit") (value (__ put-off))))))
       (html:h4 (__ Review) "&nbsp;" creativecommons-attribution-logo)
       (html:form ((action (build-entry-path 'edit-review uuid)))
                  (hidden-field "id" id)
                  (html:input ((type "submit") (value (__ edit-review)))))
       (cond ((lookup (review (exlibris review) (account exlibris)) ((exlibris (id id)))) => review-div)
             (else "(なし)"))
       (html:h4 (__ Table) "&nbsp;" creativecommons-attribution-logo)
       (html:div
        (html:form ((action (build-entry-path 'new-report uuid)))
                   (hidden-field "id" (id-of ex))
                   (html:input ((type "submit") (value (__ new-report))))))
       (revision-reports uuid r (lambda (rep)
                                  (append
                                   (html:form ((action (build-entry-path 'modify-report uuid)))
                                              (hidden-field "report" (id-of rep))
                                              (hidden-field "exlibris" (id-of ex))
                                              (html:input ((type "submit") (value (__ modify-report)))))
                                   (html:form ((action (build-entry-path 'drop-report uuid)))
                                              (hidden-field "report" (id-of rep))
                                              (hidden-field "exlibris" (id-of ex))
                                              (html:input ((type "submit") (value (__ drop-report)))))))))))

  (define (shelf-window uuid body)
    (match body
      ((id page)
       (with-pagination
        (shelf uuid page)
        (exlibris (revision exlibris) (bib revision))
        ((exlibris (account-id id)))
        ((order-by (exlibris (position asc) (updated-at desc))))
        (lambda (tuple)
          (match tuple
            ((ex r b) (exlibris-panel uuid b r ex))
            (_ "?")))))
      (_ "??")))

  (define (exlibris-window uuid id)
    (match (lookup (exlibris (revision exlibris) (bib revision)) ((exlibris (id id))))
      ((ex r b) (exlibris-frame uuid b r ex))
      (_ "?")))

  )
