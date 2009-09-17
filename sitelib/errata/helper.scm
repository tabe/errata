(library (errata helper)
  (export errata-keywords
          errata-logo
          powered-by-lunula
          creativecommons-attribution-logo
          creativecommons-attribution
          preload-script
          menu
          links
          belt
          public-revisions
          bib-window
          report-window
          revision-window
          shelf-window
          exlibris-window
          build-entry-path)
  (import (except (rnrs) div)
          (only (core) format)
          (match)
          (only (srfi :13) string-tokenize)
          (only (srfi :19) date->string string->date date-year)
          (only (lcs) lcs-fold)
          (prefix (only (uri) encode-string) uri:)
          (only (lunula gettext) __)
          (only (lunula mod_lisp) entry-paths build-entry-path build-api-path)
          (only (lunula mysql) lookup lookup-all)
          (prefix (lunula html) html:)
          (only (lunula persistent-record) id-of created-at-of)
          (only (lunula session) account account-nick account-name)
          (only (lunula string) blank?)
          (only (errata calendar) ad->japanese-era)
          (only (errata isbn) isbn10->amazon)
          (errata model)
          (errata helper pagination))

  (define errata-keywords 
    (html:meta ((name "keywords") (content "errata,typo,正誤表,誤植,タイポ,共有"))))

  (define (errata-logo uuid . _)
    (html:h1 ((id "logo") (title "えらった べーた"))
             (html:a ((href (build-entry-path 'index uuid)))
                     (cons "Errata" (html:span ((style "color:red;")) "β")))))

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
      "$('.dog').corner('dog tr 15px');"
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
      (modify-account . "アカウントの編集")
      (cancel . "アカウントの解除")
      ))

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
       (html:div (map p-link *private-links*))))))

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

  (define (datetime->date str)
    (guard (e (else #f))
      (string->date str "~Y-~m-~d ~H:~M:~S")))

  (define (date->ymd date)
    (let ((year (date-year date)))
      (guard (e
              (else (date->string date "~Y-~m-~d")))
        (append (html:span ((title (ad->japanese-era year))) year)
                (date->string date "-~m-~d")))))

  (define (date->y/m/d date)
    (date->string date "~Y/~m/~d"))

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
          (html:td y)
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
                                                                 (date->y/m/d (datetime->date (revision-revised-at r))))))
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

  (define (revision-report-tr uuid rep a q c x y)
    (append
     (html:tr
      (html:td ((colspan 2) (style "font-size:small;"))
               "pp." (quotation-page q) "/" (quotation-position q) "&nbsp;"
               (report-subject rep) "&nbsp;"
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
                 (signature a) ":"
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
                   ((ack a)
                    (html:div
                     (html:span ((class "credit"))
                                (signature a)
                                ":&nbsp;")
                     (html:span ((class (if (acknowledgement-positive? ack) "ack" "nak")))
                                (html:escape-string (acknowledgement-comment ack)))))
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
                    ((agr a)
                     (html:div
                      (html:span ((class "credit"))
                                 (signature a)
                                 ":&nbsp;")
                      (html:span (html:escape-string (agreement-comment agr)))))
                    (_ "?")))
                tuples)))))))))

  (define (revision-reports uuid r proc)
    (diff-table
     (map
      (lambda (tuple)
        (match tuple
          ((rep a q c)
           (revision-report-tr uuid rep a q c (proc rep) '()))
          (_ "?")))
      (lookup-all (report (account report) (quotation report) (correction report))
                  ((report (revision-id (id-of r))))
                  ((order-by (quotation (page asc))))))))

  (define (revision-frame uuid b r)
    (html:div
     (go-to-board uuid)
     (revision-skeleton b r '() '() '())
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
                        '())
     (html:hr ((style "color:#999999;")))))

  (define (exlibris-frame uuid b r ex)
    (let ((id (id-of ex)))
      (html:div
       (go-to-shelf uuid)
       (revision-skeleton b r
                          (html:form ((action (build-entry-path 'modify-revision uuid)))
                                     (hidden-field "id" id)
                                     (html:input ((type "submit") (value (__ modify-revision)))))
                          (cond ((lookup publicity `((exlibris-id ,id)))
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
        ((order-by (exlibris (updated-at desc))))
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
