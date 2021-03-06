(library (errata helper)
  (export errata-description
          errata-keywords
          errata-logo
          errata-rss-links
          jquery-scripts
          powered-by-lunula
          powered-by-mathjax
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
          report-history-window
          report-window
          review-div
          revision-report-tr
          diff-table
          diff-tr
          revision-window
          shelf-window
          exlibris-window
          notification-window)
  (import (except (rnrs) div)
          (only (core) format)
          (match)
          (only (srfi :13) string-tokenize)
          (only (srfi :19) date->string date-year)
          (only (lcs) lcs-fold)
          (prefix (only (uri) encode-string) uri:)
          (only (lunula gettext) __ ___)
          (only (lunula md) md5)
          (only (lunula mysql) lookup lookup-all)
          (prefix (lunula html) html:)
          (only (lunula path) build-entry-path build-api-path)
          (only (lunula persistent-record) id-of created-at-of)
          (only (lunula session) account account-nick account-name account-mail-address)
          (only (lunula string) blank? string-truncate)
          (only (errata calendar) ad->japanese-era datetime->date datetime->y/m/d)
          (only (errata configuration) url-base google-cx)
          (only (errata font) face->style)
          (only (errata isbn) pretty-isbn13 isbn10->amazon)
          (errata model)
          (only (errata notification) notification notification-subject notification-body notification->url)
          (errata helper pagination)
          (errata page)
          (only (errata url) bib&revision->url record->fragment report->url))

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
     (lambda (name)
       (html:link ((href (format "/~a.rss" name)) (rel "alternate") (type "application/rss+xml") (title (___ name)))))
     '(recent-revisions
       recent-reports
       recent-reviews
       recent-acknowledgements
       recent-agreements)))

  (define jquery-scripts
    (map
     (lambda (src)
       (html:script ((type "text/javascript") (src (format "/javascript/~a" src)))))
     '(jquery-1.4.2.min.js
       jquery.corner.js
       MathJax/MathJax.js)))

  (define powered-by-lunula
    (html:div ((id "bottom")) "powered by "
              (html:a ((href "http://fixedpoint.jp/lunula/") (target "_blank")) 'Lunula)))

  (define powered-by-mathjax
    (let ((title "Powered by MathJax")
          (url "http://www.mathjax.org/"))
      (html:a ((href url)
               (target "_blank"))
              (html:img ((src (string-append url "/badge.gif"))
                         (border 0)
                         (alt title)
                         (title title))))))

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
      "$('.permalink').focus(function() {$(this).select();});"
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
    '((inbox . "Inbox")
      (shelf . "書棚の閲覧")
      (put-on . "蔵書の登録")
      (edit-preference . "設定の編集")
      (modify-account . "アカウントの編集")
      (modify-password . "パスワードの編集")
      (cancel . "アカウントの解除")
      ))

  (define (recent-acknowledgement uuid tuple)
    (match tuple
      ((ack a pref q rep r b)
       (html:p
        (html:a ((href (report->url rep uuid ack)))
                (html:escape-string (acknowledgement->caption ack)))))
      (_ "?")))

  (define (recent-agreement uuid tuple)
    (match tuple
      ((agr a pref c q rep r b)
       (html:p
        (html:a ((href (report->url rep uuid agr)))
                (html:escape-string (agreement->caption agr)))))
      (_ "?")))

  (define (recent-revision uuid tuple)
    (match tuple
      ((pub ex a pref r b)
       (html:p (html:a ((href (bib&revision->url b r uuid))) (bib-title b))))
      (_ "?")))

  (define (recent-review uuid tuple)
    (match tuple
      ((rvw ex a pref r b)
       (html:p (html:a ((href (bib&revision->url b r uuid rvw)))
                       (html:escape-string (review->caption rvw)))))
      (_ "?")))

  (define (recent-report uuid tuple)
    (match tuple
      ((rep a pref r b q o c)
       (html:p (html:a ((href (bib&revision->url b r uuid rep)))
                       (html:escape-string (report->caption rep)))))
      (_ "?")))

  (define (google-search-form)
    (format "<form action=\"http://www.google.co.jp/cse\" id=\"cse-search-box\" target=\"_blank\">
 <div>
  <input type=\"hidden\" name=\"cx\" value=\"~a\" />
  <input type=\"hidden\" name=\"ie\" value=\"UTF-8\" />
  <input type=\"text\" name=\"q\" size=\"20\" />
  <input type=\"submit\" name=\"sa\" value=\"&#x691c;&#x7d22;\" />
 </div>
</form>
<script type=\"text/javascript\" src=\"http://www.google.co.jp/cse/brand?form=cse-search-box&amp;lang=ja\"></script>"
            google-cx))

  (define (links uuid . _)
    (define (p-link pair)
      (html:p (html:a ((href (build-entry-path (car pair) uuid))) (cdr pair))))
    (append
     `(,(google-search-form))
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
     " &copy; 2009,2010 "
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

  (define *gravatar-size* 24)

  (define (optional-gravatar a pref)
    (if (and (number? (preference-gravatar pref))
             (= 1 (preference-gravatar pref)))
        (html:img ((src (format "http://www.gravatar.com/gravatar/~a.jpg?s=~d"
                                (md5 (string->utf8 (account-mail-address a)))
                                *gravatar-size*))
                   (alt (account-nick a))
                   (width *gravatar-size*)
                   (height *gravatar-size*)))
        (account-nick a)))

  (define (signature a pref)
    (html:span ((title (html:escape-string (account-name a))))
               (optional-gravatar a pref)))

  (define-syntax with-uuid
    (syntax-rules ()
      ((_ uuid thunk)
       (if (string? uuid)
           thunk
           '()))))

  (define (bib->image b)
    (let* ((image (bib-image b))
           (img (if (blank? image)
                    (html:img ((src "/image/no-image.png") (alt "No Image")))
                    (html:img ((src image) (alt (html:escape-string (bib-title b))) (style "border-width:0px;"))))))
      (cond ((isbn10->amazon (bib-isbn10 b))
             => (lambda (url) (html:a ((href url) (target "_blank")) img)))
            (else img))))

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
          (html:td ((style "color:#555555;")) (cond ((bib-isbn13 b) => pretty-isbn13) (else "-"))))
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

  (define (go-to-detail uuid rep)
    (html:form ((action (build-entry-path 'detail uuid)))
               (hidden-field "id" (id-of rep))
               (html:input ((type "submit") (value (__ to-detail))))))

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
    (let ((tuples (lookup-all (revision
                               (bib revision))
                              ((bib (id id))
                               (exists (publicity
                                        (exlibris publicity)
                                        (account exlibris))
                                       ((exlibris (revision)))))
                              ((order-by (revision (revised-at desc)))))))
      (cond ((null? tuples)
             (__ bib-not-found))
            (else
             (let ((title (bib-title (cadr (car tuples)))))
               (html:div
                (html:h3 (html:escape-string title))
                (html:p (__ following-revisions-found))
                (html:table
                 (html:tbody
                  (map
                   (lambda (tuple)
                     (match tuple
                       ((r b)
                        (html:tr
                         (html:td (html:escape-string (revision-name r))
                                  "("
                                  (datetime->ymd (revision-revised-at r))
                                  ")")
                         (html:td (html:a ((href (bib&revision->url b r uuid)))
                                          (__ permanent-link)))))
                       (_ '())))
                   tuples)))))))))

  (define (revision-window uuid id)
    (assert (integer? id))
    (match (lookup (revision (bib revision)) ((revision (id id))))
      ((r b) (revision-frame uuid b r))
      (_ "?")))

  (define (report-history-window uuid id)
    (assert (integer? id))
    (cond ((lookup (report (revision report) (bib revision)) ((report (id id))))
           => (lambda (tuple0)
                (match tuple0
                  ((rep r b)
                   (html:div
                    (go-to-detail uuid rep)
                    (revision-skeleton b r '() '() '())
                    (html:h4 (__ History) "&nbsp;" creativecommons-attribution-logo)
                    (diff-table
                     (map
                      (lambda (tuple1)
                        (match tuple1
                          ((rh a pref q o c)
                           (report-history-tr uuid rh a pref q o c))
                          (_ "?")))
                      (lookup-all (report-history
                                   (account report-history)
                                   (preference (account left))
                                   (quotation report-history)
                                   (occurrence report-history)
                                   (correction report-history))
                                  ((report-history (uuid (report-uuid rep))))
                                  ((order-by (report-history (created-at desc)))))))))
                  (_ '()))))
          (else '())))

  (define-syntax report-history-tr
    (syntax-rules ()
      ((_ uuid rh a pref q o c)
       (append
        (html:tr
         (html:td ((colspan 2) (style "font-size:small;"))
                  (html:span ((class "page")) "page " (occurrence-page o) "/" (occurrence-position o)) "&nbsp;"
                  (anchor (rh (class "subject")) (report-history-subject rh)) "&nbsp;"
                  "("
                  (html:span ((style "font-size:x-small;")) "reported by ")
                  (signature a pref)
                  ")"))
        (diff-tr uuid q c)
        (ack/nak-tr uuid q c '())))))

  (define (report-window uuid id)
    (assert (integer? id))
    (match (lookup (report
                    (account report)
                    (preference (account left))
                    (quotation report)
                    (occurrence report)
                    (correction report)
                    (revision report)
                    (bib revision))
                   ((report (id id))))
      ((rep a pref q o c r b) (report-frame uuid rep a pref q o c r b))
      (_ "?")))

  (define (report-frame uuid rep a pref q o c r b)
    (html:div
     (go-to-table uuid r)
     (revision-skeleton b r '() '() '())
     (html:h4 (__ Detail) "&nbsp;" creativecommons-attribution-logo)
     (diff-table
      (revision-report-tr uuid rep a pref q o c
                          (html:form ((action (build-entry-path 'show-report-history uuid)))
                                     (hidden-field "id" (id-of rep))
                                     (html:input ((type "submit") (value (__ show-report-history)))))
                          (ack/nak-tr uuid q c
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
                                                    (html:input ((type "submit") (value (__ agree)))))))))))))

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
       (html:a ((name (record->fragment record))
                a0 ...)
               e0 ...))))

  (define (revision-report-tr uuid rep a pref q o c x y)
    (append
     (html:tr
      (html:td ((colspan 2) (style "font-size:small;"))
               (html:span ((class "page")) "page " (occurrence-page o) "/" (occurrence-position o)) "&nbsp;"
               (anchor (rep (class "subject")) (report-subject rep)) "&nbsp;"
               "("
               (html:span ((style "font-size:x-small;")) "reported by ")
               (signature a pref)
               (cond ((created-at-of rep)
                      => (lambda (t) (cons "&nbsp;@&nbsp;" (html:span ((style "text-align:right;")) (datetime->ymd t)))))
                     (else '()))
               ")"))
     (diff-tr uuid q c x)
     y))

  (define (review-div tuple)
    (match tuple
      ((rvw ex a pref)
       (html:div ((class "dog") (style "background-color:#c7ff6f;"))
                 (anchor (rvw) (signature a pref) ":")
                 (html:pre (html:escape-string (review-body rvw)))
                 ))
      (_ "?")))

  (define (revision-reviews r)
    (html:div
     (html:h4 (__ Review) "&nbsp;" creativecommons-attribution-logo)
     (let ((ls (lookup-all (review
                            (exlibris review)
                            (account exlibris)
                            (preference (account left)))
                           ((exlibris (revision-id (id-of r)))))))
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

  (define (tex-display body)
    (html:script ((type "math/tex; mode=display")) body))

  (define (tex-text body)
    (html:script ((type "math/tex")) body))

  (define (diff-tr uuid q c . forms)
    (let* ((q-body (quotation-body q))
           (c-body (correction-body c))
           (a (string->list q-body))
           (b (string->list c-body)))

      (define (side ff body zc)
        (html:td ((class "width:49%;"))
                 (html:blockquote
                  (let ((bq (html:span ((style (face->style ff))) (reverse zc))))
                    (cond ((string=? "tex-display" ff)
                           (cons
                            (html:span ((class "MathJax_Preview")) bq)
                            (tex-display (html:escape-string body))))
                          ((string=? "tex-text" ff)
                           (cons
                            (html:span ((class "MathJax_Preview")) bq)
                            (tex-text (html:escape-string body))))
                          (else bq))))))

      (match (lcs-fold
              lcs-filter-minus
              lcs-filter-plus
              lcs-filter-both
              '(() . ())
              a
              b)
        ((xa . xb)
         (html:tr
          (side (quotation-font-face q) q-body xa)
          (side (correction-font-face c) c-body xb)
          (html:td forms))))))

  (define (acknowledgement-view ack a pref)
    (html:div
     (anchor (ack (class "credit")) (signature a pref) ":&nbsp;")
     (html:span ((class (if (acknowledgement-positive? ack) "ack" "nak")))
                (html:escape-string (acknowledgement-comment ack)))))

  (define (agreement-view agr a pref)
    (html:div
     (anchor (agr (class "credit")) (signature a pref) ":&nbsp;")
     (html:span (html:escape-string (agreement-comment agr)))))

  (define (ack/nak-tr uuid q c x)
    (append
     x
     (html:tr
      (html:td
       (let ((tuples (lookup-all (acknowledgement
                                  (account acknowledgement)
                                  (preference (account left)))
                                 ((acknowledgement (quotation-id (id-of q))))
                                 ((order-by (acknowledgement (created-at desc)))))))
         (if (null? tuples)
             '()
             (html:div
              ((class "acknowledgement"))
              (map
               (lambda (tuple)
                 (apply acknowledgement-view tuple))
               tuples))))
       (html:td
        (let ((tuples (lookup-all (agreement
                                   (account agreement)
                                   (preference (account left)))
                                  ((agreement (correction-id (id-of c))))
                                  ((order-by (agreement (created-at desc)))))))
          (if (null? tuples)
              '()
              (html:div
               ((class "agreement"))
               (map
                (lambda (tuple)
                  (apply agreement-view tuple))
                tuples)))))))))

  (define-syntax report-table
    (syntax-rules ()
      ((_ uuid r proc tuples)
       (diff-table
        (map
         (lambda (tuple)
           (match tuple
             ((rep a pref q o c) (revision-report-tr uuid rep a pref q o c (proc rep) '()))))
         (list-sort
          (lambda (t0 t1)
            (page<? (occurrence-page (cadddr (cdr t0)))
                    (occurrence-page (cadddr (cdr t1)))))
          tuples))))))

  (define (revision-reports uuid r proc)
    (report-table uuid r proc
                  (lookup-all (report
                               (account report)
                               (preference (account left))
                               (quotation report)
                               (occurrence report)
                               (correction report))
                              ((report (revision-id (id-of r)))
                               (exists (publicity (exlibris publicity))
                                       ((exlibris (account))
                                        (exlibris (revision-id (id-of r)))))))))

  (define (revision-frame uuid b r)
    (html:div
     (go-to-board uuid)
     (revision-skeleton b r
                        '()
                        (__ permalink)
                        (html:input ((class "permalink")
                                     (type "text")
                                     (readonly #t)
                                     (size 64)
                                     (value
                                      (string-append
                                       url-base
                                       (bib&revision->url b r))))))
     (with-uuid uuid
                (html:form ((action (build-entry-path 'import-bib uuid)))
                           (hidden-field "id" (id-of b))
                           (html:input ((type "submit") (value (__ import-bib))))))
     (html:h4 (__ Table) "&nbsp;" creativecommons-attribution-logo)
     (revision-reports uuid r (lambda (rep) (go-to-detail uuid rep)))))

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

  (define (exlibris-reports uuid ex r proc)
    (report-table uuid r proc
                  (append
                   (if (lookup publicity ((exlibris-id (id-of ex))))
                       '() ; the public ones suffice
                       (lookup-all (report
                                    (account report)
                                    (preference (account left))
                                    (quotation report)
                                    (occurrence report)
                                    (correction report))
                                   ((report (revision-id (id-of r)))
                                    (account (id (exlibris-account-id ex))))))
                   (lookup-all (report
                                (account report)
                                (preference (account left))
                                (quotation report)
                                (occurrence report)
                                (correction report))
                               ((report (revision-id (id-of r)))
                                (exists (publicity (exlibris publicity))
                                        ((exlibris (account))
                                         (exlibris (revision-id (id-of r))))))))))

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
       (cond ((lookup (review (exlibris review) (account exlibris) (preference (account left))) ((exlibris (id id)))) => review-div)
             (else "(なし)"))
       (html:h4 (__ Table) "&nbsp;" creativecommons-attribution-logo)
       (html:div
        (html:form ((action (build-entry-path 'new-report uuid)))
                   (hidden-field "id" (id-of ex))
                   (html:input ((type "submit") (value (__ new-report))))))
       (exlibris-reports uuid ex r (lambda (rep)
                                     (append
                                      (html:form ((action (build-entry-path 'add-occurrence uuid)))
                                                 (hidden-field "report" (id-of rep))
                                                 (hidden-field "exlibris" (id-of ex))
                                                 (html:input ((type "submit") (value (__ add-occurrence)))))
                                      (if (= (exlibris-account-id ex) (report-account-id rep)) ; it is one's own report
                                          (append
                                           (html:form ((action (build-entry-path 'modify-report uuid)))
                                                      (hidden-field "report" (id-of rep))
                                                      (hidden-field "exlibris" (id-of ex))
                                                      (html:input ((type "submit") (value (__ modify-report)))))
                                           (html:form ((action (build-entry-path 'drop-report uuid)))
                                                      (hidden-field "report" (id-of rep))
                                                      (hidden-field "exlibris" (id-of ex))
                                                      (html:input ((type "submit") (value (__ drop-report))))))
                                          '())))))))

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

  (define (notification-window uuid id)
    (let ((ls (lookup-all
               (notification)
               ((notification (account-id id)))
               ((order-by (notification (created-at desc)))))))
      (if (null? ls)
          (html:p "(新しい通知はありません)")
          (html:ul
           (map
            (lambda (tuple)
              (match tuple
                ((nt)
                 (html:li
                  (html:escape-string (notification-subject nt))
                  (html:span ((style "font-size:x-small;"))
                             "(" (datetime->ymd (created-at-of nt)) ")")
                  (html:form ((action (build-entry-path 'drop-notification uuid)))
                             (hidden-field "id" (id-of nt))
                             (html:input ((type "submit") (value (__ drop-notification)))))
                  (html:br)
                  (html:a ((href (notification->url nt uuid)))
                          (html:q (html:escape-string (notification-body nt))))))
                (_ '())))
            ls)))))

)
