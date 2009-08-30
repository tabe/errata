(library (errata)
  (export start
          connect
          close
          cancel
          sign-up
          login
          logout
          put-on)
  (import (rnrs)
          (pregexp)
          (only (srfi :13) string-null?)
          (srfi :48)
          (lunula)
          (prefix (lunula html) html:)
          (lunula tree)
          (lunula gettext)
          (only (errata query) query-image)
          (only (errata isbn) valid-isbn?)
          (errata model))

  (define (blank? x)
    (or (not x)
        (and (string? x) (string-null? x))))

  (define (string->id str)
    (cond ((string->number str)
           => (lambda (id)
                (and (exact? id)
                     (fixnum? id)
                     (< 0 id)
                     id)))
          (else #f)))

  (define (string->page str)
    (cond ((string->number str)
           => (lambda (page)
                (and (exact? page)
                     (fixnum? page)
                     (<= 0 page)
                     page)))
          (else #f)))

  (define (valid-account? a)
    (and (account? a)
         (let ((nick (account-nick a))
               (password (account-password a)))
           (and (pregexp-match "^[A-Za-z_][A-Za-z0-9_]*$" nick)
                (<= 8 (string-length password))))))

  (define (new-account? a)
    (and (account? a)
         (not (lookup account `((nick ,(account-nick a))
                                (password ,(account-password a)))))))

  (define (authenticate-account a)
    (and (account? a)
         (lookup account `((nick ,(account-nick a))
                           (password ,(account-password a))))))

  (define (yes? c) (pregexp-match "^[yY]" (ok? c)))

  (define-syntax with-session
    (syntax-rules ()
      ((_ (io request) proc)
       (cond ((logged-in? (parameter-of request)) => proc)
             (else (redirect (io) "/"))))))

  (define-syntax without-session
    (syntax-rules ()
      ((_ (io request) thunk)
       (cond ((logged-in? (parameter-of request))
              => (lambda (sess) (page (io sess) base (__ you-have-already-logged-in))))
             (else thunk)))))

  (define-scenario (sign-up io request)
    (without-session
     (io request)
     (let lp ((a (form (io) (account) base)))
       (cond ((and (valid-account? a)
                   (new-account? a))
              (if (save a)
                  (page (io) base (__ now-you-have-your-own-account))
                  (page (io) base (__ hmm-we-have-failed-to-create-your-account))))
             (else
              (lp (form (io) (account a) base (__ please-retry))))))))

  (define-scenario (modify-account io request)
    (with-session
     (io request)
     (lambda (sess)
       (let loop ((a (form (io sess) (account (user-account (session-user sess))) base)))
         (cond ((valid-account? a)
                (account-id-set! a (account-id (user-account (session-user sess))))
                (if (save a)
                    (page (io sess) base (__ your-account-has-been-updated))
                    (page (io sess) base (__ hmm-an-error-occurred))))
               (else
                (loop (form (io sess) (account a) base (__ check-your-data)))))))))

  (define-scenario (cancel io request)
    (with-session
     (io request)
     (lambda (sess)
       (let ((c (form (io sess) (confirmation) base (__ are-you-sure?))))
         (cond ((yes? c)
                (if (destroy (user-account (session-user sess)))
                    (page (io) base (__ now-you-have-left-errata))
                    (page (io sess) base (__ hmm-an-error-occurred))))
               (else
                (page (io sess) base (__ of-course-we-know-you-are-kidding))))))))

  (define-scenario (login io request)
    (without-session
     (io request)
     (let loop ((a (form (io) (account-to-login) base)))
       (cond ((and (valid-account? a)
                   (authenticate-account a))
              => (lambda (a)
                   (let ((sess (do-login a)))
                     (page (io sess) base (__ now-you-have-logged-in)))))
             (else
              (loop (form (io) (account-to-login a) base (__ please-retry))))))))

  (define-scenario (logout io request)
    (with-session
     (io request)
     (lambda (sess)
       (do-logout sess)
       (page (io) base (__ now-you-have-logged-out)))))

  (define (valid-revision? rev)
    (and (revision? rev)
         (not (string-null? (revision-name rev)))))

  (define-scenario (put-on io request)
    (with-session
     (io request)
     (lambda (sess)

       (define (specify-bib b)

         (define (confirm-bib isbn13 isbn10 url title)
           (let ((c (form (io sess) (confirmation) base
                          (__ is-this-content-ok?)
                          (tree->string
                           (if (eof-object? url)
                               (list (html:br)
                                     (bib-title b))
                               (list (html:br)
                                     title
                                     (html:br)
                                     (html:image ((src url)))))))))

             (define (specify-revision r)
               (cond ((valid-revision? r)
                      (revision-bib-id-set! r (bib-id b))
                      (if (save r)
                          (let ((ex (make-exlibris #f
                                                   (account-id (user-account (session-user sess)))
                                                   (revision-id r))))
                            (if (save ex)
                                (page (io sess) base (__ you-are-done))
                                (page (io sess) base (__ hmm-we-have-failed-to-put-on-your-exlibris))))
                          (page (io sess) base (__ hmm-we-have-failed-to-put-on-your-exlibris))))
                     (else
                      (specify-revision (form (io sess) (revision r) base (__ please-retry))))))

             (cond ((pregexp-match "^[yY]" (ok? c))
                    (unless (eof-object? isbn13)
                      (bib-isbn13-set! b isbn13))
                    (unless (eof-object? isbn10)
                      (bib-isbn10-set! b isbn10))
                    (unless (eof-object? title)
                      (bib-title-set! b title))
                    (unless (eof-object? url)
                      (bib-image-set! b url))
                    (cond ((save b)
                           (specify-revision
                            (form (io sess) (revision) base (__ now-new-book-has-been-put-on) (bib-title b))))
                          (else
                           (page (io sess) base (__ hmm-we-have-failed-to-put-on-your-exlibris)))))
                   (else
                    (specify-bib (form (io sess) (bib b) base (__ please-retry)))))))

         (let ((title (bib-title b))
               (isbn (bib-isbn13 b)))
           (cond ((and (blank? title)
                       (blank? isbn))
                  (specify-bib (form (io sess) (bib b) base (__ please-input-title-or-isbn))))
                 ((blank? isbn)
                  (confirm-bib (eof-object) (eof-object) (eof-object) (eof-object)))
                 ((valid-isbn? isbn)
                  (call/cc
                   (lambda (cont)
                     (let ((info (guard (e
                                         ((i/o-error? e)
                                          (write e)
                                          (newline)
                                          (cont (specify-bib (form (io sess) (bib b) base (__ hmm-an-error-occurred))))))
                                   (query-image isbn))))
                       (cond ((eof-object? info)
                              (specify-bib (form (io sess) (bib b) base (__ please-check-isbn))))
                             (else
                              (call-with-port (open-string-input-port info)
                                (lambda (port)
                                  (let* ((isbn13 (get-line port))
                                         (isbn10 (get-line port))
                                         (url    (get-line port))
                                         (title  (get-line port)))
                                    (confirm-bib isbn13 isbn10 url title))))))))))
                 (else
                  (specify-bib (form (io sess) (bib b) base (__ please-check-isbn)))))))

       (specify-bib (form (io sess) (bib) base)))))

  (define-syntax with-session&id
    (syntax-rules ()
      ((_ (io request data) proc)
       (with-session
        (io request)
        (lambda (sess)
          (cond ((and data (assq 'id (content->alist data)))
                 => (lambda (pair)
                      (cond ((string->id (cdr pair))
                             => (lambda (id) (proc sess id)))
                            (else (redirect (io sess) 'shelf)))))
                (else (redirect (io sess) 'shelf))))))))

  (define-syntax with-session/
    (syntax-rules ()
      ((_ (io request data) (sess (v key string->v default) ...) thunk)
       (with-session
        (io request)
        (lambda (sess)
          (let ((alist (and data (content->alist data))))
            (let ((v (cond ((and alist (assq 'key alist))
                            => (lambda (pair) (or (string->v (cdr pair)) default)))
                           (else default)))
                  ...)
              thunk)))))))

  (define-scenario (put-off io request data)
    (with-session&id
     (io request data)
     (lambda (sess id)
       (let ((c (form (io sess) (confirmation) base (__ are-you-sure-put-off-this-one?))))
         (cond ((yes? c)
                (if (destroy exlibris id)
                    (page (io sess) base (__ you-have-put-it-off))
                    (page (io sess) base (__ hmm-we-have-failed-to-put-off-your-exlibris))))
               (else
                (redirect (io sess) 'shelf)))))))

  (define-scenario (shelf io request data)
    (with-session/
     (io request data)
     (sess (p page string->page 0))
     (let ((id (account-id (user-account (session-user sess)))))
       (page (io sess) shelf (list id p)))))

  (define-scenario (desk io request data)
    (with-session&id
     (io request data)
     (lambda (sess id)
       (let ((ex (lookup exlibris id)))
         (if ex
             (page (io sess) desk id)
             (redirect (io sess) 'shelf))))))

  (define-scenario (modify-exlibris io request data)
    (with-session&id
     (io request data)
     (lambda (sess id)
       (let ((ex (lookup exlibris id)))
         (if ex
             (let ((r (lookup revision (exlibris-revision-id ex))))
               (let loop ((r-new (form (io sess) (revision r) base)))
                 (cond ((valid-revision? r-new)
                        (revision-bib-id-set! r-new (revision-bib-id r))
                        (if (and (save r-new)
                                 (begin
                                   (exlibris-revision-id-set! ex (revision-id r-new))
                                   (save ex)))
                            (page (io sess) desk id)
                            (page (io sess) base (__ hmm-an-error-occurred))))
                       (else
                        (loop (form (io sess) (revision r-new) base (__ please-retry)))))))
             (redirect (io sess) 'shelf))))))

  (define-scenario (share-exlibris io request data)
    (with-session&id
     (io request data)
     (lambda (sess id)
       (let ((ex (lookup exlibris id)))
         (if ex
             (if (save (make-publicity #f id))
                 (page (io sess) desk id)
                 (page (io sess) base (__ hmm-an-error-occurred)))
             (redirect (io sess) 'shelf))))))

  (define-scenario (hide-exlibris io request data)
    (with-session&id
     (io request data)
     (lambda (sess id)
       (let ((c (form (io sess) (confirmation) base (__ are-you-sure-hide-this-one?))))
         (if (yes? c)
             (if (destroy publicity id)
                 (page (io sess) desk id)
                 (page (io sess) base (__ hmm-an-error-occurred)))
             (redirect (io sess) 'shelf))))))

  (define (valid-review? r)
    (and (review? r)
         (cond ((review-body r)
                => (lambda (body)
                     (let ((length (string-length body)))
                       (< 0 length 1024))))
               (else #f))))

  (define-scenario (edit-review io request data)
    (with-session&id
     (io request data)
     (lambda (sess id)
       (let ((r (lookup review `((exlibris-id ,id)))))
         (let loop ((r-new (form (io sess) (review r) base)))
           (cond ((valid-review? r-new)
                  (if (review? r) (review-id-set! r-new (review-id r)))
                  (review-exlibris-id-set! r-new id)
                  (if (save r-new)
                      (page (io sess) desk id)
                      (page (io sess) base (__ hmm-an-error-occurred))))
                 (else
                  (loop (form (io sess) (review r-new) base (__ please-retry))))))))))

  (define-scenario (board io request)
    (cond ((logged-in? (parameter-of request))
           => (lambda (sess)
                (page (io sess) board "")))
          (else
           (page (io) board ""))))

  (define-scenario (new-report io request data)
    (with-session&id
     (io request data)
     (lambda (sess id)
       (let ((acc-id (account-id (user-account (session-user sess))))
             (r (lookup revision id)))
         (if r
             (let loop ((q (form (io sess) (quotation) base)))
               (cond ((valid-quotation? q)
                      (quotation-account-id-set! q acc-id)
                      (quotation-revision-id-set! q id)
                      (if (save q)
                          (let lp ((c (form (io sess) (correction) base)))
                            (cond ((valid-correction? c)
                                   (correction-account-id-set! c acc-id)
                                   (correction-quotation-id-set! c (quotation-id q))
                                   (if (save c)
                                       (let ((r (make-report #f acc-id id "Example" (quotation-id q) (correction-id c))))
                                         (if (save r)
                                             (page (io sess) base (__ reported))
                                             (page (io sess) base (__ hmm-an-error-occurred))))
                                       (page (io sess) base (__ hmm-an-error-occurred))))
                                  (else
                                   (lp (form (io sess) (correction c) base)))))
                          (page (io sess) base (__ hmm-an-error-occurred))))
                     (else
                      (loop (form (io sess) (quotation q) base)))))
             (redirect (io sess) 'shelf))))))

  (define (prepare-report-to-modify rep q c)
    (make-report-to-modify
     (report-subject rep)
     (quotation-page q)
     (quotation-position q)
     (quotation-body q)
     (correction-body c)))

  (define (update-quotation q modified)
    (make-quotation
     #f
     (quotation-account-id q)
     (quotation-revision-id q)
     (report-to-modify-page modified)
     (report-to-modify-position modified)
     (report-to-modify-quotation-body modified)))

  (define (update-correction c q modified)
    (make-correction
     #f
     (correction-account-id c)
     (quotation-id q)
     (report-to-modify-correction-body modified)))

  (define (update-report rep q c modified)
    (let ((q-new (update-quotation q modified)))
      (and (save q-new)
           (let ((c-new (update-correction c q-new modified)))
             (and (save c-new)
                  (let ((rep-new (make-report
                                  #f
                                  (report-account-id rep)
                                  (report-revision-id rep)
                                  (report-to-modify-subject modified)
                                  (quotation-id q-new)
                                  (correction-id c-new))))
                    (and (destroy rep)
                         (save rep-new)
                         rep-new)))))))

  (define-scenario (modify-report io request data)
    (with-session&id
     (io request data)
     (lambda (sess id)
       (let ((acc-id (account-id (user-account (session-user sess))))
             (rep (lookup report id)))
         (if rep
             (let ((q (lookup quotation (report-quotation-id rep)))
                   (c (lookup correction (report-correction-id rep))))
               (cond ((and (quotation? q)
                           (correction? c))
                      (let loop ((modified (form (io sess) (report-to-modify (prepare-report-to-modify rep q c)) base)))
                        (cond ((valid-report-to-modify? modified)
                               (if (update-report rep q c modified)
                                   (redirect (io sess) 'shelf)
                                   (page (io sess) base (__ hmm-an-error-occurred))))
                              (else
                               (loop (form (io sess) (report-to-modify modified) base (__ please-retry)))))))
                     (else
                      (redirect (io sess) 'shelf))))
             (redirect (io sess) 'shelf))))))

  (define-scenario (drop-report io request data)
    (with-session&id
     (io request data)
     (lambda (sess id)
       (let ((rep (lookup report id)))
         (if (and (report? rep)
                  (destroy rep))
             (redirect (io sess) 'shelf)
             (page (io sess) base (__ hmm-an-error-occurred)))))))

  (add-input-fields account (#f text text password text #f))
  (add-input-fields account-to-login (text password))
  (add-input-fields confirmation (text))
  (add-input-fields bib (#f text text #f #f))
  (add-input-fields revision (#f #f text text))
  (add-input-fields review (#f #f textarea))
  (add-input-fields quotation (#f #f #f text text textarea))
  (add-input-fields correction (#f #f #f textarea))
  (add-input-fields report (#f #f #f text #f #f))
  (add-input-fields report-to-modify (text text text textarea textarea))

  (templates "/home/tabe/errata/templates")
  (static-template "static")
  (template-environment (except (rnrs) div)
                        (lunula html)
                        (errata helper))

  (gettext

   ;; fields
   (account-nick (en "nick")
                 (ja "ニックネーム"))
   (account-name (en "name")
                 (ja "名前"))
   (account-password (en "password")
                     (ja "パスワード"))
   (account-mail-address (en "mail address")
                         (ja "メールアドレス"))
   (account-to-login-nick (en "nick")
                          (ja "ニックネーム"))
   (account-to-login-password (en "password")
                              (ja "パスワード"))
   (confirmation-ok (en "OK?")
                    (ja "OK?"))
   (bib-title (en "title")
              (ja "タイトル"))
   (bib-isbn13 (en "ISBN")
               (ja "ISBN"))
   (revision-name (en "name")
                  (ja "名前"))
   (revision-revised-at (en "revised at")
                        (ja "改訂日時"))

   ;; messages
   (hmm-an-error-occurred (en "Hmm ... an error occurred.")
                          (ja "残念ながら ... エラーが発生しました。"))
   (you-have-already-logged-in (en "You have already logged in!")
                               (ja "あなたは既にログインしています。"))
   (now-you-have-your-own-account (en "Now you have your own account!")
                                  (ja "あなたのアカウントができました。"))
   (hmm-we-have-failed-to-create-your-account (en "Hmm ... we have failed to create your account.")
                                              (ja "残念ながら ... あなたのアカウントの作成に失敗しました。"))
   (now-you-have-logged-in (en "Now you have logged in!")
                           (ja "ログインしました。"))
   (now-you-have-logged-out (en "Now you have logged out!")
                            (ja "ログアウトしました。"))
   (now-you-have-left-errata (en "Now you have left errata.fixedpoint.jp. Thanks for your favor.")
                             (ja "errata.fixedpoint.jp から解除されました。ご利用ありがとうございました。"))
   (please-input-title-or-isbn (en "Please input title or ISBN.")
                               (ja "タイトルまたは ISBN を入力してください。"))
   (is-this-content-ok? (en "Is this content OK?")
                        (ja "この内容でよろしいですか?"))
   (please-retry (en "Please retry!")
                 (ja "再度入力してください。"))
   (now-new-book-has-been-put-on (en "Now new book has been put on your bookshelf: ")
                                 (ja "新しい蔵書があなたの書棚に並びました: "))
   (hmm-we-have-failed-to-put-on-your-exlibris (en "Hmm ... we have failed to put on your exlibris.")
                                               (ja "残念ながら ... あなたの蔵書の追加に失敗しました。"))
   )

  (locale ja)

  )
