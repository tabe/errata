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
          (only (errata isbn) valid-isbn?))

  (define (blank? x)
    (or (not x)
        (and (string? x) (string-null? x))))

  (define (maybe-number id)
    (if (string? id) (string->number id) id))

  (define-record-type account-to-login
    (parent account)
    (fields nick password)
    (protocol
     (lambda (n)
       (lambda (nick password)
         (let ((p (n #f nick #f password #f "plain")))
           (p nick password))))))

  (define-record-type bib
    (fields (mutable id) (mutable title) isbn (mutable image))
    (protocol
     (lambda (p)
       (lambda (id title isbn image)
         (p (maybe-number id)
            title
            isbn
            image)))))

  (define-record-type revision
    (fields (mutable id) (mutable bib-id) (mutable name) revised-at)
    (protocol
     (lambda (p)
       (lambda (id bib-id name revised-at)
         (p (maybe-number id)
            (maybe-number bib-id)
            name
            revised-at)))))

  (define-record-type exlibris
    (fields (mutable id) account-id revision-id)
    (protocol
     (lambda (p)
       (lambda (id account-id revision-id)
         (p (maybe-number id)
            (maybe-number account-id)
            (maybe-number revision-id))))))

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

         (define (confirm-bib url title)
           (let ((c (form (io sess) (confirmation) base
                          (__ is-this-content-ok?)
                          (if (eof-object? url)
                              (bib-title b)
                              (tree->string (list (html:br)
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
               (isbn (bib-isbn b)))
           (cond ((and (blank? title)
                       (blank? isbn))
                  (specify-bib (form (io sess) (bib b) base (__ please-input-title-or-isbn))))
                 ((blank? isbn)
                  (confirm-bib (eof-object) (eof-object)))
                 ((valid-isbn? isbn)
                  (let ((url-and-title (query-image isbn)))
                    (call-with-port (open-string-input-port url-and-title)
                      (lambda (port)
                        (confirm-bib (get-line port) (get-line port))))))
                 (else
                  (specify-bib (form (io sess) (bib b) base (__ please-check-isbn)))))))

       (specify-bib (form (io sess) (bib) base)))))

  (define-scenario (put-off io request data)
    (with-session
     (io request)
     (lambda (sess)
       (cond ((and data (assq 'id (content->alist data)))
              => (lambda (pair)
                   (cond ((string->number (cdr pair))
                          => (lambda (id)
                               (let ((c (form (io sess) (confirmation) base (__ are-you-sure-put-off-this-one?))))
                                 (cond ((yes? c)
                                        (if (destroy exlibris id)
                                            (page (io sess) base (__ you-have-put-it-off))
                                            (page (io sess) base (__ hmm-we-have-failed-to-put-off-your-exlibris))))
                                       (else
                                        (redirect (io sess) 'shelf))))))
                         (else
                          (redirect (io sess) 'shelf)))))
             (else
              (redirect (io sess) 'shelf))))))

  (define-scenario (shelf io request)
    (with-session
     (io request)
     (lambda (sess)
       (let ((id (account-id (user-account (session-user sess)))))
         (page (io sess) shelf
               (tree->string
                (map (lambda (ex)
                       (let ((r (lookup revision (exlibris-revision-id ex))))
                         (if r
                             (let ((b (lookup bib (revision-bib-id r))))
                               (if r
                                   (html:div
                                    (html:image ((src (bib-image b))))
                                    (html:p (bib-title b))
                                    (html:p (bib-isbn b))
                                    (html:form ((action (build-entry-path 'put-off (session-uuid sess)))
                                                (method "POST"))
                                               (html:input ((type "hidden") (name "id") (value (exlibris-id ex))))
                                               (html:input ((type "submit") (value (__ put-off)))))
                                    )
                                   "??"))
                             "?")))
                     (lookup-all exlibris `((account-id ,id))))))))))

  (add-input-fields account (#f text text password text #f))
  (add-input-fields account-to-login (text password))
  (add-input-fields confirmation (text))
  (add-input-fields bib (#f text text #f))
  (add-input-fields revision (#f #f text text))

  (templates "/home/tabe/errata/templates")
  (static-template "static")
  ;;(template-environment (only (rnrs) list quote) (lunula html))

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
   (bib-isbn (en "ISBN")
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
