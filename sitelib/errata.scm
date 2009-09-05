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
          (only (core) format lookup-process-environment)
          (pregexp)
          (only (srfi :13) string-null?)
          (srfi :48)
          (lunula)
          (prefix (lunula html) html:)
          (lunula tree)
          (lunula gettext)
          (only (lunula persistent-record) string->id id-of id-set!)
          (lunula validation)
          (only (errata query) query-image)
          (only (errata isbn) valid-isbn?)
          (errata model))

  (define *password-min-length* 8)
  (define *nick-max-length* 16)
  (define *mail-address-max-length* 256)

  (define-validator (validate-password password)
    (password-too-short)
    (when (< (string-length password) *password-min-length*)
      (password-too-short)))

  (define-validator (validate-nick nick)
    (nick-invalid-char nick-too-long)
    (unless (pregexp-match "^[A-Za-z_][A-Za-z0-9_]*$" nick)
      (nick-invalid-char))
    (when (< *nick-max-length* (string-length nick))
      (nick-too-long)))

  (define-validator (validate-mail-address mail-address)
    (mail-address-too-long)
    (when (< *mail-address-max-length* (string-length mail-address))
      (mail-address-too-long)))

  (define-composite-validator validate-account
    (account-nick validate-nick)
    (account-password validate-password)
    (account-mail-address validate-mail-address))

  (define-validator (validate-new-nick nick)
    (nick-already-used)
    (when (lookup account `((nick ,nick)))
      (nick-already-used)))

  (define-composite-validator validate-new-account
    (account-nick validate-nick validate-new-nick)
    (account-password validate-password)
    (account-mail-address validate-mail-address))

  (define-validator (existing-account a)
    (does-not-exist)
    (or (lookup account `((nick ,(account-nick a))
                          (password ,(account-password a))))
        (does-not-exist)))

  (define-composite-validator authenticate-account
    (account-nick validate-nick)
    (account-password validate-password)
    (values existing-account))

  (define (blank? x)
    (or (not x)
        (and (string? x) (string-null? x))))

  (define (string->page str)
    (cond ((string->number str) => (lambda (page) (and (fixnum? page) (<= 0 page) page)))
          (else #f)))

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
              => (lambda (sess) (page (io sess) private (__ you-have-already-logged-in))))
             (else thunk)))))

  (define-syntax with-or-without-session
    (syntax-rules ()
      ((_ (io request) proc)
       (proc (logged-in? (parameter-of request))))))

  (define-scenario (sign-up io request)
    (without-session
     (io request)
     (let loop ((a (form (io) (account) public)))
       (guide (validate-new-account a)
         (lambda (ht) (loop (form (io) (account a) public (hashtable->messages ht))))
         (lambda _
           (if (save a)
               (page (io) public (__ now-you-have-your-own-account))
               (page (io) public (__ hmm-we-have-failed-to-create-your-account))))))))

  (define-scenario (modify-account io request)
    (with-session
     (io request)
     (lambda (sess)
       (let loop ((a (form (io sess) (account (user-account (session-user sess))) private)))
         (guide (validate-account a)
           (lambda (ht) (loop (form (io) (account a) public (hashtable->messages ht))))
           (lambda _
             (id-set! a (id-of (user-account (session-user sess))))
             (if (save a)
                 (page (io sess) private (__ your-account-has-been-updated))
                 (page (io sess) private (__ hmm-an-error-occurred)))))))))

  (define-scenario (cancel io request)
    (with-session
     (io request)
     (lambda (sess)
       (let ((c (form (io sess) (confirmation) private (__ are-you-sure?))))
         (cond ((yes? c)
                (if (destroy (user-account (session-user sess)))
                    (page (io) private (__ now-you-have-left-errata))
                    (page (io sess) private (__ hmm-an-error-occurred))))
               (else
                (page (io sess) private (__ of-course-we-know-you-are-kidding))))))))

  (define-scenario (login io request)
    (without-session
     (io request)
     (let loop ((a (form (io) (account-to-login) public)))
       (guide (authenticate-account a)
         (lambda _ (loop (form (io) (account-to-login a) public (__ please-retry))))
         (lambda (a)
           (let ((sess (do-login a)))
             (page (io sess) private (__ now-you-have-logged-in))))))))

  (define-scenario (logout io request)
    (with-session
     (io request)
     (lambda (sess)
       (do-logout sess)
       (page (io) public (__ now-you-have-logged-out)))))

  (define (valid-revision? rev)
    (and (revision? rev)
         (not (string-null? (revision-name rev)))))

  (define-scenario (put-on io request)
    (with-session
     (io request)
     (lambda (sess)

       (define (specify-bib b)

         (define (confirm-bib isbn13 isbn10 url title)
           (let ((c (form (io sess) (confirmation) private
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
                      (revision-bib-id-set! r (id-of b))
                      (if (save r)
                          (let ((ex (make-exlibris (id-of (user-account (session-user sess)))
                                                   (id-of r))))
                            (if (save ex)
                                (page (io sess) private (__ you-are-done))
                                (page (io sess) private (__ hmm-we-have-failed-to-put-on-your-exlibris))))
                          (page (io sess) private (__ hmm-we-have-failed-to-put-on-your-exlibris))))
                     (else
                      (specify-revision (form (io sess) (revision r) private (__ please-retry))))))

             (cond ((yes? c)
                    (unless (eof-object? isbn13) (bib-isbn13-set! b isbn13))
                    (unless (eof-object? isbn10) (bib-isbn10-set! b isbn10))
                    (unless (eof-object? title) (bib-title-set! b title))
                    (unless (eof-object? url) (bib-image-set! b url))
                    (cond ((save b)
                           (specify-revision
                            (form (io sess) (revision) private (__ now-new-book-has-been-put-on) (bib-title b))))
                          (else
                           (page (io sess) private (__ hmm-we-have-failed-to-put-on-your-exlibris)))))
                   (else
                    (specify-bib (form (io sess) (bib b) private (__ please-retry)))))))

         (let ((title (bib-title b))
               (isbn (bib-isbn13 b)))
           (cond ((and (blank? title)
                       (blank? isbn))
                  (specify-bib (form (io sess) (bib b) private (__ please-input-title-or-isbn))))
                 ((blank? isbn)
                  (confirm-bib (eof-object) (eof-object) (eof-object) (eof-object)))
                 ((valid-isbn? isbn)
                  (call/cc
                   (lambda (cont)
                     (let ((info (guard (e
                                         ((i/o-error? e)
                                          (write e)
                                          (newline)
                                          (cont (specify-bib (form (io sess) (bib b) private (__ hmm-an-error-occurred))))))
                                   (query-image isbn))))
                       (cond ((eof-object? info)
                              (specify-bib (form (io sess) (bib b) private (__ please-check-isbn))))
                             (else
                              (call-with-port (open-string-input-port info)
                                (lambda (port)
                                  (let* ((isbn13 (get-line port))
                                         (isbn10 (get-line port))
                                         (url    (get-line port))
                                         (title  (get-line port)))
                                    (confirm-bib isbn13 isbn10 url title))))))))))
                 (else
                  (specify-bib (form (io sess) (bib b) private (__ please-check-isbn)))))))

       (specify-bib (form (io sess) (bib) private)))))

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

  (define-syntax with-or-without-session/
    (syntax-rules ()
      ((_ (io request data) (sess (v key string->v default) ...) thunk)
       (with-or-without-session
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
       (let ((c (form (io sess) (confirmation) private (__ are-you-sure-put-off-this-one?))))
         (cond ((yes? c)
                (if (destroy exlibris id)
                    (page (io sess) private (__ you-have-put-it-off))
                    (page (io sess) private (__ hmm-we-have-failed-to-put-off-your-exlibris))))
               (else
                (redirect (io sess) 'shelf)))))))

  (define-scenario (shelf io request data)
    (with-session/
     (io request data)
     (sess (p page string->page 0))
     (let ((id (id-of (user-account (session-user sess)))))
       (page (io sess) shelf (list id p)))))

  (define-scenario (desk io request data)
    (with-session&id
     (io request data)
     (lambda (sess id)
       (let ((ex (lookup exlibris id)))
         (if ex
             (page (io sess) desk id)
             (redirect (io sess) 'shelf))))))

  (define-scenario (modify-revision io request data)
    (with-session&id
     (io request data)
     (lambda (sess id)
       (let ((ex (lookup exlibris id)))
         (if ex
             (let ((r (lookup revision (exlibris-revision-id ex))))
               (let loop ((r-new (form (io sess) (revision r) private)))
                 (cond ((valid-revision? r-new)
                        (revision-bib-id-set! r-new (revision-bib-id r))
                        (if (and (save r-new)
                                 (begin
                                   (exlibris-revision-id-set! ex (id-of r-new))
                                   (save ex)))
                            (page (io sess) desk id)
                            (page (io sess) private (__ hmm-an-error-occurred))))
                       (else
                        (loop (form (io sess) (revision r-new) private (__ please-retry)))))))
             (redirect (io sess) 'shelf))))))

  (define-scenario (share-exlibris io request data)
    (with-session&id
     (io request data)
     (lambda (sess id)
       (let ((ex (lookup exlibris id)))
         (if ex
             (if (save (make-publicity id))
                 (page (io sess) desk id)
                 (page (io sess) private (__ hmm-an-error-occurred)))
             (redirect (io sess) 'shelf))))))

  (define-scenario (hide-exlibris io request data)
    (with-session&id
     (io request data)
     (lambda (sess id)
       (let ((c (form (io sess) (confirmation) private (__ are-you-sure-hide-this-one?))))
         (if (yes? c)
             (if (destroy publicity id)
                 (page (io sess) desk id)
                 (page (io sess) private (__ hmm-an-error-occurred)))
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
         (let loop ((r-new (form (io sess) (review r) private)))
           (cond ((valid-review? r-new)
                  (if (review? r) (id-set! r-new (id-of r)))
                  (review-exlibris-id-set! r-new id)
                  (if (save r-new)
                      (page (io sess) desk id)
                      (page (io sess) private (__ hmm-an-error-occurred))))
                 (else
                  (loop (form (io sess) (review r-new) private (__ please-retry))))))))))

  (define-scenario (board io request data)
    (with-or-without-session/
     (io request data)
     (sess (p page string->page 0))
     (page (io sess) board p)))

  (define-scenario (table io request data)
    (with-or-without-session/
     (io request data)
     (sess (id id string->id #f))
     (if id
         (page (io sess) table id)
         (redirect (io sess) 'board))))

  (define-scenario (detail io request data)
    (with-or-without-session/
     (io request data)
     (sess (id id string->id #f))
     (if id
         (page (io sess) detail id)
         (redirect (io sess) 'board))))

  (define (report-to-modify->quotation rep a-id r-id)
    (make-quotation a-id
                    r-id
                    (report-to-modify-page rep)
                    (report-to-modify-position rep)
                    (report-to-modify-quotation-body rep)))

  (define (report-to-modify->correction rep a-id q-id)
    (make-correction a-id
                     q-id
                     (report-to-modify-correction-body rep)))

  (define (report-to-modify->report rep a-id r-id q-id c-id)
    (make-report a-id
                 r-id
                 (report-to-modify-subject rep)
                 q-id
                 c-id))

  (define-scenario (new-report io request data)
    (with-session&id
     (io request data)
     (lambda (sess id)
       (let ((a-id (id-of (user-account (session-user sess))))
             (ex (lookup exlibris id)))
         (if ex
             (let loop ((rep (form (io sess) (report-to-modify) private)))
               (cond ((valid-report-to-modify? rep)
                      (let ((q (report-to-modify->quotation rep a-id (exlibris-revision-id ex))))
                        (if (save q)
                            (let ((c (report-to-modify->correction rep a-id (id-of q))))
                              (if (save c)
                                  (let ((r (report-to-modify->report rep a-id (exlibris-revision-id ex) (id-of q) (id-of c))))
                                    (if (save r)
                                        (page (io sess) desk id)
                                        (page (io sess) private (__ hmm-an-error-occurred))))
                                  (page (io sess) private (__ hmm-an-error-occurred))))
                            (page (io sess) private (__ hmm-an-error-occurred)))))
                     (else
                      (loop (form (io sess) (report-to-modify rep) private (__ please-retry))))))
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
     (quotation-account-id q)
     (quotation-revision-id q)
     (report-to-modify-page modified)
     (report-to-modify-position modified)
     (report-to-modify-quotation-body modified)))

  (define (update-correction c q modified)
    (make-correction
     (correction-account-id c)
     (id-of q)
     (report-to-modify-correction-body modified)))

  (define (update-report rep q c modified)
    (let ((q-new (update-quotation q modified)))
      (and (save q-new)
           (let ((c-new (update-correction c q-new modified)))
             (and (save c-new)
                  (let ((rep-new (make-report
                                  (report-account-id rep)
                                  (report-revision-id rep)
                                  (report-to-modify-subject modified)
                                  (id-of q-new)
                                  (id-of c-new))))
                    (and (destroy rep)
                         (save rep-new)
                         rep-new)))))))

  (define-scenario (modify-report io request data)
    (with-session/
     (io request data)
     (sess (rep-id report string->id #f)
           (ex-id exlibris string->id #f))
     (let ((rep (lookup report rep-id)))
       (if rep
           (let ((q (lookup quotation (report-quotation-id rep)))
                 (c (lookup correction (report-correction-id rep))))
             (cond ((and (quotation? q)
                         (correction? c))
                    (let loop ((modified (form (io sess) (report-to-modify (prepare-report-to-modify rep q c)) private)))
                      (cond ((valid-report-to-modify? modified)
                             (if (update-report rep q c modified)
                                 (page (io sess) desk ex-id)
                                 (page (io sess) private (__ hmm-an-error-occurred))))
                            (else
                             (loop (form (io sess) (report-to-modify modified) private (__ please-retry)))))))
                   (else
                    (redirect (io sess) 'shelf))))
           (redirect (io sess) 'shelf)))))

  (define-scenario (drop-report io request data)
    (with-session/
     (io request data)
     (sess (rep-id report string->id #f)
           (ex-id exlibris string->id #f))
     (if (and rep-id ex-id)
         (let ((rep (lookup report rep-id)))
           (and (report? rep)
                (let ((c (form (io sess) (confirmation) private (__ are-you-sure-drop-report?))))
                  (yes? c))
                (destroy rep))
           (page (io sess) desk ex-id))
         (redirect (io sess) 'shelf))))

  (define-scenario (acknowledge io request data)
    (with-session/
     (io request data)
     (sess (r-id report string->id #f)
           (q-id quotation string->id #f))
     (if q-id
         (let ((q (lookup quotation q-id)))
           (if (quotation? q)
               (let loop ((a (form (io sess) (acknowledgement) private)))
                 (cond ((valid-acknowledgement? a)
                        (acknowledgement-account-id-set! a (id-of (user-account (session-user sess))))
                        (acknowledgement-quotation-id-set! a q-id)
                        (if (save a)
                            (page (io sess) detail r-id)
                            (page (io sess) private (__ hmm-an-error-occurred))))
                       (else (loop (form (io sess) (acknowledgement a) private (__ please-retry))))))
               (redirect (io sess) 'table)))
         (redirect (io sess) 'table))))

  (define-scenario (agree io request data)
    (with-session/
     (io request data)
     (sess (r-id report string->id #f)
           (c-id correction string->id #f))
     (if c-id
         (cond ((lookup correction c-id)
                => (lambda (c)
                     (let loop ((a (form (io sess) (agreement) private)))
                       (cond ((valid-agreement? a)
                              (agreement-account-id-set! a (id-of (user-account (session-user sess))))
                              (agreement-correction-id-set! a c-id)
                              (if (save a)
                                  (page (io sess) detail r-id)
                                  (page (io sess) private (__ hmm-an-error-occurred))))
                             (else (loop (form (io sess) (agreement a) private (__ please-retry))))))))
               (else (redirect (io sess) 'board)))
         (redirect (io sess) 'board))))

  ;; input fields
  (add-input-fields account (text text password text #f))
  (add-input-fields account-to-login (text password))
  (add-input-fields confirmation (text))
  (add-input-fields bib (text text #f #f))
  (add-input-fields revision (#f text text))
  (add-input-fields review (#f textarea))
  (add-input-fields quotation (#f #f text text textarea))
  (add-input-fields correction (#f #f textarea))
  (add-input-fields report (#f #f text #f #f #f))
  (add-input-fields report-to-modify (text text text textarea textarea))
  (add-input-fields acknowledgement (#f #f text textarea))
  (add-input-fields agreement (#f #f textarea))

  ;; templates
  (templates (string-append (lookup-process-environment "PWD") "/templates"))
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
   (review-body (en "Body")
                (ja "レビュー本文"))
   (quotation-page (en "Page")
                   (ja "ページ"))
   (quotation-position (en "Position")
                       (ja "位置"))
   (quotation-body (en "Quotation's Body")
                   (ja "引用本文"))
   (correction-body (en "Correction's Body")
                    (ja "訂正本文"))
   (report-to-modify-subject (en "Subject")
                             (ja "題名"))
   (report-to-modify-page (en "Page")
                          (ja "ページ"))
   (report-to-modify-position (en "Position")
                              (ja "位置"))
   (report-to-modify-quotation-body (en "Quotation's Body")
                                    (ja "引用本文"))
   (report-to-modify-correction-body (en "Correction's Body")
                                     (ja "訂正本文"))
   (acknowledgement-sign (en "Sign")
                         (ja "賛成/反対"))
   (acknowledgement-comment (en "Comment")
                            (ja "コメント"))

   ;; messages
   (password-too-short (en "password is too short.")
                       (ja "パスワードが短いです。"))
   (nick-already-used (en "nick is already used.")
                      (ja "ニックネームは既に使用されています。"))
   (nick-invalid-char (en "nick contains invalid characters.")
                      (ja "ニックネームに使えない文字が含まれています。"))
   (nick-too-long (en "nick is too long.")
                  (ja (format "ニックネームが規定の長さ(~d)を超えています。" *nick-max-length*)))
   (mail-address-too-long (en "mail address is too long.")
                          (ja (format "メールアドレスが規定の長さ(~d)を超えています。" *mail-address-max-length*)))
   (submit (en "submit")
           (ja "送信"))
   (hmm-an-error-occurred (en "Hmm ... an error occurred.")
                          (ja "残念ながら ... エラーが発生しました。"))
   (you-have-already-logged-in (en "You have already logged in!")
                               (ja "既にログインしています。"))
   (now-you-have-your-own-account (en "Now you have your own account!")
                                  (ja "アカウントができました。"))
   (hmm-we-have-failed-to-create-your-account (en "Hmm ... we have failed to create your account.")
                                              (ja "残念ながら ... アカウントの作成に失敗しました。"))
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
   (please-retry (en "Please check your input and retry.")
                 (ja "入力内容を確認して再度入力してください。"))
   (now-new-book-has-been-put-on (en "Now new book has been put on your bookshelf: ")
                                 (ja "新しい蔵書があなたの書棚に並びました: "))
   (hmm-we-have-failed-to-put-on-your-exlibris (en "Hmm ... we have failed to put on your exlibris.")
                                               (ja "残念ながら ... 蔵書の追加に失敗しました。"))

   (ISBN (en "ISBN")
         (ja "ISBN"))
   (Revision (en "Revision")
             (ja "改訂情報"))
   (Review (en "Review")
           (ja "レビュー"))
   (Table (en "Table")
          (ja "正誤表"))
   (Detail (en "Detail")
           (ja "詳細"))
   (Quotation (en "Quotation")
              (ja "誤(引用)"))
   (Correction (en "Correction")
               (ja "正(訂正)"))
   (to-board (en "To Board")
             (ja "一覧へ"))
   (to-desk (en "To Desk")
            (ja "デスクへ"))
   (to-detail (en "To Detail")
              (ja "詳細へ"))
   (to-shelf (en "To Shelf")
             (ja "一覧へ"))
   (to-table (en "To Table")
             (ja "正誤表へ"))
   (share-exlibris (en "Share Exlibris")
                   (ja "共有する"))
   (hide-exlibris (en "Hide Exlibris")
                  (ja "隠す"))
   (put-off (en "Put Off")
            (ja "外す"))
   (edit-review (en "Edit")
                (ja "編集"))
   (new-report (en "New Report")
               (ja "新たに報告"))
   (modify-revision (en "Modify Revision")
                    (ja "修正"))
   (modify-report (en "Modify Report")
                  (ja "報告を修正"))
   (drop-report (en "Drop Report")
                (ja "報告を削除"))
   (acknowledge (en "Acknowledge")
                (ja "引用に関してコメント"))
   (agree (en "Agree")
          (ja "訂正に同意"))
   (disagree (en "Disagree")
             (ja "訂正に反対"))
   )

  (locale ja)

  )
