(library (errata)
  (export start
          connect
          close)
  (import (rnrs)
          (only (core) format)
          (match)
          (pregexp)
          (only (srfi :13) string-null?)
          (srfi :48)
          (lunula)
          (lunula gettext)
          (prefix (lunula html) html:)
          (prefix (lunula log) log:)
          (lunula session)
          (only (lunula string) blank?)
          (lunula tree)
          (only (lunula persistent-record) string->id id-of id-set!)
          (lunula validation)
          (only (errata query) query-image)
          (only (errata isbn) valid-isbn?)
          (errata message)
          (errata model)
          (prefix (only (errata rss) query) rss:)
          (errata validator))

  (define *domain* "errata.fixedpoint.jp")

  (define *mail-from* "errata@fixedpoint.jp")

  (define (mail-subject subject)
    (string-append "Errata - " subject))

  (define (mail-body . body)
    (tree->string `(,@body "\n-- Errata\n")))

  (define (string->page str)
    (cond ((string->number str) => (lambda (page) (and (fixnum? page) (<= 0 page) page)))
          (else #f)))

  (define (session->account-id sess)
    (id-of (user-account (session-user sess))))

  (define (yes? c) (and (confirmation? c) (string? (ok? c)) (pregexp-match "^[yY]" (ok? c))))

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

  (define-syntax define-simple-scenario
    (syntax-rules ()
      ((_ name)
       (define-scenario (name io request)
         (with-or-without-session
          (io request)
          (lambda (sess) (page (io sess) name)))))))

  (define-simple-scenario index)
  (define-simple-scenario faq)
  (define-simple-scenario terms-of-service)
  (define-simple-scenario privacy-policy)
  (define-simple-scenario feedback)

  (define (sign-up-confirmation a)
    (lambda (path)
      (values
       (account-mail-address a)
       *mail-from*
       (mail-subject (format "Confirmation for New Account '~a'" (account-nick a)))
       (mail-body
        (account-nick a) " 様\n"
        "\n"
        "アカウント申請ありがとうございます。\n"
        "下記の URL にアクセスしていただいくとアカウントの作成が完了します。\n"
        "(このメールは安全に無視できます; 何もしなければアカウントは作成はされません。)\n"
        (format "http://~a~a~%" *domain* path)))))

  (define (sign-up-summary new-a)
    (tree->string
     (html:dl
      (html:dt (__ new-account-nick))
      (html:dd (new-account-nick new-a))
      (html:dt (__ new-account-name))
      (html:dd (new-account-name new-a))
      (html:dt (__ new-account-mail-address))
      (html:dd (new-account-mail-address new-a)))))

  (define-scenario (sign-up io request)
    (without-session
     (io request)
     (let loop ((new-a (form (io) (new-account) public (__ you-can-create-your-own-account))))
       (if (new-account? new-a)
           (guide (validate-new-account new-a)
             (lambda (ht) (loop (form (io) (new-account new-a) public (hashtable->messages ht))))
             (lambda _
               (let ((c (form (io) (confirmation) public (__ is-this-content-ok?) (sign-up-summary new-a))))
                 (if (yes? c)
                     (let ((a (new-account->account new-a)))
                       (and (mail (io) public (__ we-have-sent-confirmation-message-to-you) (sign-up-confirmation a))
                            (if (save a)
                                (page (io) public (__ now-you-have-your-own-account))
                                (page (io) public (__ hmm-an-error-occurred)))))
                     (loop (form (io) (new-account new-a) public (__ please-retry)))))))
           (page (io) index)))))

  (define (password-reset-request a)
    (lambda (path)
      (values
       (account-mail-address a)
       *mail-from*
       (mail-subject "Reset Password")
       (mail-body
        (account-nick a) " 様\n"
        "\n"
        "パスワードのリセットを行います。\n"
        "下記の URL にアクセスして新しいパスワードを指定してください。\n"
        "(このメールは安全に無視できます; 何もしなければパスワードはそのままです。)\n"
        (format "http://~a~a~%" *domain* path)))))

  (define-scenario (forgot-password io request)
    (without-session
     (io request)
     (let loop ((a (form (io) (forgotten-account) public (__ let-me-know-your-mail-address))))
       (if (forgotten-account? a)
           (guide (existing-mail-address a)
             (lambda _ (page (io) public (__ we-have-sent-message-to-you)))
             (lambda (a)
               (and (mail (io) public (__ we-have-sent-message-to-you) (password-reset-request a))
                    (let loop ((p (form (io) (password-reset) public (__ specify-new-password))))
                      (guide (validate-password-reset p)
                        (lambda _ (loop (form (io) (password-reset p) public (__ please-retry))))
                        (lambda _
                          (let ((a (password-reset->account p a)))
                            (if (save a)
                                (page (io) public (__ your-password-updated))
                                (page (io) public (__ hmm-an-error-occurred))))))))))
           (page (io) index)))))

  (define-scenario (modify-account io request)
    (with-session
     (io request)
     (lambda (sess)
       (let ((current-account (user-account (session-user sess))))
         (let loop ((a (form (io sess) (account-to-modify (account->account-to-modify current-account)) private)))
           (if (account-to-modify? a)
               (guide (validate-account-to-modify a current-account)
                 (lambda (ht) (loop (form (io sess) (account-to-modify a) private (hashtable->messages ht))))
                 (lambda _
                   (let ((a (account-to-modify->account a current-account)))
                     (id-set! a (id-of current-account))
                     (cond ((save a)
                            (do-logout sess)
                            (let ((sess (do-login a)))
                              (page (io sess) private (__ your-account-has-been-updated))))
                           (else
                            (page (io sess) private (__ hmm-an-error-occurred)))))))
               (page (io sess) index)))))))

  (define-scenario (cancel io request)
    (with-session
     (io request)
     (lambda (sess)
       (let ((c (form (io sess) (confirmation) private (__ are-you-sure-to-cancel-account?))))
         (cond ((yes? c)
                (if (destroy (user-account (session-user sess)))
                    (page (io) private (__ now-you-have-left-errata))
                    (page (io sess) private (__ hmm-an-error-occurred))))
               (else
                (page (io sess) private (__ of-course-we-know-you-are-kidding))))))))

  (define-scenario (login io request)
    (without-session
     (io request)
     (let loop ((a (form (io) (account-to-login) login)))
       (if (account-to-login? a)
           (guide (authenticate-account a)
             (lambda _ (loop (form (io) (account-to-login a) login (__ please-retry))))
             (lambda (a)
               (let ((sess (do-login a)))
                 (page (io sess) private (__ now-you-have-logged-in)))))
           (page (io) index)))))

  (define-scenario (logout io request)
    (with-session
     (io request)
     (lambda (sess)
       (do-logout sess)
       (page (io) public (__ now-you-have-logged-out)))))

  (define (preference->preference-to-edit pref)
    (make-preference-to-edit (preference-report-format pref)))

  (define-scenario (edit-preference io request)
    (with-session
     (io request)
     (lambda (sess)
       (let* ((a-id (session->account-id sess))
              (pref (lookup preference `((account-id ,a-id)))))
         (let loop ((e (form (io sess) (preference-to-edit (and (preference? pref) (preference->preference-to-edit pref))) private)))
           (cond ((preference-to-edit? e)
                  (guide (validate-preference-to-edit e)
                    (lambda (ht) (loop (form (io sess) (preference-to-edit e) private (hashtable->messages ht))))
                    (lambda _
                      (let ((p (make-preference (if (preference? pref) #f a-id) (preference-to-edit-report-format e))))
                        (when (preference? pref) (id-set! p (id-of pref)))
                        (if (save p)
                            (page (io sess) private (__ your-preference-has-been-updated))
                            (page (io sess) private (__ hmm-an-error-occurred)))))))
                 (else (redirect (io sess) 'shelf))))))))

  (define-scenario (find-bib io request data)
    (with-or-without-session/
     (io request data)
     (sess (isbn isbn values #f))
     (cond ((and (string? isbn) (valid-isbn? isbn))
            => (lambda (n)
                 (cond ((case n
                          ((10) (lookup (publicity
                                         (exlibris publicity)
                                         (account exlibris)
                                         (revision exlibris)
                                         (bib revision))
                                        ((bib (isbn10 isbn)))))
                          (else (lookup (publicity
                                         (exlibris publicity)
                                         (account exlibris)
                                         (revision exlibris)
                                         (bib revision))
                                        ((bib (isbn13 isbn))))))
                        => (lambda (tuple)
                             (page (io sess) find-bib (id-of (list-ref tuple 4)))))
                       (else (page (io sess) public (__ bib-not-found))))))
           (else (page (io sess) public (__ please-retry))))))

  (define (existing-revisions bib-id . last)
    (let ((revisions (lookup-all revision `((bib-id ,bib-id)))))
      (cond ((null? revisions) '())
            ((null? last) revisions)
            (else (remp (lambda (r) (= (car last) (id-of r))) revisions)))))

  (define (show-existing-revisions bib-id . last)
    (lambda (path)
      (let ((revisions (apply existing-revisions bib-id last)))
        (if (null? revisions)
            '()
            (append
             (html:p (__ choose-a-revision))
             (map
              (lambda (r)
                (let ((name (html:escape-string (revision-name r)))
                      (revised-at (html:escape-string (revision-revised-at r))))
                  (html:p
                   (html:form ((action path))
                              name "(" revised-at ")"
                              (html:input ((type "hidden") (name "name") (value name)))
                              (html:input ((type "hidden") (name "revised-at") (value revised-at)))
                              (html:input ((type "submit") (name "submit") (value (__ submit))))))))
              revisions)
             (html:p (__ or-specify-revision)))))))

  (define-scenario (put-on io request)
    (with-session
     (io request)
     (lambda (sess)

       (define (specify-bib new-ex)

         (define (confirm-bib b)

             (define (specify-revision r)

               (define (save-exlibris r)
                 (let ((a-id (session->account-id sess))
                       (done (lambda (ex) (page (io sess) desk (id-of ex)))))
                   (cond ((lookup exlibris `((account-id ,a-id)
                                             (revision-id ,(id-of r))))
                          => done)
                         (else
                          (let ((ex (make-exlibris a-id (id-of r) 0)))
                            (if (and (execute "BEGIN")
                                     (execute "UPDATE exlibris SET position = position + 1")
                                     (save ex)
                                     (execute "COMMIT"))
                                (done ex)
                                (page (io sess) private (__ hmm-an-error-occurred))))))))

               (cond ((revision? r)
                      (guide (validate-revision r)
                        (lambda (ht) (specify-revision (form (io sess) (revision r) private (hashtable->messages ht))))
                        (lambda _
                          (cond ((lookup revision `((bib-id ,(id-of b))
                                                    (name ,(revision-name r))
                                                    (revised-at ,(revision-revised-at r))))
                                 => save-exlibris)
                                ((begin (revision-bib-id-set! r (id-of b)) (save r))
                                 (save-exlibris r))
                                (else (page (io sess) private (__ hmm-an-error-occurred)))))))
                     (else (redirect (io sess) 'shelf))))

             (if (yes? (form (io sess) (confirmation) private
                             (__ is-this-content-ok?)
                             (tree->string
                              (cons* (html:br)
                                     (bib-title b)
                                     (cond ((bib-image b)
                                            => (lambda (url)
                                                 (append (html:br) (html:img ((src url))))))
                                           (else '()))))))
                 (cond ((id-of b)
                        => (lambda (id)
                             (specify-revision (form (io sess) (revision) private (show-existing-revisions id)))))
                       ((save b)
                        (specify-revision
                         (form (io sess) (revision) private (__ specify-revision) (bib-title b))))
                       (else
                        (page (io sess) private (__ hmm-an-error-occurred))))
                 (specify-bib (form (io sess) (new-exlibris new-ex) private (__ please-retry)))))

         (if (new-exlibris? new-ex)
             (let ((title (new-exlibris-title new-ex))
                   (isbn (new-exlibris-isbn new-ex)))
               (cond ((and (blank? title) (blank? isbn))
                      (specify-bib (form (io sess) (new-exlibris new-ex) private (__ please-input-title-or-isbn))))
                     ((blank? isbn)
                      (guide (validate-bib-title title)
                        (lambda (ht) (specify-bib (form (io sess) (new-exlibris new-ex) private (hashtable->messages ht))))
                        (lambda _ (confirm-bib (make-bib title #f #f #f)))))
                     ((valid-isbn? isbn)
                      => (lambda (n)
                           (let ((b (case n
                                      ((10) (lookup bib `((isbn10 ,isbn))))
                                      (else (lookup bib `((isbn13 ,isbn)))))))
                             (if (bib? b)
                                 (confirm-bib b)
                                 (call/cc
                                  (lambda (cont)
                                    (let ((info (guard (e
                                                        ((i/o-error? e)
                                                         (log:info "errata> ~s" e)
                                                         (cont (specify-bib (form (io sess) (new-exlibris new-ex) private (__ hmm-an-error-occurred))))))
                                                  (query-image isbn))))
                                      (if (eof-object? info)
                                          (specify-bib (form (io sess) (new-exlibris new-ex) private (__ please-check-isbn)))
                                          (call-with-port (open-string-input-port info)
                                            (lambda (port)
                                              (let* ((isbn13 (get-line port))
                                                     (isbn10 (get-line port))
                                                     (url    (get-line port))
                                                     (title  (get-line port)))
                                                (confirm-bib (make-bib (and (not (eof-object? title)) title)
                                                                       (and (not (eof-object? isbn13)) isbn13)
                                                                       (and (not (eof-object? isbn10)) isbn10)
                                                                       (and (not (eof-object? url)) url))))))))))))))
                     (else
                      (specify-bib (form (io sess) (new-exlibris new-ex) private (__ please-check-isbn))))))
             (redirect (io sess) 'shelf)))

       (specify-bib (form (io sess) (new-exlibris) private)))))

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
       (let ((c (form (io sess) (confirmation) private (__ are-you-sure-to-put-off-this-one?))))
         (cond ((yes? c)
                (cond ((lookup exlibris id)
                       => (lambda (ex)
                            (cond ((and (= (exlibris-account-id ex) (session->account-id sess)) ; security
                                        (destroy ex))
                                   (execute (format "UPDATE exlibris SET position = position - 1 WHERE position > '~d'" (exlibris-position ex)))
                                   (delete-orphan-revisions)
                                   (page (io sess) private (__ you-have-put-it-off)))
                                  (else
                                   (page (io sess) private (__ hmm-an-error-occurred))))))
                      (else (page (io sess) private (__ you-have-put-it-off)))))
               (else (page (io sess) desk id)))))))

  (define-scenario (shelf io request data)
    (with-session/
     (io request data)
     (sess (p page string->page 0))
     (let ((id (session->account-id sess)))
       (page (io sess) shelf (list id p)))))

  (define-scenario (desk io request data)
    (with-session&id
     (io request data)
     (lambda (sess id)
       (cond ((lookup exlibris id)
              => (lambda (ex)
                   (if (= (exlibris-account-id ex) (session->account-id sess)) ; security
                       (page (io sess) desk id)
                       (page (io sess) private (__ hmm-an-error-occurred)))))
             (else (redirect (io sess) 'shelf))))))

  (define-scenario (put-at-top io request data)
    (with-session&id
     (io request data)
     (lambda (sess id)
       (cond ((lookup exlibris id)
              => (lambda (ex)
                   (if (and (= (exlibris-account-id ex) (session->account-id sess)) ; security
                            (for-all
                             execute
                             `("BEGIN"
                               ,(format "UPDATE exlibris SET position = position + 1 WHERE position <= '~d'" (exlibris-position ex))
                               ,(format "UPDATE exlibris SET position = 0 WHERE id = '~d'" (id-of ex))
                               "COMMIT")))
                       (redirect (io sess) 'shelf)
                       (page (io sess) private (__ hmm-an-error-occurred)))))
             (else (redirect (io sess) 'shelf))))))

  (define (rewrite-revision-id account-id old-id new-id)
    (execute "BEGIN")
    (for-each
     (lambda (table)
       (let ((template "UPDATE ~a SET revision_id = '~d' WHERE account_id = '~d' AND revision_id = '~d'"))
         (execute (format template table new-id account-id old-id))))
     '("quotation" "report" "report_history"))
    (execute "COMMIT"))

  (define (delete-orphan-revisions)
    (execute "DELETE FROM revision WHERE NOT EXISTS (SELECT 1 FROM exlibris e WHERE revision.id = e.revision_id)"))

  (define-scenario (modify-revision io request data)
    (with-session&id
     (io request data)
     (lambda (sess id)
       (cond ((lookup (exlibris (revision exlibris))
                      ((exlibris
                        (account-id (session->account-id sess)) ; security
                        (id id))))
              => (lambda (tuple)
                   (match tuple
                     ((ex r)
                      (let loop ((r-new (form (io sess) (revision r) private (show-existing-revisions (revision-bib-id r) (id-of r)))))
                        (if (revision? r-new)
                            (guide (validate-revision r-new)
                              (lambda (ht) (loop (form (io sess) (revision r-new) private (hashtable->messages ht))))
                              (lambda _
                                (cond ((lookup revision `((bib-id ,(revision-bib-id r))
                                                          (name ,(revision-name r-new))
                                                          (revised-at ,(revision-revised-at r-new))))
                                       => (lambda (r-cur)
                                            (cond ((= (id-of r) (id-of r-cur)) ; nothing changed
                                                   (page (io sess) desk id))
                                                  ((lookup exlibris `((account-id ,(exlibris-account-id ex))
                                                                      (revision-id ,(id-of r-cur))))
                                                   => (lambda (ex-cur) ; it already exists
                                                        (page (io sess) desk (id-of ex-cur))))
                                                  ((begin
                                                     (exlibris-revision-id-set! ex (id-of r-cur))
                                                     (save ex))
                                                   (rewrite-revision-id (exlibris-account-id ex) (id-of r) (id-of r-cur))
                                                   (delete-orphan-revisions)
                                                   (page (io sess) desk id))
                                                  (else
                                                   (page (io sess) private (__ hmm-an-error-occurred))))))
                                      ((begin
                                         (revision-bib-id-set! r-new (revision-bib-id r))
                                         (and (save r-new)
                                              (begin
                                                (exlibris-revision-id-set! ex (id-of r-new))
                                                (save ex))))
                                       (rewrite-revision-id (exlibris-account-id ex) (id-of r) (id-of r-new))
                                       (delete-orphan-revisions)
                                       (page (io sess) desk id))
                                      (else (page (io sess) private (__ hmm-an-error-occurred))))))
                            (page (io sess) desk id))))
                     (_ (redirect (io sess) 'shelf)))))
             (else (redirect (io sess) 'shelf))))))

  (define-scenario (share-exlibris io request data)
    (with-session&id
     (io request data)
     (lambda (sess id)
       (cond ((lookup exlibris id)
              => (lambda (ex)
                   (cond ((and (= (exlibris-account-id ex) (session->account-id sess)) ; security
                               (save (make-publicity id)))
                          (rss:query "recent-public-revisions")
                          (page (io sess) desk id))
                         (else (page (io sess) private (__ hmm-an-error-occurred))))))
             (else (redirect (io sess) 'shelf))))))

  (define-scenario (hide-exlibris io request data)
    (with-session&id
     (io request data)
     (lambda (sess id)
       (let ((c (form (io sess) (confirmation) private (__ are-you-sure-to-hide-this-one?))))
         (if (yes? c)
             (cond ((lookup (publicity (exlibris publicity))
                            ((exlibris
                              (account-id (session->account-id sess)) ; security
                              (id id))))
                    => (lambda (tuple)
                         (match tuple
                           ((pub ex)
                            (cond ((destroy pub)
                                   (rss:query "recent-public-revisions")
                                   (page (io sess) desk id))
                                  (else (page (io sess) private (__ hmm-an-error-occurred)))))
                           (_ (page (io sess) private (__ hmm-an-error-occurred))))))
                   (else
                    (page (io sess) private (__ hmm-an-error-occurred))))
             (page (io sess) desk id))))))

  (define-scenario (edit-review io request data)
    (with-session&id
     (io request data)
     (lambda (sess id)
       (cond ((lookup exlibris
                      `((account-id ,(session->account-id sess)) ; security
                        (id ,id)))
              (let ((r (lookup review `((exlibris-id ,id)))))
                (let loop ((r-new (form (io sess) (review r) private)))
                  (if (review? r-new)
                      (guide (validate-review r-new)
                        (lambda (ht) (loop (form (io sess) (review r-new) private (hashtable->messages ht))))
                        (lambda _
                          (when (review? r) (id-set! r-new (id-of r)))
                          (review-exlibris-id-set! r-new id)
                          (cond  ((save r-new)
                                  (rss:query "recent-reviews")
                                  (page (io sess) desk id))
                                 (else (page (io sess) private (__ hmm-an-error-occurred))))))
                      (page (io sess) desk id)))))
             (else (page (io sess) private (__ hmm-an-error-occurred)))))))

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
         (redirect (io sess) 'index))))

  (define-scenario (detail io request data)
    (with-or-without-session/
     (io request data)
     (sess (id id string->id #f))
     (if id
         (page (io sess) detail id)
         (redirect (io sess) 'index))))

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

  (define (report-format account-id)
    (cond ((lookup preference `((account-id ,account-id))) => preference-report-format)
          (else "plain")))

  (define-scenario (new-report io request data)
    (with-session&id
     (io request data)
     (lambda (sess id)
       (let ((a-id (session->account-id sess)))
         (cond ((lookup exlibris
                        `((account-id ,a-id) ; security
                          (id ,id)))
                => (lambda (ex)
                     (let ((f (report-format a-id)))

                       (define (save-new-report rep)
                         (let ((q (report-to-modify->quotation rep a-id (exlibris-revision-id ex))))
                           (if (save q)
                               (let ((c (report-to-modify->correction rep a-id (id-of q))))
                                 (if (save c)
                                     (let ((r (report-to-modify->report rep a-id (exlibris-revision-id ex) (id-of q) (id-of c))))
                                       (cond ((save r)
                                              (rss:query "recent-reports")
                                              (page (io sess) desk id))
                                             (else (page (io sess) private (__ hmm-an-error-occurred)))))
                                     (page (io sess) private (__ hmm-an-error-occurred))))
                               (page (io sess) private (__ hmm-an-error-occurred)))))

                       (cond ((string=? f "manued")
                              (let loop ((rbm (form (io sess) (report-by-manued) private)))
                                (if (report-by-manued? rbm)
                                    (guide (validate-report-by-manued rbm)
                                      (lambda (ht) (loop (form (io sess) (report-by-manued rbm) private (hashtable->messages ht))))
                                      (lambda (q-body c-body)
                                        (save-new-report (make-report-to-modify (report-by-manued-subject rbm)
                                                                                (report-by-manued-page rbm)
                                                                                (report-by-manued-position rbm)
                                                                                q-body
                                                                                c-body))))
                                    (page (io sess) desk id))))
                             (else
                              (let loop ((rep (form (io sess) (report-to-modify) private)))
                                (if (report-to-modify? rep)
                                    (guide (validate-report-to-modify rep)
                                      (lambda (ht) (loop (form (io sess) (report-to-modify rep) private (hashtable->messages ht))))
                                      (lambda _ (save-new-report rep)))
                                    (page (io sess) desk id))))))))
               (else (redirect (io sess) 'shelf)))))))

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
                    (and (save rep-new)
                         (let ((rep-h (report->report-history rep (id-of rep-new))))
                           (and (save rep-h)
                                (destroy rep)
                                rep-new)))))))))

  (define-scenario (modify-report io request data)
    (with-session/
     (io request data)
     (sess (rep-id report string->id #f)
           (ex-id exlibris string->id #f))
     (if (and rep-id ex-id)
         (match (lookup (report (quotation report) (correction report))
                        ((report
                          (account-id (session->account-id sess)) ; security
                          (id rep-id))))
           ((rep q c)
            (let loop ((modified (form (io sess) (report-to-modify (prepare-report-to-modify rep q c)) private)))
              (if (report-to-modify? modified)
                  (guide (validate-report-to-modify modified)
                    (lambda (ht)
                      (loop (form (io sess) (report-to-modify modified) private (hashtable->messages ht))))
                    (lambda _
                      (cond ((update-report rep q c modified)
                             (rss:query "recent-reports")
                             (page (io sess) desk ex-id))
                            (else (page (io sess) private (__ hmm-an-error-occurred))))))
                  (page (io sess) desk ex-id))))
           (_ (page (io sess) desk ex-id)))
         (redirect (io sess) 'shelf))))

  (define-scenario (drop-report io request data)
    (with-session/
     (io request data)
     (sess (rep-id report string->id #f)
           (ex-id exlibris string->id #f))
     (cond ((and rep-id
                 ex-id
                 (lookup report
                         `((account-id ,(session->account-id sess)) ; security
                           (id ,rep-id))))
            => (lambda (rep)
                 (and (let ((c (form (io sess) (confirmation) private (__ are-you-sure-to-drop-report?))))
                        (yes? c))
                      (destroy rep)
                      (rss:query "recent-reports"))
                 (page (io sess) desk ex-id)))
           (else (redirect (io sess) 'shelf)))))

  (define-scenario (acknowledge io request data)
    (with-session/
     (io request data)
     (sess (r-id report string->id #f)
           (q-id quotation string->id #f))
     (cond ((and q-id
                 (lookup quotation q-id))
            => (lambda (q)
                 (let loop ((a (form (io sess) (acknowledgement) private)))
                   (if (acknowledgement? a)
                       (guide (validate-acknowledgement a)
                         (lambda (ht) (loop (form (io sess) (acknowledgement a) private (hashtable->messages ht))))
                         (lambda _
                           (acknowledgement-account-id-set! a (session->account-id sess))
                           (acknowledgement-quotation-id-set! a q-id)
                           (if (save a)
                               (page (io sess) detail r-id)
                               (page (io sess) private (__ hmm-an-error-occurred)))))
                       (page (io sess) detail r-id)))))
           (else (redirect (io sess) 'table)))))

  (define-scenario (agree io request data)
    (with-session/
     (io request data)
     (sess (r-id report string->id #f)
           (c-id correction string->id #f))
     (cond ((and r-id
                 c-id
                 (lookup correction c-id))
            => (lambda (c)
                 (let loop ((a (form (io sess) (agreement) private)))
                   (if (agreement? a)
                       (guide (validate-agreement a)
                         (lambda (ht) (loop (form (io sess) (agreement a) private (hashtable->messages ht))))
                         (lambda _
                           (agreement-account-id-set! a (session->account-id sess))
                           (agreement-correction-id-set! a c-id)
                           (if (save a)
                               (page (io sess) detail r-id)
                               (page (io sess) private (__ hmm-an-error-occurred)))))
                       (page (io sess) detail r-id)))))
           (else (redirect (io sess) 'index)))))

  (define *password-advice*
    (format "~d文字以上~d文字以下" *account-password-min-length* *account-password-max-length*))

  (define-api (r isbn10 name year month day)
    validate-isbn10/revision-name/year/month/day
    table
    (cond ((lookup (publicity
                    (exlibris publicity)
                    (account exlibris)
                    (revision exlibris)
                    (bib revision))
                   ((bib (isbn10 isbn10))
                    (revision (name name)
                              (revised-at (format "~a-~a-~a" year month day)))))
           => (lambda (tuple) (id-of (cadddr tuple))))
          (else #f)))

  ;; input fields
  (add-input-fields new-account
    ((text "半角英数字")
     (text)
     (password *password-advice*)
     (password *password-advice*)
     (text)))
  (add-input-fields account-to-modify
    ((text)
     (password)
     (password *password-advice*)
     (password *password-advice*)
     (text)))
  (add-input-fields account-to-login
    ((text)
     (password)))
  (add-input-fields forgotten-account
    ((text)))
  (add-input-fields password-reset
    ((password *password-advice*)
     (password *password-advice*)))
  (add-input-fields preference-to-edit
    (((radio (plain "標準" #t) (manued "Manued(真鵺道)方式" #f)))))
  (add-input-fields confirmation
    (((radio (yes "はい" #f) (no "いいえ" #t)))))
  (add-input-fields new-exlibris
    ((text)
     (text "半角英数字で10または13桁(例: \"2222222222\" / \"475614084X\" / \"9784873113487\")")))
  (add-input-fields revision
    (#f
     (text "(例: 「初版第1刷」)")
     (text "(例: 「2007-10-23」「2009-09-09 09:09:09」)")))
  (add-input-fields review
    (#f
     (textarea)))
  (add-input-fields quotation
    (#f
     #f
     (text "(例: 「7」「vi」)")
     (text "(例: 「10行目」「末尾」「図A-1内」)")
     (textarea)))
  (add-input-fields correction
    (#f
     #f
     (textarea)))
  (add-input-fields report-to-modify
    ((text)
     (text "(例: 「7」「vi」)")
     (text "(例: 「10行目」「末尾」「図A-1内」)")
     (textarea)
     (textarea)))
  (add-input-fields report-by-manued
    ((text)
     (text "(例: 「7」「vi」)")
     (text "(例: 「10行目」「末尾」「図A-1内」)")
     (textarea "例: 「あ[あ/い]うえお」「0123[6|5|4]789」")))
  (add-input-fields acknowledgement
    (#f
     #f
     ((select (positive "賛成") (negative "反対")))
     (textarea)))
  (add-input-fields agreement
    (#f
     #f
     (textarea)))

  ;; templates
  (template-environment (except (rnrs) div)
                        (lunula html)
                        (errata helper))

  )
