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
          (lunula))

  (define-record-type bib
    (fields id name)
    (protocol
     (lambda (p)
       (lambda (x y)
         (p (if (string? x) (string->number x) x)
            y)))))

  (define (valid-account? a)
    (and (account? a)
         (let ((name (account-name a))
               (password (account-password a)))
           (and (pregexp-match "^[A-Za-z_][A-Za-z0-9_]*$" name)
                (<= 8 (string-length password))))))

  (define (new-account? a)
    (and (account? a)
         (not (lookup account `((name ,(account-name a))
                                (password ,(account-password a)))))))

  (define (authenticate-account a)
    (and (account? a)
         (lookup account `((name ,(account-name a))
                           (password ,(account-password a))))))

  (define-scenario (sign-up io request)
    (let ((x (parameter-of request)))
      (cond ((logged-in? x)
             => (lambda (sess)
                  (page (io sess) "You have already logged in!")))
            (else
             (let lp ((a (form (io) (account))))
               (cond ((and (valid-account? a)
                           (new-account? a))
                      (if (save a)
                          (page (io) "Now you have your own account!")
                          (page (io) "Hmm ... we have failed to create your account.")))
                     (else
                      (lp (form (io) (account a) "Please retry!")))))))))

  (define-scenario (cancel io request)
    (let ((x (parameter-of request)))
      (cond ((logged-in? x)
             => (lambda (sess)
                  (if (destroy (user-account (session-user sess)))
                      (page (io) "Now you have left errata.fixedpoint.jp. Thanks for your favor.")
                      (page (io sess) "Hmm ... an error occurred."))))
            (else
             (redirect (io) "/")))))

  (define-scenario (login io request)
    (let ((x (parameter-of request)))
      (cond ((logged-in? x)
             => (lambda (sess)
                  (page (io sess) "you have already logged in!")))
            ((form (io) (account))
             => (lambda (a)
                  (let lp ((a a))
                    (cond ((and (valid-account? a)
                                (authenticate-account a))
                           => (lambda (a)
                                (let ((sess (do-login a)))
                                  (page (io sess) "Now you have logged in!"))))
                          (else
                           (lp (form (io) (account a) "Please retry!")))))))
            (else
             (redirect (io) "/")))))

  (define-scenario (logout io request)
    (let ((x (parameter-of request)))
      (cond ((logged-in? x)
             => (lambda (sess)
                  (do-logout sess)
                  (page (io) "Now you have logged out!")))
            (else
             (redirect (io) "/")))))

  (define-scenario (put-on io request)
    (let ((x (parameter-of request)))
      (cond ((logged-in? x)
             => (lambda (sess)
                  (let lp ((b (form (io sess) (bib))))
                    (cond ((string-null? (bib-name b))
                           (lp (form (io sess) (bib b) "Please input name!")))
                          (else
                           (let ((c (form (io sess) (confirmation) "Is this content OK?")))
                             (cond ((pregexp-match "^[yY]" (ok? c))
                                    (if (save b)
                                        (page (io sess) "Now new bib \"~a\" has been put on your bookshelf." (bib-name b))
                                        (page (io sess) "Hmm ... we have failed to create your bib.")))
                                   (else
                                    (lp (form (io sess) (bib b) "Please retry!"))))))))))
            (else
             (redirect (io) "/login")))))

  )
