(library (errata model)
  (export new-account
          new-account?
          make-new-account
          new-account-nick
          new-account-name
          new-account-password
          new-account-mail-address
          new-account->account
          account-to-modify
          account-to-modify?
          make-account-to-modify
          account->account-to-modify
          account-to-modify->account
          account-to-modify-name
          account-to-modify-current-password
          account-to-modify-new-password
          account-to-modify-mail-address
          account-to-login
          account-to-login?
          make-account-to-login
          account-to-login-nick
          account-to-login-password
          bib
          bib?
          make-bib
          bib-title
          bib-title-set!
          bib-isbn10
          bib-isbn10-set!
          bib-isbn13
          bib-isbn13-set!
          bib-image
          bib-image-set!
          revision
          revision?
          make-revision
          revision-bib-id
          revision-bib-id-set!
          revision-name
          revision-name-set!
          revision-revised-at
          exlibris
          exlibris?
          make-exlibris
          exlibris-account-id
          exlibris-revision-id
          exlibris-revision-id-set!
          new-exlibris
          new-exlibris?
          make-new-exlibris
          new-exlibris-title
          new-exlibris-isbn
          publicity
          publicity?
          make-publicity
          publicity-exlibris-id
          publicity-exlibris-id-set!
          review
          review?
          make-review
          review-exlibris-id
          review-exlibris-id-set!
          review-body
          quotation
          quotation?
          valid-quotation?
          make-quotation
          quotation-account-id
          quotation-account-id-set!
          quotation-revision-id
          quotation-revision-id-set!
          quotation-page
          quotation-position
          quotation-body
          correction
          correction?
          valid-correction?
          make-correction
          correction-account-id
          correction-account-id-set!
          correction-quotation-id
          correction-quotation-id-set!
          correction-body
          report
          report?
          make-report
          report-account-id
          report-account-id-set!
          report-revision-id
          report-revision-id-set!
          report-subject
          report-quotation-id
          report-quotation-id-set!
          report-correction-id
          report-correction-id-set!
          report-to-modify
          report-to-modify?
          valid-report-to-modify?
          make-report-to-modify
          report-to-modify-subject
          report-to-modify-page
          report-to-modify-position
          report-to-modify-quotation-body
          report-to-modify-correction-body
          acknowledgement
          acknowledgement?
          valid-acknowledgement?
          make-acknowledgement
          acknowledgement-account-id
          acknowledgement-account-id-set!
          acknowledgement-quotation-id
          acknowledgement-quotation-id-set!
          acknowledgement-sign
          acknowledgement-comment
          acknowledgement-positive?
          acknowledgement-negative?
          agreement
          agreement?
          valid-agreement?
          make-agreement
          agreement-account-id
          agreement-account-id-set!
          agreement-correction-id
          agreement-correction-id-set!
          agreement-comment
          )
  (import (rnrs)
          (only (core) make-uuid)
          (prefix (lunula hmac) hmac:)
          (lunula persistent-record)
          (lunula session))

  (define-record-type new-account
    (fields nick name password mail-address))

  (define (new-account->account a)
    (let ((key (make-uuid)))
      (make-account (new-account-nick a)
                    (new-account-name a)
                    (hmac:sha-256 key (string->utf8 (new-account-password a)))
                    (new-account-mail-address a)
                    "sha-256"
                    key)))

  (define-record-type account-to-modify
    (fields name current-password new-password mail-address))

  (define (account->account-to-modify a)
    (make-account-to-modify (account-name a)
                            #f
                            #f
                            (account-mail-address a)))

  (define (account-to-modify->account a current-account)
    (let ((key (make-uuid)))
      (make-account (account-nick current-account)
                    (account-to-modify-name a)
                    (hmac:sha-256 key (string->utf8 (account-to-modify-new-password a)))
                    (account-to-modify-mail-address a)
                    "sha-256"
                    key)))

  (define-record-type account-to-login
    (fields nick password))

  (define-persistent-record-type bib
    (fields (mutable title) (mutable isbn13) (mutable isbn10) (mutable image))
    (protocol
     (persistent-protocol
      (lambda (p)
        (lambda (title isbn13 isbn10 image)
          (p title
             isbn13
             isbn10
             image))))))

  (define-persistent-record-type revision
    (fields (mutable bib-id) (mutable name) revised-at)
    (protocol
     (persistent-protocol
      (lambda (p)
        (lambda (bib-id name revised-at)
          (p (maybe-id bib-id)
             name
             revised-at))))))

  (define-persistent-record-type exlibris
    (fields account-id (mutable revision-id))
    (protocol
     (persistent-protocol
      (lambda (p)
        (lambda (account-id revision-id)
          (p (maybe-id account-id)
             (maybe-id revision-id)))))))

  (define-record-type new-exlibris
    (fields title isbn))

  (define-persistent-record-type publicity
    (fields (mutable exlibris-id))
    (protocol
     (persistent-protocol
      (lambda (p)
        (lambda (exlibris-id)
          (p (maybe-id exlibris-id)))))))

  (define-persistent-record-type review
    (fields (mutable exlibris-id) body)
    (protocol
     (persistent-protocol
      (lambda (p)
        (lambda (exlibris-id body)
          (p (maybe-id exlibris-id)
             body))))))

  (define-persistent-record-type quotation
    (fields (mutable account-id) (mutable revision-id) page position body)
    (protocol
     (persistent-protocol
      (lambda (p)
        (lambda (account-id revision-id page position body)
          (p (maybe-id account-id)
             (maybe-id revision-id)
             page
             position
             body))))))

  (define (valid-quotation? q)
    (quotation? q))

  (define-persistent-record-type correction
    (fields (mutable account-id) (mutable quotation-id) body)
    (protocol
     (persistent-protocol
      (lambda (p)
        (lambda (account-id quotation-id body)
          (p (maybe-id account-id)
             (maybe-id quotation-id)
             body))))))

  (define (valid-correction? c)
    (correction? c))

  (define-persistent-record-type report
    (fields (mutable account-id) (mutable revision-id) subject (mutable quotation-id) (mutable correction-id))
    (protocol
     (persistent-protocol
      (lambda (p)
        (lambda (account-id revision-id subject quotation-id correction-id)
          (p (maybe-id account-id)
             (maybe-id revision-id)
             subject
             (maybe-id quotation-id)
             (maybe-id correction-id)))))))

  (define-record-type report-to-modify
    (fields subject page position quotation-body correction-body))

  (define (valid-report-to-modify? x)
    (and (report-to-modify? x)
         (let ((qb (report-to-modify-quotation-body x))
               (cb (report-to-modify-correction-body x)))
           (and (string? qb)
                (not (string=? qb ""))
                (string? cb)
                (not (string=? qb cb))))))

  (define (valid-comment? c)
    (and (string? c)
         (< (string-length c) 1024)))

  (define-persistent-record-type acknowledgement
    (fields (mutable account-id) (mutable quotation-id) sign comment)
    (protocol
     (persistent-protocol
      (lambda (p)
        (lambda (account-id quotation-id sign comment)
          (p (maybe-id account-id)
             (maybe-id quotation-id)
             sign
             comment))))))

  (define (valid-acknowledgement? a)
    (and (acknowledgement? a)
         (member (acknowledgement-sign a) '("positive" "negative"))
         (valid-comment? (acknowledgement-comment a))))

  (define (acknowledgement-positive? a)
    (assert (acknowledgement? a))
    (string=? (acknowledgement-sign a) "positive"))

  (define (acknowledgement-negative? a)
    (assert (acknowledgement? a))
    (string=? (acknowledgement-sign a) "negative"))

  (define-persistent-record-type agreement
    (fields (mutable account-id) (mutable correction-id) comment)
    (protocol
     (persistent-protocol
      (lambda (p)
        (lambda (account-id correction-id comment)
          (p (maybe-id account-id)
             (maybe-id correction-id)
             comment))))))

  (define (valid-agreement? a)
    (and (agreement? a)
         (valid-comment? (agreement-comment a))))

)
