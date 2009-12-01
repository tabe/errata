(library (errata model)
  (export new-account
          new-account?
          make-new-account
          new-account-nick
          new-account-name
          new-account-password
          new-account-password-re
          new-account-mail-address
          new-account->account
          account-to-modify
          account-to-modify?
          make-account-to-modify
          account->account-to-modify
          account-to-modify->account
          account-to-modify-name
          account-to-modify-mail-address
          account-to-modify-password
          account-to-login
          account-to-login?
          make-account-to-login
          account-to-login-nick
          account-to-login-password
          forgotten-account
          forgotten-account?
          make-forgotten-account
          forgotten-account-mail-address
          password-reset
          password-reset?
          make-password-reset
          password-reset-password
          password-reset-password-re
          password-reset->account
          make-password-to-modify
          password-to-modify
          password-to-modify?
          password-to-modify-current-password
          password-to-modify-new-password
          password-to-modify-new-password-re
          password-to-modify->account
          preference
          preference?
          make-preference
          preference-account-id
          preference-account-id-set!
          preference-gravatar
          preference-report-format
          preference-to-edit
          preference-to-edit?
          make-preference-to-edit
          preference-to-edit-gravatar
          preference-to-edit-report-format
          bib
          bib?
          make-bib
          bib-uuid
          bib-title
          bib-title-set!
          bib-isbn10
          bib-isbn10-set!
          bib-isbn13
          bib-isbn13-set!
          bib-image
          bib-image-set!
          string->bib
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
          exlibris-position
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
          review->caption
          quotation
          quotation?
          make-quotation
          quotation-account-id
          quotation-account-id-set!
          quotation-revision-id
          quotation-revision-id-set!
          quotation-body
          quotation-font-face
          occurrence
          occurrence?
          make-occurrence
          occurrence-account-id
          occurrence-quotation-id
          occurrence-page
          occurrence-position
          occurrence-to-add
          occurrence-to-add?
          make-occurrence-to-add
          occurrence-to-add-page
          occurrence-to-add-position
          correction
          correction?
          make-correction
          correction-account-id
          correction-account-id-set!
          correction-quotation-id
          correction-quotation-id-set!
          correction-body
          correction-font-face
          report
          report?
          make-report
          report-uuid
          report-account-id
          report-account-id-set!
          report-revision-id
          report-revision-id-set!
          report-subject
          report-quotation-id
          report-quotation-id-set!
          report-occurrence-id
          report-occurrence-id-set!
          report-correction-id
          report-correction-id-set!
          report->caption
          report-by-manued
          report-by-manued?
          make-report-by-manued
          report-by-manued-subject
          report-by-manued-page
          report-by-manued-position
          report-by-manued-body
          report-by-manued-quotation-font-face
          report-by-manued-correction-font-face
          report-to-modify
          report-to-modify?
          make-report-to-modify
          report-to-modify-subject
          report-to-modify-page
          report-to-modify-position
          report-to-modify-quotation-body
          report-to-modify-quotation-font-face
          report-to-modify-correction-body
          report-to-modify-correction-font-face
          report-history
          report-history?
          report-history-subject
          report->report-history
          acknowledgement
          acknowledgement?
          make-acknowledgement
          acknowledgement-account-id
          acknowledgement-account-id-set!
          acknowledgement-quotation-id
          acknowledgement-quotation-id-set!
          acknowledgement-sign
          acknowledgement-comment
          acknowledgement-positive?
          acknowledgement-negative?
          acknowledgement->caption
          agreement
          agreement?
          make-agreement
          agreement-account-id
          agreement-account-id-set!
          agreement-correction-id
          agreement-correction-id-set!
          agreement-comment
          agreement->caption
          recent-acknowledgements
          recent-agreements
          recent-revisions
          recent-reviews
          recent-reports
          )
  (import (rnrs)
          (only (core) make-uuid)
          (prefix (lunula hmac) hmac:)
          (only (lunula mysql) lookup-all)
          (lunula persistent-record)
          (lunula session)
          (only (lunula string) string-truncate))

  (define-record-type new-account
    (fields nick name password password-re mail-address))

  (define (new-account->account a)
    (let ((key (make-uuid)))
      (make-account (new-account-nick a)
                    (new-account-name a)
                    (hmac:sha-256 key (string->utf8 (new-account-password a)))
                    (new-account-mail-address a)
                    "sha-256"
                    key)))

  (define-record-type account-to-modify
    (fields name mail-address password))

  (define (account->account-to-modify a)
    (make-account-to-modify (account-name a)
                            (account-mail-address a)
                            #f))

  (define (account-to-modify->account a current-account)
    (let* ((acc (make-account (account-nick current-account)
                              (account-to-modify-name a)
                              (account-password current-account)
                              (account-to-modify-mail-address a)
                              (account-hash-algorithm current-account)
                              (account-hash-key current-account))))
      (id-set! acc (id-of current-account))
      acc))

  (define-record-type account-to-login
    (fields nick password))

  (define-record-type forgotten-account
    (fields mail-address))

  (define-record-type password-reset
    (fields password password-re))

  (define (password-reset->account p current-account)
    (let* ((key (make-uuid))
           (a (make-account #f
                            #f
                            (hmac:sha-256 key (string->utf8 (password-reset-password p)))
                            #f
                            "sha-256"
                            key)))
      (id-set! a (id-of current-account))
      a))

  (define-record-type password-to-modify
    (fields current-password new-password new-password-re))

  (define (password-to-modify->account p current-account)
    (let* ((key (make-uuid))
           (a (make-account (account-nick current-account)
                            (account-name current-account)
                            (hmac:sha-256 key (string->utf8 (password-to-modify-new-password p)))
                            (account-mail-address current-account)
                            "sha-256"
                            key)))
      (id-set! a (id-of current-account))
      a))

  (define-persistent-record-type preference
    (fields (mutable account-id) gravatar report-format)
    (protocol
     (persistent-protocol
      (lambda (p)
        (lambda (account-id gravatar report-format)
          (p (maybe-id account-id)
             (maybe-integer gravatar)
             report-format))))))

  (define-record-type preference-to-edit
    (fields gravatar report-format)
    (protocol
     (lambda (p)
       (lambda (gravatar report-format)
         (p (maybe-integer gravatar)
            report-format)))))

  (define-persistent-record-type bib
    (fields uuid (mutable title) (mutable isbn13) (mutable isbn10) (mutable image))
    (protocol
     (persistent-protocol
      (lambda (p)
        (lambda (uuid title isbn13 isbn10 image)
          (p uuid
             title
             isbn13
             isbn10
             image))))))

  (define (string->bib str)
    (call-with-port (open-string-input-port str)
      (lambda (port)
        (let* ((isbn13 (get-line port))
               (isbn10 (get-line port))
               (title  (get-line port))
               (url    (get-line port)))
          (make-bib (make-uuid)
                    (and (not (eof-object? title)) title)
                    (and (not (eof-object? isbn13)) isbn13)
                    (and (not (eof-object? isbn10)) isbn10)
                    (and (not (eof-object? url)) url))))))

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
    (fields account-id (mutable revision-id) position)
    (protocol
     (persistent-protocol
      (lambda (p)
        (lambda (account-id revision-id position)
          (p (maybe-id account-id)
             (maybe-id revision-id)
             (maybe-integer position)))))))

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

  (define (review->caption rvw)
    (string-truncate (review-body rvw) 32))

  (define-persistent-record-type quotation
    (fields (mutable account-id) (mutable revision-id) body font-face)
    (protocol
     (persistent-protocol
      (lambda (p)
        (lambda (account-id revision-id body font-face)
          (p (maybe-id account-id)
             (maybe-id revision-id)
             body
             font-face))))))

  (define-persistent-record-type occurrence
    (fields account-id quotation-id page position)
    (protocol
     (persistent-protocol
      (lambda (p)
        (lambda (account-id quotation-id page position)
          (p (maybe-id account-id)
             (maybe-id quotation-id)
             page
             position))))))

  (define-record-type occurrence-to-add
    (fields page position))

  (define-persistent-record-type correction
    (fields (mutable account-id) (mutable quotation-id) body font-face)
    (protocol
     (persistent-protocol
      (lambda (p)
        (lambda (account-id quotation-id body font-face)
          (p (maybe-id account-id)
             (maybe-id quotation-id)
             body
             font-face))))))

  (define-persistent-record-type report
    (fields uuid (mutable account-id) (mutable revision-id) subject (mutable quotation-id) (mutable occurrence-id) (mutable correction-id))
    (protocol
     (persistent-protocol
      (lambda (p)
        (lambda (uuid account-id revision-id subject quotation-id occurrence-id correction-id)
          (p uuid
             (maybe-id account-id)
             (maybe-id revision-id)
             subject
             (maybe-id quotation-id)
             (maybe-id occurrence-id)
             (maybe-id correction-id)))))))

  (define (report->caption r)
    (string-truncate (report-subject r) 32))

  (define-record-type report-by-manued
    (fields subject page position body quotation-font-face correction-font-face))

  (define-record-type report-to-modify
    (fields subject page position quotation-body quotation-font-face correction-body correction-font-face))

  (define-persistent-record-type report-history
    (fields uuid account-id revision-id subject quotation-id occurrence-id correction-id)
    (protocol
     (persistent-protocol
      (lambda (p)
        (lambda (uuid account-id revision-id subject quotation-id occurrence-id correction-id)
          (p uuid
             (maybe-id account-id)
             (maybe-id revision-id)
             subject
             (maybe-id quotation-id)
             (maybe-id occurrence-id)
             (maybe-id correction-id)))))))

  (define (report->report-history r)
    (make-report-history (report-uuid r)
                         (report-account-id r)
                         (report-revision-id r)
                         (report-subject r)
                         (report-quotation-id r)
                         (report-occurrence-id r)
                         (report-correction-id r)))

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

  (define (acknowledgement-positive? a)
    (assert (acknowledgement? a))
    (string=? (acknowledgement-sign a) "positive"))

  (define (acknowledgement-negative? a)
    (assert (acknowledgement? a))
    (string=? (acknowledgement-sign a) "negative"))

  (define (acknowledgement->caption a)
    (assert (acknowledgement? a))
    (string-truncate (acknowledgement-comment a) 32))

  (define-persistent-record-type agreement
    (fields (mutable account-id) (mutable correction-id) comment)
    (protocol
     (persistent-protocol
      (lambda (p)
        (lambda (account-id correction-id comment)
          (p (maybe-id account-id)
             (maybe-id correction-id)
             comment))))))

  (define (agreement->caption a)
    (assert (agreement? a))
    (string-truncate (agreement-comment a) 32))

  (define (recent-acknowledgements n)
    (lookup-all (acknowledgement
                 (account acknowledgement)
                 (preference (account left))
                 (quotation acknowledgement)
                 (report (quotation))
                 (revision quotation)
                 (bib revision))
                ((exists (publicity
                          (exlibris publicity))
                         ((exlibris (revision)))))
                ((order-by (acknowledgement (updated-at desc)))
                 (limit n))))

  (define (recent-agreements n)
    (lookup-all (agreement
                 (account agreement)
                 (preference (account left))
                 (correction agreement)
                 (quotation correction)
                 (report (quotation))
                 (revision quotation)
                 (bib revision))
                ((exists (publicity
                          (exlibris publicity))
                         ((exlibris (revision)))))
                ((order-by (agreement (updated-at desc)))
                 (limit n))))

  (define (recent-revisions n)
    (lookup-all (publicity
                 (exlibris publicity)
                 (account exlibris)
                 (preference (account left))
                 (revision exlibris)
                 (bib revision))
                ()
                ((order-by (publicity (created-at desc)))
                 (limit n))))

  (define (recent-reviews n)
    (lookup-all (review
                 (exlibris review)
                 (account exlibris)
                 (preference (account left))
                 (revision exlibris)
                 (bib revision))
                ((exists (publicity)
                         ((publicity (exlibris)))))
                ((order-by (review (updated-at desc)))
                 (limit n))))

  (define (recent-reports n)
    (lookup-all (report
                 (account report)
                 (preference (account left))
                 (revision report)
                 (bib revision)
                 (quotation report)
                 (occurrence report)
                 (correction report))
                ((exists (publicity (exlibris publicity))
                         ((exlibris (revision))
                          (exlibris (account)))))
                ((order-by (report (created-at desc)))
                 (limit n))))

)
