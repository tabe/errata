(library (errata validator)
  (export *account-password-min-length*
          *account-password-max-length*
          *account-nick-max-length*
          *account-mail-address-max-length*
          validate-account-to-modify
          validate-new-account
          authenticate-account
          existing-mail-address
          validate-password-reset
          validate-password-to-modify
          validate-preference-to-edit
          validate-bib-title
          validate-revision
          validate-review
          validate-report-to-modify
          validate-report-by-manued
          validate-acknowledgement
          validate-agreement
          validate-year
          validate-month
          validate-day
          validate-/isbn10/revision-name/year/month/day
          validate-/uuid
          validate-/uuid/revision-name/year/month/day)
  (import (rnrs)
          (pregexp)
          (prefix (only (lunula hmac) sha-256) hmac:)
          (prefix (manued) manued:)
          (only (lunula mysql) lookup)
          (lunula session)
          (lunula validation)
          (only (errata isbn) valid-isbn10?)
          (errata model))

  (define *account-name-min-length* 1)
  (define *account-name-max-length* 32)
  (define *account-nick-min-length* 1)
  (define *account-nick-max-length* 16)
  (define *account-password-min-length* 8)
  (define *account-password-max-length* 32)
  (define *account-mail-address-min-length* 6)
  (define *account-mail-address-max-length* 256)

  (define *bib-title-min-length* 1)
  (define *bib-title-max-length* 128)
  (define *bib-image-min-length* 10)
  (define *bib-image-max-length* 256)

  (define *revision-name-min-length* 1)
  (define *revision-name-max-length* 32)

  (define *review-body-min-length* 1)
  (define *review-body-max-length* 1024)

  (define *quotation-page-min-length* 1)
  (define *quotation-page-max-length* 32)
  (define *quotation-position-min-length* 1)
  (define *quotation-position-max-length* 32)
  (define *quotation-body-min-length* 1)
  (define *quotation-body-max-length* 256)

  (define *correction-body-min-length* 0)
  (define *correction-body-max-length* 512)

  (define *report-subject-min-length* 0)
  (define *report-subject-max-length* 64)

  (define *acknowledgement-comment-min-length* 1)
  (define *acknowledgement-comment-max-length* 1024)

  (define *agreement-comment-min-length* 1)
  (define *agreement-comment-max-length* 1024)

  (define *uuid-max-length* 36)

  (define-string-length-validator validate-account-name
    (name-is-blank name-too-long)
    (*account-name-max-length*))

  (define-string-length-validator validate-password
    (password-is-blank password-too-short password-too-long)
    (*account-password-min-length* *account-password-max-length*))

  (define-validator (validate-nick nick)
    (nick-is-blank nick-too-long nick-invalid-char)
    (let ((len (string-length nick)))
      (cond ((zero? len) (nick-is-blank))
            (else
             (when (< *account-nick-max-length* len)
               (nick-too-long))
             (unless (pregexp-match "^[A-Za-z_][A-Za-z0-9_]*$" nick)
               (nick-invalid-char))))))

  (define-validator (validate-uuid uuid)
    (uuid-is-blank uuid-too-long uuid-invalid-char)
    (let ((len (string-length uuid)))
      (cond ((zero? len) (uuid-is-blank))
            (else
             (when (< *uuid-max-length* len)
               (uuid-too-long))
             (unless (pregexp-match "^[A-Za-z0-9-]+$" uuid)
               (uuid-invalid-char))))))

  (define-string-length-validator validate-mail-address
    (mail-address-is-blank mail-address-too-short mail-address-too-long)
    (*account-mail-address-min-length* *account-mail-address-max-length*))

  (define-composite-validator validate-account-to-modify
    ((lambda (a _) (account-to-modify-name a)) validate-account-name)
    ((lambda (a _) (account-to-modify-mail-address a)) validate-mail-address)
    ((lambda (a _) (account-to-modify-password a)) validate-password)
    ((lambda (a c) (make-account-to-login (account-nick c) (account-to-modify-password a)))
     existing-account))

  (define-validator (validate-new-nick nick)
    (nick-already-used)
    (when (lookup account ((nick nick)))
      (nick-already-used)))

  (define-validator (same-password password password-re)
    (password-differs-from-re)
    (unless (string=? password password-re)
      (password-differs-from-re)))

  (define-composite-validator validate-new-account
    (new-account-nick validate-nick validate-new-nick)
    (new-account-name validate-account-name)
    (new-account-password validate-password)
    (new-account-password-re validate-password)
    ((lambda (x) (values (new-account-password x)
                         (new-account-password-re x)))
     same-password)
    (new-account-mail-address validate-mail-address))

  (define-validator (existing-account a)
    (does-not-exist)
    (cond ((lookup account ((nick (account-to-login-nick a))))
           => (lambda (found)
                (let ((algorithm (account-hash-algorithm found)))
                  (cond ((string=? "sha-256" algorithm)
                         (if (string=? (account-password found)
                                       (hmac:sha-256 (account-hash-key found)
                                                     (string->utf8 (account-to-login-password a))))
                             found
                             (does-not-exist)))
                        (else (does-not-exist))))))
          (else (does-not-exist))))

  (define-composite-validator authenticate-account
    (account-to-login-nick validate-nick)
    (account-to-login-password validate-password)
    (values existing-account))

  (define-validator (existing-mail-address a)
    (does-not-exist)
    (or (lookup account ((mail-address (forgotten-account-mail-address a))))
        (does-not-exist)))

  (define-composite-validator validate-password-reset
    (password-reset-password validate-password)
    (password-reset-password-re validate-password)
    ((lambda (x) (values (password-reset-password x)
                         (password-reset-password-re x)))
     same-password))

  (define-composite-validator validate-password-to-modify
    ((lambda (p _) (password-to-modify-current-password p)) validate-password)
    ((lambda (p c) (make-account-to-login (account-nick c) (password-to-modify-current-password p)))
     existing-account)
    ((lambda (p _) (password-to-modify-new-password p)) validate-password)
    ((lambda (p _) (password-to-modify-new-password-re p)) validate-password)
    ((lambda (p _) (values (password-to-modify-new-password p)
                           (password-to-modify-new-password-re p)))
     same-password))

  (define-validator (validate-preference-report-format str)
    (invalid-report-format)
    (unless (member str '("plain" "manued"))
      (invalid-report-format)))

  (define-composite-validator validate-preference-to-edit
    (preference-to-edit-report-format validate-preference-report-format))

  (define-string-length-validator validate-bib-title
    (title-is-blank title-too-long)
    (*bib-title-max-length*))

  (define-composite-validator validate-bib
    (bib-title validate-bib-title))

  (define-string-length-validator validate-revision-name
    (name-is-blank name-too-long)
    (*revision-name-max-length*))

  (define-composite-validator validate-revision
    (revision-name validate-revision-name))

  (define-string-length-validator validate-review-body
    (body-is-blank body-too-long)
    (*review-body-max-length*))

  (define-composite-validator validate-review
    (review-body validate-review-body))

  (define-string-length-validator validate-quotation-page
    (page-is-blank page-too-long)
    (*quotation-page-max-length*))

  (define-string-length-validator validate-quotation-position
    (position-is-blank position-too-long)
    (*quotation-position-max-length*))

  (define-string-length-validator validate-quotation-body
    (body-is-blank body-too-long)
    (*quotation-body-max-length*))

  (define-composite-validator validate-quotation
    (quotation-page validate-quotation-page)
    (quotation-position validate-quotation-position)
    (quotation-body validate-quotation-body))

  (define-string-length-validator validate-correction-body
    (body-too-long)
    (*correction-body-max-length*))

  (define-composite-validator validate-correction
    (correction-body validate-correction-body))

  (define-string-length-validator validate-report-subject
    (subject-is-blank subject-too-long)
    (*report-subject-max-length*))

  (define-composite-validator validate-report
    (report-subject validate-report-subject))

  (define-composite-validator validate-report-to-modify
    (report-to-modify-subject validate-report-subject)
    (report-to-modify-page validate-quotation-page)
    (report-to-modify-position validate-quotation-position)
    (report-to-modify-quotation-body validate-quotation-body)
    (report-to-modify-correction-body validate-correction-body))

  (define-condition-validator validate-report-by-manued-body
    ((invalid-manued-format manued:manued-condition?))
    manued:string->before&after
    validate-quotation-body
    validate-correction-body)

  (define-composite-validator validate-report-by-manued
    (report-by-manued-subject validate-report-subject)
    (report-by-manued-page validate-quotation-page)
    (report-by-manued-position validate-quotation-position)
    (report-by-manued-body validate-report-by-manued-body))

  (define-validator (validate-acknowledgement-sign str)
    (invalid-sign)
    (unless (member str '("positive" "negative"))
      (invalid-sign)))

  (define-string-length-validator validate-acknowledgement-comment
    (comment-is-blank comment-too-long)
    (*acknowledgement-comment-max-length*))

  (define-composite-validator validate-acknowledgement
    (acknowledgement-sign validate-acknowledgement-sign)
    (acknowledgement-comment validate-acknowledgement-comment))

  (define-string-length-validator validate-agreement-comment
    (comment-is-blank comment-too-long)
    (*agreement-comment-max-length*))

  (define-composite-validator validate-agreement
    (agreement-comment validate-agreement-comment))

  (define-predicate-validator validate-isbn10
    invalid-isbn10 valid-isbn10?)

  (define-predicate-validator validate-year
    invalid-year
    (lambda (s) (pregexp-match "^[12][0-9]{3}$" s)))

  (define-predicate-validator validate-month
    invalid-month
    (lambda (s) (pregexp-match "^(?:0[1-9]|1[012])$" s)))

  (define-predicate-validator validate-day
    invalid-day
    (lambda (s) (pregexp-match "^(?:0[1-9]|1[0-9]|2[0-9]|3[01])$" s)))

  (define-composite-validator validate-/isbn10/revision-name/year/month/day
    ((0) validate-isbn10)
    ((1) validate-revision-name)
    ((2) validate-year)
    ((3) validate-month)
    ((4) validate-day))

  (define-composite-validator validate-/uuid
    ((0) validate-uuid))

  (define-composite-validator validate-/uuid/revision-name/year/month/day
    ((0) validate-uuid)
    ((1) validate-revision-name)
    ((2) validate-year)
    ((3) validate-month)
    ((4) validate-day))

)
