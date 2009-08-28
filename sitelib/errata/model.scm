(library (errata model)
  (export account-to-login
          account-to-login?
          make-account-to-login
          account-to-login-nick
          account-to-login-password
          bib
          bib?
          make-bib
          bib-id
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
          revision-id
          revision-id-set!
          revision-bib-id
          revision-bib-id-set!
          revision-name
          revision-name-set!
          revision-revised-at
          exlibris
          exlibris?
          make-exlibris
          exlibris-id
          exlibris-id-set!
          exlibris-account-id
          exlibris-revision-id
          exlibris-revision-id-set!
          publicity
          publicity?
          make-publicity
          publicity-id
          publicity-id-set!
          publicity-exlibris-id
          publicity-exlibris-id-set!
          review
          review?
          make-review
          review-id
          review-id-set!
          review-exlibris-id
          review-exlibris-id-set!
          review-body
          quotation
          quotation?
          valid-quotation?
          make-quotation
          quotation-id
          quotation-id-set!
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
          correction-id
          correction-id-set!
          correction-account-id
          correction-account-id-set!
          correction-quotation-id
          correction-quotation-id-set!
          correction-body
          report
          report?
          make-report
          report-id
          report-id-set!
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
          )
  (import (rnrs)
          (lunula session))

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
    (fields (mutable id) (mutable title) (mutable isbn13) (mutable isbn10) (mutable image))
    (protocol
     (lambda (p)
       (lambda (id title isbn13 isbn10 image)
         (p (maybe-number id)
            title
            isbn13
            isbn10
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
    (fields (mutable id) account-id (mutable revision-id))
    (protocol
     (lambda (p)
       (lambda (id account-id revision-id)
         (p (maybe-number id)
            (maybe-number account-id)
            (maybe-number revision-id))))))

  (define-record-type publicity
    (fields (mutable id) (mutable exlibris-id))
    (protocol
     (lambda (p)
       (lambda (id exlibris-id)
         (p (maybe-number id)
            (maybe-number exlibris-id))))))

  (define-record-type review
    (fields (mutable id) (mutable exlibris-id) body)
    (protocol
     (lambda (p)
       (lambda (id exlibris-id body)
         (p (maybe-number id)
            (maybe-number exlibris-id)
            body)))))

  (define-record-type quotation
    (fields (mutable id) (mutable account-id) (mutable revision-id) page position body)
    (protocol
     (lambda (p)
       (lambda (id account-id revision-id page position body)
         (p (maybe-number id)
            (maybe-number account-id)
            (maybe-number revision-id)
            page
            position
            body)))))

  (define (valid-quotation? q)
    (quotation? q))

  (define-record-type correction
    (fields (mutable id) (mutable account-id) (mutable quotation-id) body)
    (protocol
     (lambda (p)
       (lambda (id account-id quotation-id body)
         (p (maybe-number id)
            (maybe-number account-id)
            (maybe-number quotation-id)
            body)))))

  (define (valid-correction? c)
    (correction? c))

  (define-record-type report
    (fields (mutable id) (mutable account-id) (mutable revision-id) subject (mutable quotation-id) (mutable correction-id))
    (protocol
     (lambda (p)
       (lambda (id account-id exlibris-id subject quotation-id correction-id)
         (p (maybe-number id)
            (maybe-number account-id)
            (maybe-number revision-id)
            subject
            (maybe-number quotation-id)
            (maybe-number correction-id))))))

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

)
