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
          bib-isbn
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
          review-body)
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

)
