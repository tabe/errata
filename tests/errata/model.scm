#!r6rs

(import (rnrs)
        (only (lunula mysql) close connect)
        (only (lunula persistent-record) id-of id-set!)
        (lunula session)
        (only (errata configuration) mysql-user mysql-password mysql-database)
        (errata model)
        (only (xunit) assert-list? skip-unless)
        (rename (xunit) (report xunit:report)))

(let ((a (make-account-to-modify "F. M. Name" "name@example.com" "00000000"))
      (current-account (make-account "nick" "Full Name" "00000000" "nick@example.com" "plain" "")))
  (id-set! current-account 100)
  (let ((acc (account-to-modify->account a current-account)))
    (assert-= 100 (id-of acc))
    (assert-string=? "nick" (account-nick acc))
    (assert-string=? "F. M. Name" (account-name acc))
    (assert-string=? "00000000" (account-password acc))
    (assert-string=? "name@example.com" (account-mail-address acc))
    (assert-string=? "plain" (account-hash-algorithm acc))
    (assert-string=? "" (account-hash-key acc))))

(let ((p (make-password-to-modify "00000000" "1111111" "11111111"))
      (current-account (make-account "nick" "Full Name" "00000000" "nick@example.com" "plain" "")))
  (id-set! current-account 100)
  (let ((acc (password-to-modify->account p current-account)))
    (assert-= 100 (id-of acc))
    (assert-string=? "nick" (account-nick acc))
    (assert-string=? "Full Name" (account-name acc))
    (assert-string? (account-password acc))
    (assert-string=? "nick@example.com" (account-mail-address acc))
    (assert-string=? "sha-256" (account-hash-algorithm acc))
    (assert-string? (account-hash-key acc))))

(let ((b (string->bib "0000000000000\n0000000000\n題名\nhttp://www.example.com/image\n")))
  (assert-string? (bib-uuid b))
  (assert-string=? "0000000000" (bib-isbn10 b))
  (assert-string=? "0000000000000" (bib-isbn13 b))
  (assert-string=? "題名" (bib-title b))
  (assert-string=? "http://www.example.com/image" (bib-image b)))

(let ((b (string->bib "8888888888888\n8888888888\nタイトル\n")))
  (assert-string? (bib-uuid b))
  (assert-string=? "8888888888" (bib-isbn10 b))
  (assert-string=? "8888888888888" (bib-isbn13 b))
  (assert-string=? "タイトル" (bib-title b))
  (assert-boolean=? #f (bib-image b)))

(let ((ack (make-acknowledgement #f #f #f "This is a comment.")))
  (assert-string=? "This is a comment." (acknowledgement->caption ack)))

(let ((agr (make-agreement #f #f "これはコメントです。")))
  (assert-string=? "これはコメントです。" (agreement->caption agr)))

(let ((rvw (make-review #f "これはテストが含まれている。")))
  (assert-string=? "これはテストが含まれている。" (review->caption rvw)))

(let ((rep (make-report #f #f #f "レポートの題名" #f #f #f)))
  (assert-string=? "レポートの題名" (report->caption rep)))

(define-syntax assert-recent-stuff
  (syntax-rules ()
    ((_ name)
     (let ((ls (name 3)))
       (and (assert-list? ls)
            (for-all
             (lambda (x) (assert-list? x))
             ls))))))

(skip-unless (and mysql-user
                  mysql-password
                  mysql-database
                  (connect "localhost" mysql-user mysql-password mysql-database))
  (assert-recent-stuff recent-acknowledgements)
  (assert-recent-stuff recent-agreements)
  (assert-recent-stuff recent-reports)
  (assert-recent-stuff recent-reviews)
  (assert-recent-stuff recent-revisions)
  (close))

(xunit:report)
