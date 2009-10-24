#!r6rs

(import (rnrs)
        (only (lunula mysql) close connect)
        (only (errata configuration) mysql-user mysql-password mysql-database)
        (errata model)
        (only (xunit) assert-list? skip-unless)
        (rename (xunit) (report xunit:report)))

(let ((ack (make-acknowledgement #f #f #f "This is a comment.")))
  (assert-string=? "This is a comment." (acknowledgement->caption ack)))

(let ((agr (make-agreement #f #f "これはコメントです。")))
  (assert-string=? "これはコメントです。" (agreement->caption agr)))

(let ((rvw (make-review #f "これはテストが含まれている。")))
  (assert-string=? "これはテストが含まれている。" (review->caption rvw)))

(let ((rep (make-report #f #f #f "レポートの題名" #f #f)))
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
