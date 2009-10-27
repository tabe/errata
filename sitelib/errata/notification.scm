(library (errata notification)
  (export make-notification
          notification
          notification?
          notification-account-id
          notification-subject
          notification-body
          notification->url
          notify)
  (import (rnrs)
          (only (srfi :28) format)
          (only (lunula mysql) call-with-mysql execute save)
          (only (lunula persistent-record) define-persistent-record-type maybe-id persistent-protocol)
          (only (errata url) record->fragment report->path)
          (errata model))

  (define-persistent-record-type notification
    (fields account-id category subject body path fragment)
    (protocol
     (persistent-protocol
      (lambda (p)
        (lambda (account-id category subject body path fragment)
          (p (maybe-id account-id)
             category
             subject
             body
             path
             fragment))))))

  (define notification->url
    (case-lambda
     ((nt) (string-append
            (notification-path nt)
            "#"
            (notification-fragment nt)))
     ((nt uuid)
      (if (string? uuid)
          (string-append
           (notification-path nt)
           "?"
           uuid
           "#"
           (notification-fragment nt))
          (notification->url nt)))))

  (define-syntax drop-stale-notification
    (syntax-rules ()
      ((_ mysql)
       (execute mysql "DELETE FROM notification WHERE DATE_ADD(created_at, INTERVAL 7 DAY) < current_timestamp()"))))

  (define-syntax notify
    (syntax-rules (acknowledgement agreement)
      ((_ account-id (acknowledgement rep ack))
       (let ((nt (make-notification
                  account-id
                  "acknowledgement"
                  (format "報告\"~a\"にコメントがついています。" (report->caption rep))
                  (acknowledgement->caption ack)
                  (report->path rep)
                  (record->fragment ack))))
         (call-with-mysql
          (lambda (mysql)
            (drop-stale-notification mysql)
            (save mysql nt)))))
      ((_ account-id (agreement rep agr))
       (let ((nt (make-notification
                  account-id
                  "agreement"
                  (format "報告\"~a\"に同意がついています。" (report->caption rep))
                  (agreement->caption agr)
                  (report->path rep)
                  (record->fragment agr))))
         (call-with-mysql
          (lambda (mysql)
            (drop-stale-notification mysql)
            (save mysql nt)))))))

)
