#!r6rs

(import (rnrs)
        (errata configuration)
        (errata rss recent-acknowledgements)
        (xunit))

(skip-unless (and mysql-user mysql-password mysql-database)
  (let ((set (feed-set mysql-user mysql-password mysql-database)))
    (assert-list? set)
    (for-each
     (lambda (tuple)
       (assert-list? (feed-entry tuple)))
     set)
    (for-each
     (lambda (tuple)
       (assert-list? (feed-item tuple)))
     set)))

(report)
