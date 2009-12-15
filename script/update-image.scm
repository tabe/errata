#!/usr/bin/env ypsilon
#!r6rs

(import (rnrs (6))
        (only (core) usleep)
        (match)
        (prefix (lunula log) log:)
        (only (lunula mysql) connect close lookup-all save)
        (only (errata configuration) mysql-user mysql-password mysql-database)
        (only (errata isbn) valid-isbn10?)
        (errata model)
        (only (errata query) query-image))

(define *roll* 1000)

(define (run)
  (let ((tuples (lookup-all (bib) ((bib (image #f))))))
    (unless (null? tuples)
      (call/cc
       (lambda (cont)
         (for-each
          (lambda (tuple)
            (match tuple
              ((b)
               (let ((isbn (bib-isbn10 b)))
                 (when (valid-isbn10? isbn)
                   (let* ((info (guard (e
                                        ((i/o-error? e)
                                         (log:info "image-agent> ~s" e)
                                         (cont #f)))
                                  (query-image isbn)))
                          (new-b (string->bib info)))
                     (cond ((bib-image new-b)
                            => (lambda (url)
                                 (bib-image-set! b url)
                                 ;; Note that possibly the bib no longer exists, thus new one will be created
                                 (save b)))))))
               (usleep *roll*))))
          tuples))))))

(dynamic-wind
    (lambda () (connect "localhost" mysql-user mysql-password mysql-database))
    (lambda () (run))
    close)
