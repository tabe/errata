#!/usr/bin/env ypsilon
#!r6rs

(import (errata configuration) (xunit))

(assert-string? domain)
(assert-string? mail-address)
(assert-string? url-base)

(skip-unless port-number
  (assert-integer? port-number))

(skip-unless mysql-user
  (assert-string? mysql-user))
(skip-unless mysql-password
  (assert-string? mysql-password))
(skip-unless mysql-database
  (assert-string? mysql-database))

(skip-unless aws-access-key-id
  (assert-string? aws-access-key-id))
(skip-unless aws-secret-access-key
  (assert-string? aws-secret-access-key))

(skip-unless query-port-number
  (assert-integer? query-port-number))

(skip-unless rss-port-number
  (assert-integer? rss-port-number))
(assert-string? rss-temporary-directory)
(skip-unless rss-output-directory
  (assert-string? rss-output-directory))

(report)
