(library (errata configuration)
  (export domain
          mail-address
          url-base
          aws-access-key-id
          aws-secret-access-key
          query-port-number
          rss-port-number
          rss-temporary-directory
          rss-output-directory)
  (import (only (rnrs) string-append)
          (lunula configuration))

  (define-configuration domain "errata.fixedpoint.jp")
  (define-configuration mail-address "errata@fixedpoint.jp")
  (define-configuration url-base (string-append "http://" domain))

  (define-configuration aws-access-key-id)
  (define-configuration aws-secret-access-key)

  (define-configuration query-port-number)

  (define-configuration rss-port-number)
  (define-configuration rss-temporary-directory "/tmp")
  (define-configuration rss-output-directory)

)
