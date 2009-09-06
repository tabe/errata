(library (errata isbn)
  (export valid-isbn13?
          valid-isbn10?
          valid-isbn?
          isbn10->amazon)
  (import (rnrs)
          (only (core) format)
          (pregexp))

  (define (valid-isbn13? str)
    (pregexp-match "^[0-9]{13}$" str))

  (define (valid-isbn10? str)
    (pregexp-match "^[0-9]{9}[0-9X]$" str))

  (define (valid-isbn? str)
    (cond ((valid-isbn13? str) 13)
          ((valid-isbn10? str) 10)
          (else #f)))

  (define (isbn10->amazon str)
    (format "http://www.amazon.co.jp/exec/obidos/ASIN/~a/errata-22/ref=nosim" str))

)
