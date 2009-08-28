(library (errata isbn)
  (export valid-isbn?
          isbn10->amazon)
  (import (rnrs)
          (only (core) format)
          (pregexp))

  (define (valid-isbn? str)
    (cond ((pregexp-match "^[0-9]{9}[0-9X]$" str)
           10)
          ((pregexp-match "^[0-9]{13}$" str)
           13)
          (else #f)))

  (define (isbn10->amazon str)
    (format "http://www.amazon.co.jp/exec/obidos/ASIN/~a/errata-22/ref=nosim" str))

)
