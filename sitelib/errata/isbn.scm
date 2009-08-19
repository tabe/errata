(library (errata isbn)
  (export valid-isbn?)
  (import (rnrs)
          (pregexp))

  (define (valid-isbn? str)
    (cond ((pregexp-match "^[0-9]{9}[0-9X]$" str)
           #t)
          ((pregexp-match "^[0-9]{13}$" str)
           #t)
          (else #f)))

)
