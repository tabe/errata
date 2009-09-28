(library (errata page)
  (export page<?)
  (import (rnrs))

  (define (page<? s0 s1)
    (assert (string? s0))
    (assert (string? s1))
    (cond ((string->number s0)
           => (lambda (n0)
                (cond ((string->number s1)
                       => (lambda (n1) (< n0 n1)))
                      (else #f))))
          ((string->number s1)
           #t)
          (else
           (string<? s0 s1))))

)
