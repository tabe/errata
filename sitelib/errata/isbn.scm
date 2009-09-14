(library (errata isbn)
  (export valid-isbn13?
          valid-isbn10?
          valid-isbn?
          isbn10->amazon)
  (import (rnrs)
          (only (core) format)
          (pregexp))

  (define (char->digit c) (- (char->integer c) 48)) ; #\0

  (define (valid-isbn13? str)
    (and (pregexp-match "^[0-9]{13}$" str)
         (let ((digits (map char->digit (string->list str))))
           (= (list-ref digits 12)
              (mod (- 10 (mod (apply + (map * digits '(1 3 1 3 1 3 1 3 1 3 1 3 0))) 10)) 10)))))

  (define (valid-isbn10? str)
    (cond ((pregexp-match "^[0-9]{9}([0-9X])$" str)
           => (lambda (m)
                (let ((check (let ((c (car (string->list (cadr m)))))
                               (case c
                                 ((#\X) 10)
                                 ((#\0) 11)
                                 (else (char->digit c)))))
                      (digits (map char->digit (string->list str))))
                  (= check
                     (- 11 (mod (apply + (map * digits '(10 9 8 7 6 5 4 3 2 0))) 11))))))
          (else #f)))

  (define (valid-isbn? str)
    (cond ((valid-isbn13? str) 13)
          ((valid-isbn10? str) 10)
          (else #f)))

  (define (isbn10->amazon str)
    (format "http://www.amazon.co.jp/exec/obidos/ASIN/~a/errata-22/ref=nosim" str))

)
