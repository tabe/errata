(library (errata calendar)
  (export ad->japanese-era
          datetime->date
          datetime->y/m/d)
  (import (rnrs)
          (only (srfi :19) date->string string->date))

  (define (ad->japanese-era year)
    (define (integer->y i)
      (string-append 
       (if (= i 1)
           "元"
           (number->string i))
       "年"))
    (assert (fixnum? year))
    (cond ((<= 1989 year)
           (string-append "平成" (integer->y (- year 1988))))
          ((<= 1926 year)
           (string-append "昭和" (integer->y (- year 1925))))
          ((<= 1912 year)
           (string-append "大正" (integer->y (- year 1911))))
          ((<= 1868 year)
           (string-append "明治" (integer->y (- year 1867))))
          (else
           (error 'ad->japanese-era (number->string year)))))

  (define (datetime->date str)
    (guard (e (else #f))
      (string->date str "~Y-~m-~d ~H:~M:~S")))

  (define (date->y/m/d date)
    (date->string date "~Y/~m/~d"))

  (define (datetime->y/m/d str)
    (cond ((datetime->date str) => date->y/m/d)
          (else #f)))

)
