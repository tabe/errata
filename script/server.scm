#!r6rs

(import (rnrs)
        (errata gcrypt)
        (only (errata configuration) port-number mysql-user mysql-password mysql-database)
        (only (errata) connect close start))

(dynamic-wind
    (lambda () (connect "localhost" mysql-user mysql-password mysql-database))
    (lambda () (start (number->string port-number)))
    close)
