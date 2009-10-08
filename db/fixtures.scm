#!/usr/bin/env ypsilon
#!r6rs

(import (lunula session)
        (lunula mysql)
        (only (errata configuration) mysql-user mysql-password mysql-database)
        (errata))

(connect "localhost" mysql-user mysql-password mysql-database)

;; FIXME

(close)
