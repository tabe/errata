#!/usr/bin/env ypsilon
#!r6rs

(import (rnrs (6))
        (only (errata isbn) tolerant-isbn? isbn-strip)
        (errata query))

(when (< (length (command-line)) 2)
  (display "Usage: ")
  (display (car (command-line)))
  (display " [ISBN]")
  (newline)
  (exit #f))

(let ((isbn- (cadr (command-line))))
  (cond ((tolerant-isbn? isbn-)
         (display (query-image (isbn-strip isbn-)))
         (exit))
        (else
         (display "ISBN expected, but got: ")
         (display isbn-)
         (newline)
         (exit #f))))
