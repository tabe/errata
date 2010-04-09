(library (errata query)
  (export start
          query-image)
  (import (core)
          (rnrs)
          (only (srfi :19) current-date date->string)
          (only (ypsilon ffi) on-freebsd)
          (ypsilon socket)
          (prefix (uri) uri:)
          (prefix (http client) http:)
          (sxml ssax)
          (prefix (only (lunula hmac) sha-256) hmac:)
          (prefix (only (lunula log) info) log:)
          (only (errata configuration) aws-access-key-id aws-secret-access-key query-port-number))

(define *aws-authority* "ecs.amazonaws.jp")
(define *aws-path* "/onca/xml")
(define *aws-parameters*
  `((Service . "AWSECommerceService")
    (AWSAccessKeyId . ,aws-access-key-id)
    (Operation . "ItemLookup")
    ;;(ItemId . "9784756134141")
    ;;(ResponseGroup . "ItemAttributes,Offers,Images,Reviews")
    (ResponseGroup . "ItemAttributes,Images,Reviews")
    (IdType . "ISBN")
    (SearchIndex . "Books")
    (Version . "2009-07-01")
    ))

(define (parameter<? x y)
  (string<? (symbol->string (car x)) (symbol->string (car y))))

(define (generate-query parameters)
  (call-with-string-output-port
   (lambda (port)
     (define (put-k=v kv)
       (put-datum port (car kv))
       (display #\= port)
       (put-string port (uri:encode-string (cdr kv))))
     (let loop ((rest (list-sort parameter<? parameters)))
       (case (length rest)
         ((0) #f)
         ((1) (put-k=v (car rest)))
         (else
          (put-k=v (car rest))
          (display #\& port)
          (loop (cdr rest))))))))

(define (generate-signature isbn parameters)
  (let ((str (format "GET~%~a~%~a~%~a"
                     *aws-authority*
                     *aws-path*
                     (generate-query (cons `(ItemId . ,isbn) parameters)))))
    (hmac:sha-256 aws-secret-access-key (string->utf8 str))))

;; (let ((signature (generate-signature)))
;;   (display signature)
;;   (newline))

(define-syntax define-finder
  (syntax-rules ()
    ((_ name proc)
     (define (name tree)
       (cond ((null? tree)
              #f)
             ((symbol? (car tree))
              (if (eq? 'name (car tree))
                  (proc tree)
                  (name (cdr tree))))
             ((pair? (car tree))
              (or (name (car tree))
                  (name (cdr tree))))
             (else
              (name (cdr tree))))))))

(define-finder aws:MediumImage (lambda (tree) (cadr (cadr tree))))
(define-finder aws:Title (lambda (tree) (cadr tree)))
(define-finder aws:ISBN (lambda (tree) (cadr tree)))
(define-finder aws:EAN (lambda (tree) (cadr tree)))

(define (get-image isbn proc)
  (let* ((parameters (cons `(Timestamp . ,(date->string (current-date 0) "~5Z")) *aws-parameters*))
         (signature (generate-signature isbn parameters))
         (url (format "http://~a~a?~a"
                     *aws-authority*
                     *aws-path*
                     (generate-query `((ItemId . ,isbn) (Signature . ,signature) ,@parameters)))))
    (log:info "url: ~a~%" url)
    (call-with-values
        (lambda () (http:get url))
      (lambda (status headers body)
        (case status
          ((200)
           (log:info "~a" (utf8->string body))
           ;;body
           (call-with-port (open-string-input-port (utf8->string body))
             (lambda (iport)
               (let ((tree (ssax:xml->sxml iport '((aws . "http://webservices.amazon.com/AWSECommerceService/2009-07-01")))))
                 (proc (aws:EAN tree)
                       (aws:ISBN tree)
                       (aws:Title tree)
                       (aws:MediumImage tree)))))
           )
          (else
           (proc #f #f #f #f)))))))

(define (start)
  (let ((socket (make-server-socket (number->string query-port-number))))
    (let loop ((client (socket-accept socket)))
      (call-with-port (socket-port client)
        (lambda (port)
          (let ((bv (get-bytevector-all port)))
            (cond ((eof-object? bv)
                   (shutdown-output-port port))
                  (else
                   (let ((isbn (utf8->string bv)))
                     (get-image
                      isbn
                      (lambda (isbn13 isbn10 title url)
                        (if (and isbn13 isbn10 title)
                            (call-with-port (transcoded-port port (make-transcoder (utf-8-codec) (eol-style none)))
                              (lambda (tport)
                                (put-string tport isbn13)
                                (newline tport)
                                (put-string tport isbn10)
                                (newline tport)
                                (put-string tport title)
                                (newline tport)
                                (when url
                                  (put-string tport url)
                                  (newline tport))
                                (flush-output-port tport)
                                (shutdown-output-port tport)))
                            (shutdown-output-port port))))))))))
      (usleep 1000000)
      (loop (socket-accept socket)))))

(define (query-image isbn)
  (call-with-socket (make-client-socket "localhost"
                                        (number->string query-port-number)
                                        AF_INET
                                        SOCK_STREAM
                                        (if on-freebsd AI_ADDRCONFIG (+ AI_V4MAPPED AI_ADDRCONFIG))) ; workaround for FreeBSD 7.x, cf. http://lists.freebsd.org/pipermail/freebsd-bugs/2008-February/028260.html
    (lambda (socket)
      (call-with-port (transcoded-port (socket-port socket) (make-transcoder (utf-8-codec) (eol-style none)))
        (lambda (port)
          (put-string port isbn)
          (shutdown-output-port port)
          (get-string-all port))))))

)
