(library (errata rss)
  (export emit
          start
          query)
  (import (only (core) format lookup-process-environment system)
          (rnrs)
          (only (rnrs eval) eval environment)
          (only (srfi :13) string-tokenize)
          (only (srfi :27) random-integer)
          (match)
          (only (ypsilon concurrent) make-mailbox recv send shutdown-mailbox spawn*)
          (ypsilon socket)
          (only (ypsilon ffi) on-freebsd)
          (prefix (lunula log) log:)
          (only (lunula tree) put-tree)
          (only (lunula xml) declaration)
          (lunula rss)
          (only (errata configuration)
                rss-port-number rss-output-directory rss-temporary-directory
                mysql-user mysql-password mysql-database
                url-base))

  (define *extension* "rss")

  (define *mailbox* (make-mailbox))

  (define (make-rss category set entry-proc item-proc)
    (list
     (declaration 1.0 "UTF-8")
     (rdf:RDF
      (channel
       ((rdf:about (format "~a/~a.~a" url-base category *extension*)))
       (title category)
       (link (string-append url-base "/"))
       (description (format "Errata: ~a" category))
       (items
        (rdf:Seq
         (map entry-proc set))))
      (map item-proc set))))

  (define (rss-tree user password database category)
    (let ((env (environment `(errata rss ,(string->symbol category)))))
      (make-rss
       category
       (eval (list 'feed-set user password database) env)
       (eval 'feed-entry env)
       (eval 'feed-item env))))

  (define (emit category)
    (let ((tmp (format "~a/~a.~a.~d" rss-temporary-directory category *extension* (random-integer 100)))
          (dst (format "~a/~a.~a" rss-output-directory category *extension*)))
      (call-with-output-file tmp
        (lambda (port)
          (put-tree
           port
           (rss-tree mysql-user mysql-password mysql-database category))))
      (system (format "/usr/bin/install -m 644 ~a ~a" tmp dst))
      (delete-file tmp)))

  (define (server)
    (let ((socket (make-server-socket (number->string rss-port-number))))
      (let loop ((client (socket-accept socket)))
        (call-with-port (socket-port client)
          (lambda (port)
            (let ((bv (get-bytevector-all port)))
              (cond ((eof-object? bv)
                     (shutdown-output-port port))
                    (else
                     (let* ((str (utf8->string bv))
                            (categories (string-tokenize str)))
                       (for-each
                        (lambda (category)
                          (spawn*
                           (lambda () (send *mailbox* category 3000))
                           (lambda (x)
                             (when (condition? x)
                               (log:info "rss> ~a" x)))))
                        categories)
                       (call-with-port (transcoded-port port (make-transcoder (utf-8-codec) (eol-style none)))
                         (lambda (tport)
                           (put-string tport str)
                           (newline tport)
                           (shutdown-output-port tport)))
                       ))))))
        (loop (socket-accept socket)))))

  (define (start)
    (spawn*
     server
     (lambda (x)
       (log:info "rss> ~a" x)
       (shutdown-mailbox *mailbox*)))
    (let loop ((category (recv *mailbox*)))
      (log:info "rss> emit ~a" category)
      (emit category)
      (loop (recv *mailbox*))))

  (define-syntax query
    (syntax-rules ()
      ((_ category0 category1 ...)
       (let ((str (fold-left
                   (lambda (s x) (format "~a ~a" s x))
                   (symbol->string 'category0)
                   '(category1 ...))))
         (call-with-socket (make-client-socket "localhost"
                                               (number->string rss-port-number)
                                               AF_INET
                                               SOCK_STREAM
                                               (if on-freebsd AI_ADDRCONFIG (+ AI_V4MAPPED AI_ADDRCONFIG))) ; workaround for FreeBSD 7.x, cf. http://lists.freebsd.org/pipermail/freebsd-bugs/2008-February/028260.html
           (lambda (socket)
             (call-with-port (transcoded-port (socket-port socket) (make-transcoder (utf-8-codec) (eol-style none)))
               (lambda (port)
                 (put-string port str)
                 (shutdown-output-port port)
                 (get-string-all port)))))))))

)
