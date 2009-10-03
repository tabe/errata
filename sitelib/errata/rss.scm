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
          (lunula rss))

  (define *base-url* "http://errata.fixedpoint.jp")

  (define *extension* "rss")

  (define *temporary-directory* (lookup-process-environment "ERRATA_RSS_TEMPORARY_DIRECTORY"))

  (define *output-directory* (lookup-process-environment "ERRATA_RSS_OUTPUT_DIRECTORY"))

  (define *mailbox* (make-mailbox))

  (define (make-rss category set entry-proc item-proc)
    (list
     (declaration 1.0 "UTF-8")
     (rdf:RDF
      (channel
       ((rdf:about (format "~a/~a.~a" *base-url* category *extension*)))
       (title category)
       (link (string-append *base-url* "/"))
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

  (define (emit user password database category)
    (let ((tmp (format "~a/~a.~a.~d" *temporary-directory* category *extension* (random-integer 100)))
          (dst (format "~a/~a.~a" *output-directory* category *extension*)))
      (call-with-output-file tmp
        (lambda (port)
          (put-tree
           port
           (rss-tree user password database category))))
      (system (format "/usr/bin/install -m 644 ~a ~a" tmp dst))
      (delete-file tmp)))

  (define (server port-number)
    (let ((socket (make-server-socket port-number)))
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

  (define (start port-number user password database)
    (spawn*
     (lambda () (server port-number))
     (lambda (x)
       (log:info "rss> ~a" x)
       (shutdown-mailbox *mailbox*)))
    (let loop ((category (recv *mailbox*)))
      (log:info "rss> emit ~a" category)
      (emit user password database category)
      (loop (recv *mailbox*))))

  (define-syntax query
    (syntax-rules ()
      ((_ category0 category1 ...)
       (let ((str (fold-left
                   (lambda (s x) (format "~a ~a" s x))
                   (symbol->string 'category0)
                   '(category1 ...))))
         (call-with-socket (make-client-socket "localhost" "3002" AF_INET SOCK_STREAM (if on-freebsd AI_ADDRCONFIG (+ AI_V4MAPPED AI_ADDRCONFIG))) ; workaround for FreeBSD 7.x, cf. http://lists.freebsd.org/pipermail/freebsd-bugs/2008-February/028260.html
           (lambda (socket)
             (call-with-port (transcoded-port (socket-port socket) (make-transcoder (utf-8-codec) (eol-style none)))
               (lambda (port)
                 (put-string port str)
                 (shutdown-output-port port)
                 (get-string-all port)))))))))

)
