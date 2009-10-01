(library (errata rss)
  (export emit
          start
          query)
  (import (only (core) format lookup-process-environment system)
          (rnrs)
          (only (rnrs eval) eval environment)
          (match)
          (ypsilon socket)
          (only (ypsilon ffi) on-freebsd)
          (only (lunula tree) put-tree)
          (only (lunula xml) declaration)
          (lunula rss))

  (define *base-url* "http://errata.fixedpoint.jp")

  (define *extension* "rss")

  (define *temporary-directory* (lookup-process-environment "ERRATA_RSS_TEMPORARY_DIRECTORY"))

  (define *output-directory* (lookup-process-environment "ERRATA_RSS_OUTPUT_DIRECTORY"))

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
    (case (string->symbol category)
      (else
       (let ((tmp (format "~a/~a.~a.tmp" *temporary-directory* category *extension*))
             (dst (format "~a/~a.~a" *output-directory* category *extension*)))
         (call-with-output-file tmp
           (lambda (port)
             (put-tree
              port
              (rss-tree user password database category))))
         (system (format "/usr/bin/install -m 644 ~a ~a" tmp dst))
         (delete-file tmp)))))

  (define (start port-number user password database)
    (let ((socket (make-server-socket port-number)))
      (let loop ((client (socket-accept socket)))
        (call-with-port (socket-port client)
          (lambda (port)
            (let ((bv (get-bytevector-all port)))
              (cond ((eof-object? bv)
                     (shutdown-output-port port))
                    (else
                     (let ((category (utf8->string bv)))
                       (call-with-port (transcoded-port port (make-transcoder (utf-8-codec) (eol-style none)))
                         (lambda (tport)
                           (put-string tport category)
                           (newline tport)
                           (shutdown-output-port tport)))
                       ;;
                       (emit user password database category)
                       ))))))
        (loop (socket-accept socket)))))

  (define (query category)
    (call-with-socket (make-client-socket "localhost" "3002" AF_INET SOCK_STREAM (if on-freebsd AI_ADDRCONFIG (+ AI_V4MAPPED AI_ADDRCONFIG))) ; workaround for FreeBSD 7.x, cf. http://lists.freebsd.org/pipermail/freebsd-bugs/2008-February/028260.html
      (lambda (socket)
        (call-with-port (transcoded-port (socket-port socket) (make-transcoder (utf-8-codec) (eol-style none)))
          (lambda (port)
            (put-string port category)
            (shutdown-output-port port)
            (get-string-all port))))))

)
