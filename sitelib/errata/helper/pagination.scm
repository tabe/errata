(library (errata helper pagination)
  (export with-pagination)
  (import (rnrs)
          (only (core) format)
          (prefix (lunula html) html:)
          (only (srfi :1) take)
          (only (lunula mod_lisp) build-entry-path))

  (define *exlibris-per-page* 5)

  (define (pagination-condition page)
    (format " ORDER BY id DESC LIMIT ~d OFFSET ~d" (+ *exlibris-per-page* 1) (* *exlibris-per-page* page)))

  (define-syntax with-pagination
    (syntax-rules ()
      ((_ (entry-name uuid page) (e0 ...) proc)
       (let ((ls (e0 ... (pagination-condition page))))
         (append
          (map
           proc
           (if (< *exlibris-per-page* (length ls))
               (take ls *exlibris-per-page*)
               ls))
          (if (< 0 page)
              (html:form ((action (build-entry-path 'entry-name uuid)))
                         (html:input ((type "hidden") (name "page") (value (- page 1))))
                         (html:input ((type "submit") (value (html:escape-string "<<")))))
              '())
          (if (< *exlibris-per-page* (length ls))
              (html:form ((action (build-entry-path 'entry-name uuid)))
                         (html:input ((type "hidden") (name "page") (value (+ page 1))))
                         (html:input ((type "submit") (value (html:escape-string ">>")))))
              '()))))))

)
