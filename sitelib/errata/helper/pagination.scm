(library (errata helper pagination)
  (export with-pagination)
  (import (rnrs)
          (only (core) format)
          (prefix (lunula html) html:)
          (only (srfi :1) take)
          (only (lunula mod_lisp) build-entry-path)
          (only (lunula mysql) lookup-all))

  (define *exlibris-per-page* 5)

  (define-syntax pagination-body
    (syntax-rules ()
      ((_ (entry-name uuid page) exp proc)
       (let ((ls exp))
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

  (define-syntax with-pagination
    (syntax-rules ()
      ((_ (entry-name uuid page) (record-name (reference foreign) ...) param (e ...) proc)
       (pagination-body
        (entry-name uuid page)
        (lookup-all (record-name (reference foreign) ...)
                    param
                    (e ... (offset (* *exlibris-per-page* page)) (limit (+ *exlibris-per-page* 1))))
        proc))))

)
