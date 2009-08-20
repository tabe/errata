(library (errata helper)
  (export links
          public-exlibris
          shelf-window)
  (import (except (rnrs) div)
          (only (lunula gettext) __)
          (only (lunula mod_lisp) entry-paths build-entry-path)
          (lunula mysql)
          (lunula html)
          (errata model))

  (define (links uuid . _)
    (ul
     (map
      (lambda (path)
        (if (string? uuid)
            (li (a ((href (string-append path "?" uuid))) path))
            (li (a ((href path)) path))))
      (entry-paths))))

  (define (public-exlibris . args)
    (map
     (lambda (pub)
       (cond ((lookup exlibris (publicity-exlibris-id pub))
              => (lambda (ex)
                   (cond ((lookup revision (exlibris-revision-id ex))
                          => (lambda (rev)
                               (cond ((lookup bib (revision-bib-id rev))
                                      => (lambda (b)
                                           (p (bib-title b))))
                                     (else '()))))
                         (else '()))))
             (else '())))
     (lookup-all publicity '())))

  (define (exlibris-window uuid b r ex)
    (let ((id (exlibris-id ex)))
      (div
       (h3 (bib-title b))
       (image ((src (bib-image b))))
       (p ((style "color:#555;")) "ISBN: " (bib-isbn b))
       (form ((action (build-entry-path 'modify-exlibris uuid))
              (method "POST"))
             (div ((style "color:#222;")) "リビジョン情報: " (revision-name r) "(" (revision-revised-at r) ")&nbsp;"
                  (input ((type "hidden") (name "id") (value id)))
                  (input ((type "submit") (value (__ modify-exlibris))))))
       (form ((action (build-entry-path 'edit-review uuid))
              (method "POST"))
             (div "レビュー:&nbsp;"
                  (input ((type "hidden") (name "id") (value id)))
                  (input ((type "submit") (value (__ edit-review))))
                  (cond ((lookup review `((exlibris-id ,id)))
                         => (lambda (rvw) (div ((class "corner") (style "background-color: #ecfc71;")) (pre (review-body rvw)))))
                        (else "(なし)"))))
       (cond ((lookup publicity `((exlibris-id ,id)))
              => (lambda (pub)
                   (div
                    (form ((action (build-entry-path 'hide-exlibris uuid))
                           (method "POST"))
                          (input ((type "hidden") (name "id") (value (publicity-id pub))))
                          (input ((type "submit") (value (__ hide-exlibris))))))))
             (else
              (div
               (form ((action (build-entry-path 'share-exlibris uuid))
                      (method "POST"))
                     (input ((type "hidden") (name "id") (value id)))
                     (input ((type "submit") (value (__ share-exlibris))))))))
       (div
        (form ((action (build-entry-path 'put-off uuid))
               (method "POST"))
              (input ((type "hidden") (name "id") (value id)))
              (input ((type "submit") (value (__ put-off))))))
       (hr ((style "color:#999;"))))))

  (define (shelf-window uuid id)
    (map
     (lambda (ex)
       (let ((r (lookup revision (exlibris-revision-id ex))))
         (if r
             (let ((b (lookup bib (revision-bib-id r))))
               (if r (exlibris-window uuid b r ex) "??"))
             "?")))
     (lookup-all exlibris `((account-id ,id)))))

)
