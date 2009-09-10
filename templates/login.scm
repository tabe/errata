(doctype transitional)
(html
 (head
  (title "Errata")
  (link ((href "/stylesheet/errata.css") (rel "stylesheet") (type "text/css")))
  (script ((type "text/javascript") (src "/javascript/jquery-1.3.2.min.js")))
  (script ((type "text/javascript") (src "/javascript/jquery.corner.js")))
  (script ((type "text/javascript")) preload-script)
  )
 (body
  menu
  errata-logo
  (div ((id "links")) links)
  (div ((id "public"))
       (lambda (uuid body) body)
       (p ((style "font-size:small;")) "または、" (a ((href (build-entry-path 'forgot-password))) "パスワードを忘れた")))
  belt
  powered-by-lunula
  ))