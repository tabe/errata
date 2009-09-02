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
  (div ((id "links")) links)
  (div ((id "private")) (lambda (uuid body) body))
  powered-by-lunula
  ))
