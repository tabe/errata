(doctype transitional)
(html
 (head
  errata-keywords
  (title "Errata")
  (link ((href "/stylesheet/errata.css") (rel "stylesheet") (type "text/css")))
  jquery-scripts
  (script ((type "text/javascript")) preload-script)
  )
 (body
  menu
  errata-logo
  (div ((id "links")) links)
  (div ((id "private")) (lambda (uuid body) body))
  belt
  powered-by-lunula
  powered-by-mathjax
  ))
