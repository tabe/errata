(doctype transitional)
(html
 (head
  (title "Errata")
  (link ((href "/stylesheet/errata.css") (rel "stylesheet") (type "text/css")))
  (script ((type "text/javascript") (src "/javascript/jquery-1.3.2.min.js")))
  (script ((type "text/javascript") (src "/javascript/jquery.corner.js")))
  (script ((type "text/javascript"))
          "$(function() {$('div#links').corner();$('#menus').corner();});")
  )
 (body
  (div ((id "links")) links)
  (div ((id "menus")) (lambda (uuid body) body))
  (div ((id "bottom")) "generated by template 'base'")
  ))
