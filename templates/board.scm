(doctype transitional)
(html
 (head
  (title "Errata")
  (link ((href "/stylesheet/errata.css") (rel "stylesheet") (type "text/css")))
  (script ((type "text/javascript") (src "/javascript/jquery-1.3.2.min.js")))
  (script ((type "text/javascript") (src "/javascript/jquery.corner.js")))
  (script ((type "text/javascript"))
          "$(document).ready(function() {$('div#links').corner();$('#public').corner();});")
  )
 (body
  (div ((id "links")) links)
  (div ((id "public")) public-revisions)
  powered-by-lunula
  ))
