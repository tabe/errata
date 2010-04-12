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
  (div ((id "public"))
       (h2 "フィードバック")
       (ul
        (li
         (p "Errata のコードは "
            (a ((href "http://github.com/tabe/errata")) "http://github.com/tabe/errata")
            " で公開しています。"))
        (li
         (p "開発に関しては、Google グループの "
            (a ((href "http://groups.google.com/group/errata")) "Errata")
            " で意見交換できます。"))
        (li
         (p "全般に関しては、tabe at fixedpoint dot jp までメールにてご連絡ください。")
         (p "Key fingerprint = 26F7 2FE0 FB2A 64AA EA87 C96A 4B40 171F 8AFE 4953"))
        )
       )
  belt
  powered-by-lunula
  ))
