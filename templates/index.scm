(doctype transitional)
(html
 (head
  errata-keywords
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
       (h2 "Errata って何?")
       (p "書籍などの正誤表を共有するためのサービスです。現時点ではベータ版です。")
       (h2 "Errata で何ができるの?")
       (p "誰でも公開されている正誤表を見ることができます。")
       (p "さらに利用者として登録された方は")
       (ul
        (li "蔵書を登録して管理する")
        (li "正誤情報を報告して正誤表を編集する")
        (li "レビューやコメントをつける")
        (li "それらを" creativecommons-attribution "の下で公開する")
        )
       (p "ことができます。ご利用は無料です。")
       (h2 "Errata についてさらに詳しく")
       (p (a ((href (lambda args (build-entry-path 'faq (car args))))) "FAQ") " をご覧ください。")
       )
  belt
  powered-by-lunula
  ))
