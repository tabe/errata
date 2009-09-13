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
       (h2 "利用規約")
       (h3 "はじめに")
       (p "この利用規約では、fixedpoint.jp が本サイト上で提供するサービス(以下「本サービス」)における利用条件について定める。" (br)
          "利用者は本規約にしたがって本サービスを利用するものとする。")
       (p "本サービス内には、本規約以外にFAQや各種文書において、本サービスの利用方法や注意書きが提示されている。" (br)
          "これらも本規約の一部を実質的に構成するものである。")
       (h3 "免責事項")
       (ul
        (li "本サービスを利用したことで利用者または他者に発生した損害について、fixedpoint.jp は責任を負わない。")
        (li "本サービスにおいて開示された情報及び同情報のリンク先が提供するサービスの合法性、道徳性、著作権の許諾の有無、信頼性、正確性について、fixedpoint.jp は責任を負わない。")
        )
       (h3 "禁止事項")
       (p "利用者は本サービスを利用する際、以下のような行為を行ってはならない:")
       (ul 
        (li "犯罪に関わる行為あるいは法令に違反する行為")
        (li "他者が本サービスを利用することを妨害する行為")
        (li "他者になりすます行為")
        )
       (h3 "利用条件")
       (p "利用者は本サービスを利用する際、以下の項目を承諾したものとする:")
       (ul
        (li "正誤表を構成する情報として利用者が送信したテキストおよび画像等の情報は、利用者が" creativecommons-attribution "に基づいて公開するものとする。")
        (li "fixedpoint.jp の独自の判断あるいはシステム障害等により、利用者が送信した情報が失われる、もしくは利用できない状態になる場合がある。")
        )
       (h3 "規約およびサービスの変更")
       (ul
        (li "本規約は予告なしに変更される場合がある。")
        (li "本サービスの内容は予告なしに変更される場合がある。")
        )
       )
  belt
  powered-by-lunula
  ))
