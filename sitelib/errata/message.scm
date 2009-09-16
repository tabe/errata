(library (errata message)
  (export)
  (import (lunula gettext))

  (gettext

   ;; fields
   (new-account-nick (en "nick")
                     (ja "ニックネーム"))
   (new-account-name (en "name")
                     (ja "名前"))
   (new-account-password (en "password")
                         (ja "パスワード"))
   (new-account-password-re (en "password-re")
                            (ja "パスワード(再入力)"))
   (new-account-mail-address (en "mail address")
                             (ja "メールアドレス"))
   (account-to-modify-name (en "name")
                           (ja "名前"))
   (account-to-modify-current-password (en "current password")
                                       (ja "現在のパスワード"))
   (account-to-modify-new-password (en "new password")
                                   (ja "新しいパスワード"))
   (account-to-modify-new-password-re (en "new password-re")
                                      (ja "新しいパスワード(再入力)"))
   (account-to-modify-mail-address (en "mail address")
                                   (ja "メールアドレス"))
   (account-to-login-nick (en "nick")
                          (ja "ニックネーム"))
   (account-to-login-password (en "password")
                              (ja "パスワード"))
   (forgotten-account-mail-address (en "mail address")
                                   (ja "メールアドレス"))
   (password-reset-password (en "new password")
                            (ja "新しいパスワード"))
   (password-reset-password-re (en "new password-re")
                               (ja "新しいパスワード(再入力)"))
   (confirmation-ok (en "OK?")
                    (ja "OK?"))
   (new-exlibris-title (en "title")
                       (ja "タイトル"))
   (new-exlibris-isbn (en "ISBN")
                      (ja "ISBN"))
   (revision-name (en "name")
                  (ja "名前"))
   (revision-revised-at (en "revised at")
                        (ja "改訂日時"))
   (review-body (en "Body")
                (ja "レビュー本文"))
   (quotation-page (en "Page")
                   (ja "ページ"))
   (quotation-position (en "Position")
                       (ja "位置"))
   (quotation-body (en "Quotation's Body")
                   (ja "引用本文"))
   (correction-body (en "Correction's Body")
                    (ja "訂正本文"))
   (report-to-modify-subject (en "Subject")
                             (ja "題名"))
   (report-to-modify-page (en "Page")
                          (ja "ページ"))
   (report-to-modify-position (en "Position")
                              (ja "位置"))
   (report-to-modify-quotation-body (en "Quotation's Body")
                                    (ja "引用本文"))
   (report-to-modify-correction-body (en "Correction's Body")
                                     (ja "訂正本文"))
   (acknowledgement-sign (en "Sign")
                         (ja "引用部分が誤りだということに"))
   (acknowledgement-comment (en "Comment")
                            (ja "コメント"))
   (agreement-comment (en "Comment")
                      (ja "コメント"))

   ;; messages
   (password-is-blank (en "password is blank.")
                      (ja "パスワードが空です。"))
   (password-too-short (en "password is too short.")
                       (ja "パスワードが短いです。"))
   (password-too-long (en "password is too long.")
                      (ja "パスワードが規定の長さを超えています。"))
   (password-differs-from-re (en "The first password differs from the second.")
                             (ja "パスワードが再入力したものと異なります。"))
   (nick-is-blank (en "nick is blank.")
                  (ja "ニックネームが空です。"))
   (nick-too-long (en "nick is too long.")
                  (ja "ニックネームが規定の長さを超えています。"))
   (nick-already-used (en "nick is already used.")
                      (ja "ニックネームは既に使用されています。"))
   (nick-invalid-char (en "nick contains invalid characters.")
                      (ja "ニックネームに使えない文字が含まれています。"))
   (name-is-blank (en "name is blank.")
                  (ja "名前が空です。"))
   (name-too-long (en "name is too long.")
                  (ja "名前が規定の長さを超えています。"))
   (mail-address-is-blank (en "mail address is blank.")
                          (ja "メールアドレスが空です。"))
   (mail-address-too-short (en "mail address is too short.")
                           (ja "メールアドレスが規定の長さに達していません。"))
   (mail-address-too-long (en "mail address is too long.")
                          (ja "メールアドレスが規定の長さを超えています。"))
   (does-not-exist (en "authentication failed.")
                   (ja "認証に失敗しました。"))
   (title-too-long (en "title is too long.")
                   (ja "タイトルが規定の長さを超えています。"))
   (body-is-blank (en "body is blank.")
                  (ja "本文が空です。"))
   (body-too-long (en "body is too long.")
                  (ja "本文が規定の長さを超えています。"))
   (subject-is-blank (en "subject is blank.")
                     (ja "題名が空です。"))
   (subject-too-long (en "subject is too long.")
                     (ja "題名が規定の長さを超えています。"))
   (page-is-blank (en "page is blank.")
                  (ja "ページが空です。"))
   (page-too-long (en "page is too long.")
                  (ja "ページが規定の長さを超えています。"))
   (position-is-blank (en "position is blank.")
                      (ja "位置が空です。"))
   (position-too-long (en "position is too long.")
                      (ja "位置が規定の長さを超えています。"))
   (comment-is-blank (en "comment is blank.")
                     (ja "コメントが空です。"))
   (comment-too-long (en "comment is too long.")
                     (ja "コメントが規定の長さを超えています。"))
   (submit (en "submit")
           (ja "送信"))
   (cancel (en "cancel")
           (ja "キャンセル"))
   (hmm-an-error-occurred (en "Hmm ... an error occurred.")
                          (ja "残念ながら ... エラーが発生しました。"))
   (you-have-already-logged-in (en "You have already logged in!")
                               (ja "既にログインしています。"))
   (you-can-create-your-own-account (en "You can create your own account:")
                                    (ja "以下の情報を入力してアカウントを作成できます:"))
   (we-have-sent-confirmation-message-to-you (en "We have sent the confirmation message to you.")
                                             (ja "アカウント作成についての確認のメールを送信しました。作成を完了するにはメールの内容にしたがってください。"))
   (now-you-have-your-own-account (en "Now you have your own account!")
                                  (ja "アカウントができました。"))
   (your-account-has-been-updated (en "Your account has been updated.")
                                  (ja "アカウントが更新されました。"))
   (let-me-know-your-mail-address (en "Let me know your mail address:")
                                  (ja "アカウントのメールアドレスを入力してください:"))
   (we-have-sent-message-to-you (ja "We have sent the message to you.")
                                (ja "メッセージをメールを送信しました。メールの内容にしたがってください。"))
   (specify-new-password (en "Specify new password:")
                         (ja "新しいパスワードを指定してください:"))
   (your-password-updated (en "Your password has been updated.")
                          (ja "パスワードが更新されました。"))
   (are-you-sure-to-cancel-account? (en "Are you sure to cancel your account?")
                                    (ja "アカウントを解除するとこれまでに登録されたデータが利用できなくなります。アカウントを解除しますか?"))
   (now-you-have-logged-in (en "Now you have logged in!")
                           (ja "ログインしました。"))
   (now-you-have-logged-out (en "Now you have logged out!")
                            (ja "ログアウトしました。"))
   (now-you-have-left-errata (en "Now you have left Errata. Thanks for your favor.")
                             (ja "Errata のアカウントは解除されました。ご利用ありがとうございました。"))
   (of-course-we-know-you-are-kidding (en "Of course we know you are kidding :)")
                                      (ja "アカウントの解除をキャンセルしました。"))
   (please-input-title-or-isbn (en "Please input title or ISBN.")
                               (ja "タイトルまたは ISBN を入力してください。"))
   (is-this-content-ok? (en "Is this content OK?")
                        (ja "この内容でよろしいですか?"))
   (please-retry (en "Please check your input and retry.")
                 (ja "入力内容を確認して再度入力してください。"))
   (specify-revision (en "Specify a concerned revision:")
                     (ja "該当する改訂情報を指定してください:"))
   (choose-a-revision (en "Choose a concerned revision:")
                      (ja "該当する改訂情報を選んでください:"))
   (or-specify-revision (en "Or, specify another revision:")
                        (ja "もしくは、新たな改訂情報を指定してください:"))
   (are-you-sure-to-put-off-this-one? (en "Are you sure to put off this one?")
                                      (ja "書棚から外すと報告などの関連するデータも利用できなくなります。この蔵書を書棚から外しますか?"))
   (you-have-put-it-off (en "You have put it off.")
                        (ja "この蔵書を書棚から外しました。"))
   (are-you-sure-to-hide-this-one? (en "Are you sure to hide this one?")
                                   (ja "隠すと報告などの関連するデータが非公開になります。この蔵書を隠しますか?"))
   (are-you-sure-to-drop-report? (en "Are you sure to drop report?")
                                 (ja "報告を削除するとコメントなどの関連するデータも利用できなくなります。この報告を削除しますか?"))

   (ISBN (en "ISBN")
         (ja "ISBN"))
   (Revision (en "Revision")
             (ja "改訂情報"))
   (Review (en "Review")
           (ja "レビュー"))
   (Table (en "Table")
          (ja "正誤表"))
   (Detail (en "Detail")
           (ja "詳細"))
   (Quotation (en "Quotation")
              (ja "誤(引用)"))
   (Correction (en "Correction")
               (ja "正(訂正)"))
   (to-board (en "To Board")
             (ja "書誌一覧へ"))
   (to-desk (en "To Desk")
            (ja "デスクへ"))
   (to-detail (en "To Detail")
              (ja "詳細へ"))
   (to-shelf (en "To Shelf")
             (ja "書棚へ"))
   (to-table (en "To Table")
             (ja "正誤表へ"))
   (share-exlibris (en "Share Exlibris")
                   (ja "共有する"))
   (hide-exlibris (en "Hide Exlibris")
                  (ja "隠す"))
   (put-off (en "Put Off")
            (ja "外す"))
   (edit-review (en "Edit")
                (ja "編集"))
   (new-report (en "New Report")
               (ja "新たに報告"))
   (modify-revision (en "Modify Revision")
                    (ja "修正"))
   (modify-report (en "Modify Report")
                  (ja "報告を修正"))
   (drop-report (en "Drop Report")
                (ja "報告を削除"))
   (acknowledge (en "Acknowledge")
                (ja "引用に関してコメント"))
   (agree (en "Agree")
          (ja "訂正に同意"))
   (disagree (en "Disagree")
             (ja "訂正に反対"))
   )

  (locale ja)

)