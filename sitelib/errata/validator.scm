(library (errata validator)
  (export *password-min-length*
          *nick-max-length*
          *mail-address-max-length*
          validate-account
          validate-new-account
          authenticate-account)
  (import (rnrs)
          (pregexp)
          (only (lunula mysql) lookup)
          (lunula session)
          (lunula validation))

  (define *password-min-length* 8)
  (define *nick-max-length* 16)
  (define *mail-address-max-length* 256)

  (define-validator (validate-password password)
    (password-is-blank password-too-short)
    (let ((len (string-length password)))
      (cond ((zero? len) (password-is-blank))
            ((< len *password-min-length*) (password-too-short)))))

  (define-validator (validate-nick nick)
    (nick-is-blank nick-too-long nick-invalid-char)
    (let ((len (string-length nick)))
      (cond ((zero? len) (nick-is-blank))
            (else
             (when (< *nick-max-length* len)
               (nick-too-long))
             (unless (pregexp-match "^[A-Za-z_][A-Za-z0-9_]*$" nick)
               (nick-invalid-char))))))

  (define-validator (validate-mail-address mail-address)
    (mail-address-is-blank mail-address-too-long)
    (let ((len (string-length mail-address)))
      (cond ((zero? len) (mail-address-is-blank))
            ((< *mail-address-max-length* len) (mail-address-too-long)))))

  (define-composite-validator validate-account
    (account-nick validate-nick)
    (account-password validate-password)
    (account-mail-address validate-mail-address))

  (define-validator (validate-new-nick nick)
    (nick-already-used)
    (when (lookup account `((nick ,nick)))
      (nick-already-used)))

  (define-composite-validator validate-new-account
    (account-nick validate-nick validate-new-nick)
    (account-password validate-password)
    (account-mail-address validate-mail-address))

  (define-validator (existing-account a)
    (does-not-exist)
    (or (lookup account `((nick ,(account-nick a))
                          (password ,(account-password a))))
        (does-not-exist)))

  (define-composite-validator authenticate-account
    (account-nick validate-nick)
    (account-password validate-password)
    (values existing-account))

)
