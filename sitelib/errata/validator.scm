(library (errata validator)
  (export *password-min-length*
          *nick-max-length*
          *mail-address-max-length*
          validate-account-to-modify
          validate-new-account
          authenticate-account)
  (import (rnrs)
          (pregexp)
          (prefix (only (lunula hmac) sha-256) hmac:)
          (only (lunula mysql) lookup)
          (lunula session)
          (lunula validation)
          (errata model))

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

  (define-composite-validator validate-account-to-modify
    (account-to-modify-current-password validate-password)
    (account-to-modify-new-password validate-password)
    (account-to-modify-mail-address validate-mail-address))

  (define-validator (validate-new-nick nick)
    (nick-already-used)
    (when (lookup account `((nick ,nick)))
      (nick-already-used)))

  (define-validator (same-password password password-re)
    (password-differs-from-re)
    (unless (string=? password password-re)
      (password-differs-from-re)))

  (define-composite-validator validate-new-account
    (new-account-nick validate-nick validate-new-nick)
    (new-account-password validate-password)
    (new-account-password-re validate-password)
    ((lambda (x) (values (new-account-password x)
                         (new-account-password-re x)))
     same-password)
    (new-account-mail-address validate-mail-address))

  (define-validator (existing-account a)
    (does-not-exist)
    (cond ((lookup account `((nick ,(account-to-login-nick a))))
           => (lambda (found)
                (let ((algorithm (account-hash-algorithm found)))
                  (cond ((string=? "sha-256" algorithm)
                         (if (string=? (account-password found)
                                       (hmac:sha-256 (account-hash-key found)
                                                     (string->utf8 (account-to-login-password a))))
                             found
                             (does-not-exist)))
                        (else (does-not-exist))))))
          (else (does-not-exist))))

  (define-composite-validator authenticate-account
    (account-to-login-nick validate-nick)
    (account-to-login-password validate-password)
    (values existing-account))

)
