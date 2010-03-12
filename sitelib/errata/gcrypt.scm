(library (errata gcrypt)
  (export)
  (import (ypsilon gcrypt)
          (only (lunula configuration) define-configuration))

  (define-configuration gcrypt-version)

  (gcry_check_version gcrypt-version)
  (gcry_control GCRYCTL_DISABLE_SECMEM)
  (gcry_control GCRYCTL_INITIALIZATION_FINISHED)

)
