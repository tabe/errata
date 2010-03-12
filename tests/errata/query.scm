#!r6rs

(import (ypsilon gcrypt)
        (errata query)
        (xunit))

(gcry_check_version "1.4.5") ; you might modify the version
(gcry_control GCRYCTL_DISABLE_SECMEM)
(gcry_control GCRYCTL_INITIALIZATION_FINISHED)

(report)
