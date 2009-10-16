#!/usr/bin/env ypsilon
#!r6rs

(import (rnrs)
        (errata configuration)
        (errata rss)
        (xunit))

(skip-unless (and rss-temporary-directory rss-output-directory)
  (assert-zero? (emit "recent-reports"))
  (assert-zero? (emit "recent-reviews"))
  (assert-zero? (emit "recent-revisions")))

(report)
