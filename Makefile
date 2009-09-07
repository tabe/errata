YPSILON = ypsilon --sitelib=sitelib
YPSILON_SITELIB = /home/tabe/lunula/sitelib:/home/tabe/base64:/home/tabe/lcs:/home/tabe/ssax:/home/tabe/uri:/home/tabe/xunit:/home/tabe/ypsilon-foreign-lib/sitelib:/home/tabe/ypsilon-http/sitelib

.PHONY: check migrate fixtures start image stop stats test

check: test

migrate:
	mysql -u root -p errata < db/errata.sql

fixtures:
	env YPSILON_SITELIB=$(YPSILON_SITELIB) $(YPSILON) db/fixtures.scm

start:
	env YPSILON_SITELIB=$(YPSILON_SITELIB) script/server 3000 yoursql

image:
	env YPSILON_SITELIB=$(YPSILON_SITELIB) $(YPSILON) script/image-server.scm

stop:
	kill -TERM `pidof ypsilon`

stats:
	find sitelib -type f -name '*.scm' | xargs wc -l
	find tests -type f -name '*.scm' | xargs wc -l

test:
	env YPSILON_SITELIB=$(YPSILON_SITELIB) $(YPSILON) tests/errata/calendar.scm
	env YPSILON_SITELIB=$(YPSILON_SITELIB) $(YPSILON) tests/errata/helper/pagination.scm
	env YPSILON_SITELIB=$(YPSILON_SITELIB) $(YPSILON) tests/errata/helper.scm
	env YPSILON_SITELIB=$(YPSILON_SITELIB) $(YPSILON) tests/errata/hmac.scm
	env YPSILON_SITELIB=$(YPSILON_SITELIB) $(YPSILON) tests/errata/isbn.scm
	env YPSILON_SITELIB=$(YPSILON_SITELIB) $(YPSILON) tests/errata/model.scm
	env YPSILON_SITELIB=$(YPSILON_SITELIB) $(YPSILON) tests/errata/query.scm
	env YPSILON_SITELIB=$(YPSILON_SITELIB) $(YPSILON) tests/errata/validator.scm
	env YPSILON_SITELIB=$(YPSILON_SITELIB) $(YPSILON) tests/errata.scm
