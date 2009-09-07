YPSILON_SITELIB = /home/tabe/lunula/sitelib:/home/tabe/base64:/home/tabe/lcs:/home/tabe/ssax:/home/tabe/uri:/home/tabe/xunit:/home/tabe/ypsilon-foreign-lib/sitelib:/home/tabe/ypsilon-http/sitelib

YPSILON = env YPSILON_SITELIB=$(YPSILON_SITELIB) ypsilon --sitelib=sitelib

.PHONY: check migrate fixtures start image stop stats test

check: test

migrate:
	mysql -u root -p errata < db/errata.sql

fixtures:
	$(YPSILON) db/fixtures.scm

start:
	env YPSILON_SITELIB=$(YPSILON_SITELIB) script/server 3000 yoursql

image:
	$(YPSILON) script/image-server.scm

stop:
	kill -TERM `pidof ypsilon`

stats:
	find sitelib -type f -name '*.scm' | xargs wc -l
	find tests -type f -name '*.scm' | xargs wc -l

test:
	$(YPSILON) tests/errata/calendar.scm
	$(YPSILON) tests/errata/helper/pagination.scm
	$(YPSILON) tests/errata/helper.scm
	$(YPSILON) tests/errata/isbn.scm
	$(YPSILON) tests/errata/model.scm
	$(YPSILON) tests/errata/query.scm
	$(YPSILON) tests/errata/validator.scm
	$(YPSILON) tests/errata.scm
