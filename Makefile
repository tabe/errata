YPSILON_SITELIB=/home/tabe/lunula/sitelib:/home/tabe/base64:/home/tabe/lcs:/home/tabe/manued:/home/tabe/ssax:/home/tabe/uri:/home/tabe/xunit:/home/tabe/ypsilon-foreign-lib/sitelib:/home/tabe/ypsilon-http/sitelib

YPSILON=env YPSILON_SITELIB=$(YPSILON_SITELIB) LUNULA_TEMPLATES=/home/tabe/errata/templates \
  ypsilon --sitelib=sitelib --heap-limit=16

.PHONY: check migrate dump fixtures restore start stop svc svstat image stop-all stats test

check: test

migrate:
	mysql -u root -p errata < db/errata.sql

dump:
	mysqldump -u root -p errata > db/errata.dump

fixtures:
	$(YPSILON) db/fixtures.scm

restore:
	zcat /home/tabe/.adel/errata.dump.gz | mysql -u root -p errata

start:
	$(YPSILON) script/server.scm 3000 yoursql

stop:
	pkill -TERM -f -n 'ypsilon .*script/server'

svc:
	sudo svc -t /etc/service/errata
	sudo svc -t /etc/service/errata-image
	sudo svc -t /etc/service/errata-rss

svstat:
	sudo svstat /etc/service/errata
	sudo svstat /etc/service/errata-image
	sudo svstat /etc/service/errata-rss

image:
	$(YPSILON) script/image-server.scm

stop-image:
	pkill -TERM -f -n 'ypsilon .*script/image'

stop-all:
	kill -TERM `pidof ypsilon`

stats:
	find sitelib -type f -name '*.scm' | xargs wc -l
	find tests -type f -name '*.scm' | xargs wc -l

test:
	$(YPSILON) tests/errata/calendar.scm
	$(YPSILON) tests/errata/helper/pagination.scm
	$(YPSILON) tests/errata/helper.scm
	$(YPSILON) tests/errata/isbn.scm
	$(YPSILON) tests/errata/message.scm
	$(YPSILON) tests/errata/model.scm
	$(YPSILON) tests/errata/page.scm
	$(YPSILON) tests/errata/query.scm
	$(YPSILON) tests/errata/rss/recent-public-revisions.scm
	$(YPSILON) tests/errata/rss.scm
	$(YPSILON) tests/errata/validator.scm
	$(YPSILON) tests/errata.scm
