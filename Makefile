YPSILON_SITELIB=submodules/lunula/sitelib:submodules/lunula/submodules/base64:submodules/lcs:submodules/manued:/home/tabe/sxml/sitelib:submodules/lunula/submodules/uri:submodules/lunula/submodules/xunit:submodules/lunula/submodules/ypsilon-foreign-lib/sitelib:/home/tabe/ypsilon-http/sitelib

YPSILON=env YPSILON_SITELIB=$(YPSILON_SITELIB) \
  LUNULA_CONFIGURATION_DIRECTORY=config \
  LUNULA_TEMPLATES=templates \
  ypsilon --sitelib=sitelib --heap-limit=16

.PHONY: check migrate dump fixtures restore svc svstat stats test

check: test

migrate:
	mysql -u root -p errata < db/errata.sql

dump:
	mysqldump -u root -p errata > db/errata.dump

fixtures:
	$(YPSILON) db/fixtures.scm

restore:
	zcat /home/tabe/.adel/errata.dump.gz | mysql -u root -p errata

svc:
	sudo svc -t /etc/service/errata
	sudo svc -t /etc/service/errata-image
	sudo svc -t /etc/service/errata-rss

svstat:
	sudo svstat /etc/service/errata
	sudo svstat /etc/service/errata-image
	sudo svstat /etc/service/errata-rss

stats:
	find sitelib -type f -name '*.scm' | xargs wc -l
	find tests -type f -name '*.scm' | xargs wc -l

test:
	$(YPSILON) tests/errata/calendar.scm
	$(YPSILON) tests/errata/configuration.scm
	$(YPSILON) tests/errata/font.scm
	$(YPSILON) tests/errata/helper/pagination.scm
	$(YPSILON) tests/errata/helper.scm
	$(YPSILON) tests/errata/isbn.scm
	$(YPSILON) tests/errata/message.scm
	$(YPSILON) tests/errata/model.scm
	$(YPSILON) tests/errata/notification.scm
	$(YPSILON) tests/errata/page.scm
	$(YPSILON) tests/errata/query.scm
	$(YPSILON) tests/errata/rss/recent-acknowledgements.scm
	$(YPSILON) tests/errata/rss/recent-agreements.scm
	$(YPSILON) tests/errata/rss/recent-reports.scm
	$(YPSILON) tests/errata/rss/recent-reviews.scm
	$(YPSILON) tests/errata/rss/recent-revisions.scm
	$(YPSILON) tests/errata/rss.scm
	$(YPSILON) tests/errata/url.scm
	$(YPSILON) tests/errata/validator.scm
	$(YPSILON) tests/errata.scm
