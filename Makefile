YPSILON = ypsilon --sitelib=sitelib

.PHONY: migrate test

migrate:
	mysql -u root -p typo < db/typo.sql

test:
	$(YPSILON) tests/typo.scm
