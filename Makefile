YPSILON = ypsilon --sitelib=sitelib

.PHONY: migrate test

migrate:
	mysql -u root -p errata < db/errata.sql

test:
	$(YPSILON) tests/errata.scm
