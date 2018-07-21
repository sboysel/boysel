R=/usr/local/bin/R

all: README.md check docs

README.md: README.Rmd
	$(R) -e "knitr::knit('README.Rmd')"
	rm -f README.html

docs: R/*.R tests/testthat/*.R _pkgdown.yml
	$(R) -e "devtools::document()"
	$(R) -e "pkgdown::build_site()"

check: R/*.R tests/testthat/*.R _pkgdown.yml
	$(R) -e "devtools::check()"
