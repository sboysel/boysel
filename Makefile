R=/usr/bin/R

all: knit docs

knit: README.Rmd
	$(R) -e "knitr::knit('README.Rmd')"

docs:
	$(R) -e "pkgdown::build_site()"
