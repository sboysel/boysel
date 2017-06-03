R := $(wildcard R/*.R)

all: docs README.md

check:
		R -e "devtools::check()"

README.md: README.Rmd
		R -e "knitr::knit('README.Rmd')"

docs: check $(R)
		R -e "pkgdown::build_site()"
	  
clean:
		rm -rf README.md docs
