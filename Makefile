R := $(wildcard R/*.R)

all: README.md docs

README.md: README.Rmd
		R -e "knitr::knit('README.Rmd')"

docs: $(R)
		R -e "devtools::document()"
		R -e "pkgdown::build_site()"
	  
clean:
		rm -rf README.md docs
