R := $(wildcard R/*.R)

all: README.md

README.md: README.Rmd
		R -e "rmarkdown::render('README.Rmd')"

docs: $(R)
		R -e "devtools::document()"
		R -e "pkgdown::build_site()"
	  
clean:
		rm -rf README.md docs
