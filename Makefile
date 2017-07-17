R := $(wildcard R/*.R)

all: docs

check: $(R)
		R -e "devtools::check()"

docs: check $(R)
		R -e "pkgdown::build_site()"
	  
clean:
		rm -rf README.md docs
