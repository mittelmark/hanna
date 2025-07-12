VERSION := $(shell grep Version: DESCRIPTION | perl -pe 's/.+: //')
PKG     := $(shell basename `pwd`)
build:
	R CMD build . 

check: man/simul.Rd man/hgraph.Rd man/hanna-package.Rd build 
	R CMD check $(PKG)_$(VERSION).tar.gz

man/%.Rd: R/%.R
	Rscript bin/rman.R $<

build-easy: man/simul.Rd man/hgraph.Rd man/hanna-package.Rd
	R CMD build --no-build-vignettes .
	
check-easy: build-easy
	R CMD check --no-build-vignettes $(PKG)_$(VERSION).tar.gz
