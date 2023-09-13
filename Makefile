VERSION := $(shell grep Version: DESCRIPTION | perl -pe 's/.+: //')
PKG     := $(shell basename `pwd`)

build: man/simul.Rd man/hgraph.Rd
	R CMD build . 

check: build 
	R CMD check $(PKG)_$(VERSION).tar.gz

man/%.Rd: R/%.R
	Rscript bin/rman.R $<
