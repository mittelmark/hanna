build:
	R CMD build .

check: build
	#rm -f R/*~
	#rm *~
	Rscript bin/rman.R R/add.R	
	R CMD check hanna_0.1.tar.gz
