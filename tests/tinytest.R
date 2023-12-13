## Throttle CPU threads if R CMD check (for CRAN)

# fixest
if ( requireNamespace("fixest", quietly=TRUE) ){
	library(fixest)
	setFixest_nthreads(1)
}

# data.table
if(requireNamespace("data.table", quietly = TRUE)){
	library(data.table)
	setDTthreads(1)
}


# Run tinytest suite

if ( requireNamespace("tinytest", quietly=TRUE) ){

  tinytest::test_package("ggfixest")

}

