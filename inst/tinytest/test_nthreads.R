library(tinytest)

# fixest
if ( requireNamespace("fixest", quietly=TRUE) ){
	library(fixest)
	nfx = getFixest_nthreads()
	expect_equal(nfx, 1, info = "Check fixest threads")
}

# data.table
if(requireNamespace("data.table", quietly = TRUE)){
	library(data.table)
	nDT = getDTthreads()
	expect_equal(nDT, 1, info = "Check data.table threads")
}
