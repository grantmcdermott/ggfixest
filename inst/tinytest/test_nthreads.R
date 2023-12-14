library(tinytest)

exit_if_not(any(grepl("_R_CHECK", names(Sys.getenv()), fixed = TRUE)))

# fixest
if ( requireNamespace("fixest", quietly=TRUE) ){
	library(fixest)
	nfx = getFixest_nthreads()
	expect_equal(nfx, 1, info = "Check fixest threads")
}

# data.table
if (requireNamespace("data.table", quietly = TRUE)) {
    library(data.table)
    nDT = getDTthreads()
    expect_equal(nDT, 1, info = "Check data.table threads")
}

# magick
if (requireNamespace("magick", quietly = TRUE)) {
    library(magick)
    nmg = magick:::magick_threads(1)
    expect_equal(nmg, 1, info = "Check magick threads")
}
