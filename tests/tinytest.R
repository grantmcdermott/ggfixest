## Throttle CPU threads if R CMD check (for CRAN)

if (any(grepl("_R_CHECK", names(Sys.getenv()), fixed = TRUE))) {
    # fixest
    if (requireNamespace("fixest", quietly = TRUE)) {
        library(fixest)
        setFixest_nthreads(1)
    }

    # data.table
    if (requireNamespace("data.table", quietly = TRUE)) {
        library(data.table)
        setDTthreads(1)
    }

    # magick
    if (requireNamespace("magick", quietly = TRUE)) {
        library(magick)
        magick:::magick_threads(1)
    }
}


# Run tinytest suite

if ( requireNamespace("tinytest", quietly=TRUE) ){

  tinytest::test_package("ggfixest")

}

