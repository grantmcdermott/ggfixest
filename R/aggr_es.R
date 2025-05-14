#' @title Aggregates event-study treatment effects.
#'
#' @md
#' @description Aggregates post- (and/or pre-) treatment effects of an
#' "event-study" estimation, also known as a dynamic difference-in-differences
#' (DDiD) model. The event-study should have been estimated using the `fixest`
#' package, which provides a specialised `i()` operator for this class
#' of models. By default, the function will return the average post-treatment
#' effect (i.e., across multiple periods). However, it can also return the
#' cumulative post-treatment effect and can be used to aggregate pre-treatment
#' effects too.
#' @param object A model object of class `fixest`, where the `i()` operator has
#' been used to facilitate an "event-study" DiD design. See Examples.
#' @param period Keyword string or numeric sequence. Which group of periods
#' are we aggregating? Accepts the following convenience strings: `"post"` (the
#' default), `"pre"`, `"both"`, or `"diff` (for the difference between the post
#' and pre periods). Alternatively, can also be a numeric sequence that
#' designates an explicit subset of periods in the data (e.g. `6:8`).
#' @param rhs Numeric. The null hypothesis value. Defaults to 0.
#' @param aggregation Character string. The aggregation type. Either `"mean"`
#' (the default) or `"cumulative"`.
#' @param abbr_term Logical. Should the leading "term" column of the return
#' data frame be abbreviated? The default is `TRUE`. If `FALSE`, then the term
#' column will retain the full hypothesis test string as per usual with
#' [`marginaleffects::hypotheses()`]. Note that this information is retained as
#' an attribute of the return object, regardless.
#' @param ... Additional arguments passed to [`marginaleffects::hypotheses()`].
#' @seealso [marginaleffects::hypotheses()], which this function is ultimately a
#' convenience wrapper around.
#' @return A "tidy" data frame of aggregated (pre and/or post) treatment
#' effects, plus inferential information about standard errors, confidence
#' intervals, etc. Potentially useful information about the underlying
#' hypothesis test is also provided as an attribute. See Examples.
#' @importFrom marginaleffects hypotheses
#' @export
#' @examples
#' library(ggfixest) ## Will load fixest too
#'
#' est = feols(y ~ x1 + i(period, treat, 5) | id + period, base_did)
#'
#' # Default hypothesis test is a null mean post-treatment effect
#' (post_mean = aggr_es(est))
#'
#' # The underlying hypothesis is saved as an attribute
#' attr(post_mean, "hypothesis")
#'
#' # Other hypothesis and aggregation options
#' aggr_es(est, period = "pre")             # pre period instead of post
#' aggr_es(est, period = "both")            # pre & post periods separately
#' aggr_es(est, period = "diff")            # post vs pre difference
#' aggr_es(est, period = 6:8)               # specific subset of periods
#' aggr_es(est, period = "pre", rhs = -1)   # pre period with H0 value of 1
#' aggr_es(est, aggregation = "cumulative") # cumulative instead of mean effects
#' # Etc.
#'
aggr_es = function(object,
                   period = c("post", "pre", "both", "diff"),
                   rhs = 0,
                   aggregation = c("mean", "cumulative"),
                   abbr_term = TRUE,
                   ...) {
    aggregation = match.arg(aggregation)
    # if (!(any(period %in% c("post", "pre", "both")) || is.numeric(period))) {
    #     stop('The `period` argument must be one of c("post", "pre", "both"), or a numeric sequence.')
    # }
    if (!is.numeric(period)) {
        period = match.arg(period)
    }

    fixest_obj = inherits(object, "fixest")
    if (!fixest_obj) stop("Please provide a valid fixest object.")
    mm = object$model_matrix_info[[1]]
    if (is.null(mm)) {
        stop(
            "This function only works with fixest objects that were ",
            "created with the i() operator. See `?fixest::i()`."
        )
    }
    coefs = mm$coef_names_full
    ref_id = mm$ref_id[1]
    ## Store our periods in a list to make the below lapply call easier
    if (is.numeric(period)) {
        if (!all(period %in% mm$items)) {
            stop(
                '\nSupplied period sequence does not match periods in model object.',
                '\nUser-supplied periods: ', period,
                'Model periods: ', mm$items
            )
        }
        if (any(period %in% mm$ref)) {
            warning("\nThe reference period, ", mm$ref, ", cannot be included in the aggregation and will be dropped.")
            period = setdiff(period, mm$ref)
        }
        idx = list(match(period, mm$items))
        names(idx) = paste("periods", paste(range(period), collapse=":"))
    } else if (period == "post") {
        idx = list("post" = (ref_id + 1):length(coefs))
    } else if (period == "pre") {
        idx = list("pre" = 1:(ref_id - 1)) # ref period is dropped from the model
    } else if (period %in% c("both", "diff")) {
        idx = list("pre" = 1:(ref_id - 1), "post" = (ref_id + 1):length(coefs))
        if (period == "diff") {
            idx = rev(idx) # we want post - pre
            rhs = NULL
        }
    }

    hstring = sapply(
        idx,
        function(i) gen_hstring(
            coefs,
            periods = i,
            aggregation = aggregation,
            rhs = rhs
        )
    )
    if (!is.numeric(period) && period == "diff") hstring = paste(hstring, collapse = " = ")

    res = hypotheses(object, hypothesis = hstring, ...)
    if (abbr_term) {
        if (is.numeric(period)) {
            res$term = paste0(names(idx), " (", aggregation, ")")
        } else if (period == "diff") {
            res$term = "difference (post vs pre mean)"
        } else {
            res$term = paste0(names(idx), "-treatment (", aggregation, ")")
        }
    }

    ## Remove "hypothesis" column and rather save as an attribute
    res$hypothesis = NULL
    attr(res, "hypothesis") = hstring

    return(res)
}

gen_hstring = function(coefs, periods = NULL, aggregation = "mean", rhs = 0) {
    if (is.null(periods)) periods = 1:length(coefs)
    coefs = coefs[periods]
    hstring = paste(paste0("`", coefs, "`"), collapse = " + ")
    if (aggregation == "mean") hstring = paste0("(", hstring, ")/", length(coefs))
    if (!is.null(rhs)) hstring = paste(hstring, "=", rhs)
    return(hstring)
}
