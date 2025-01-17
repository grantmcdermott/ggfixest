#' @title Aggregates event-study treatment effects.
#'
#' @md
#' @description Aggregates post- (and/or pre-) treatment effects of an
#' "event-study" estimation, also known as a dynamic difference-in-differences
#' (DDiD) model. The event-study should have been estimated using the `fixest`
#' package, which provides a specialised `i()` operator for this class
#' of models. By default, the function will return the average post-treatment
#' effect (i.e. across multiple periods). However, it can also return the
#' cumulative post-treatment effect and can be used to aggregate pre-treatment
#' effects too. At its heart, `aggr_es()` is a convenience wrapper around
#' `marginaleffects::hypotheses()`, which is used to perform the underlying
#' joint hypothesis test.
#' @param object A model object of class `fixest`, where the `i()` operator has
#' been used to facilitate an "event-study" DiD design. See Examples.
#' @param rhs Numeric. The null hypothesis value. Defaults to 0.
#' @param period Keyword string or numeric sequence. Which group of periods
#' are we aggregating? Can either be one of three convenience strings---i.e.,
#' "post" (the default), "prep", or "both"---or a numeric sequence that matches
#' a subset of periods in the data (e.g. 6:8).
#' @param aggregation Character string. The aggregation type. Either "mean" (the
#' default) or "cumulative".
#' @param abbr_term Logical. Should the leading "term" column of the return
#' data frame be abbreviated? The default is TRUE. If FALSE, then the term
#' column will retain the full hypothesis test string as per usual with
#' `marginaleffects()`. Note that this information is retained as an attribute
#' of the return object, regardless.
#' @param ... Additional arguments passed to `marginaleffects::hypotheses()`.
#' @seealso [marginaleffects::hypotheses()]
#' @return A "tidy" data frame of aggregated (pre and/or post) treatment
#' effects, plus inferential information about standard errors, confidence
#' intervals, etc. Potentially useful information about the underlying
#' hypothesis test is also provided as an attribute. See Examples.
#' @export
#' @examples
#' library(ggfixest) ## Will load fixest too
#'
#' est = feols(y ~ x1 + i(period, treat, 5) | id + period, base_did)
#'
#' # Default hypothesis test is a null mean post-treatment effect
#' (post_mean = aggr_es(est))
#' # The underlying hypothesis is saved as an attribute
#' attr(post_mean, "hypothesis")
#'
#' # Other hypothesis and aggregation options
#' aggr_es(est, aggregation = "cumulative") # cumulative instead of mean effects
#' aggr_es(est, period = "pre")             # pre period instead of post
#' aggr_es(est, period = "both")            # pre & post periods separately
#' aggr_es(est, period = 6:8)               # specific subset of periods
#' aggr_es(est, rhs = -1, period = "pre")   # pre period with H0 value of 1
#' # Etc.
#'
aggr_es = function(object,
                   rhs = 0,
                   # period = c("post", "pre", "both"),
                   period = "post",
                   aggregation = c("mean", "cumulative"),
                   abbr_term = TRUE,
                   ...) {
    aggregation = match.arg(aggregation)
    if (!(any(period %in% c("post", "pre", "both")) || is.numeric(period))) {
        stop('The `period` argument must be one of c("post", "pre", "both"), or a numeric sequence.')
    }
    # period = match.arg(period)

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
        names(idx) = paste0("periods", paste(range(period), collapse=":"))
    } else if (period == "post") {
        idx = list("post" = (ref_id + 1):length(coefs))
    } else if (period == "pre") { ## still need to handle the "both" option
        idx = list("pre" = 1:(ref_id - 1)) # ref period is dropped from the model
    } else if (period == "both") {
        idx = list("pre" = 1:(ref_id - 1), "post" = (ref_id + 1):length(coefs))
    }
    ## We're doing a bit more work than we need to here with the lapply call and
    ## binding afterwards. But this allows us to handle the "both" aggregation
    ## case.
    res = lapply(
        seq_along(idx),
        function(i) {
            coefs2 = coefs[idx[[i]]]
            period2 = names(idx)[i]
            hypothesis = paste(paste0("`", coefs2, "`"), collapse = " + ")
            if (aggregation == "mean") hypothesis = paste0("(", hypothesis, ")/", length(coefs2))
            hypothesis = paste(hypothesis, "=", rhs)
            ret = marginaleffects::hypotheses(object, hypothesis = hypothesis, ...)
            if (abbr_term) ret$term = paste0(period2, "-treatment (", aggregation, ")")
            attr(ret, "hypothesis") = hypothesis
            return(ret)
        }
    )
    ## Bind together and capture/re-assign and the hypothesis attribute
    hyp_attr = sapply(res, function(x) attr(x, "hypothesis"))
    res = do.call("rbind", res)
    row.names(res) = NULL
    if (!is.numeric(period)) attr(res, "hypothesis") = hyp_attr
    return(res)
}
