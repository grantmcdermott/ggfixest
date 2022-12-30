#' @title Aggregates event-study treatment effects.
#'
#' @description Aggregates post- (and/or pre-) treatment effects of an
#' "event-study" estimation, also known as a dynamic difference-in-differences
#' (DDiD) model. The event-study should have been estimated using the `fixest`
#' package, which provides a specialised `i()` operator for this class
#' of models. By default, the function will return the average post-treatment
#' effect (i.e. across multiple periods). However, it can also return the
#' cumulative post-treatment effect and can be used to aggregate pre-treatment
#' effects too. At its heart, `aggr_es()` is a convenience wrapper around
#' `marginaleffects::deltamethod()`, which is used to perform the underlying
#' joint hypothesis test.
#' @param object A model object of class `fixest`, where the `i()` operator has
#' been used to facilitate an "event-study" DiD design. See Examples.
#' @param rhs Numeric. The null hypothesis value. Defaults to 0.
#' @param period Character string. Which group of periods are we aggregating?
#' One of "post" (the default), "prep", or "both".
#' @param aggregation Character string. The aggregation type. Either "mean" (the
#' default) or "cumulative".
#' @param abbr_term Logical. Should the leading "term" column of the return
#' data frame be abbreviated? The default is TRUE. If FALSE, then the term
#' column will retain the full hypothesis test string as per usual with
#' `marginaleffects()`. Note that this information is retained as an attribute
#' of the return object, regardless.
#' @param ... Additional arguments passed to `marginaleffects::deltamethod()`.
#' @seealso [marginaleffects::deltamethod()]
#' @return A "tidy" data frame of aggregated (pre and/or post) treatment
#' effects, plus inferential information about standard errors, confidence
#' intervals, etc. Potentially useful information about the underlying
#' hypothesis test is also provided as an attribute. See Examples.
#' @export
#' @examples
#' library(fixest)
#'
#' est = feols(y ~ x1 + i(period, treat, 5) | id + period, base_did)
#'
#' # Default hypothesis test is a null mean post-treatment effect
#' (post_mean = aggr_es(est))
#' # The underlying hypothesis is saved as an attribute
#' attributes(post_mean)["hypothesis"]
#'
#' # Other hypothesis and aggregation options
#' aggr_es(est, aggregation = "cumulative")
#' aggr_es(est, period = "both")
#' aggr_es(est, rhs = -1, period = "pre")
#' # Etc.
#'
aggr_es = function(object,
                   rhs = 0,
                   period = c("post", "pre", "both"),
                   aggregation = c("mean", "cumulative"),
                   abbr_term = TRUE,
                   ...) {
	aggregation = match.arg(aggregation)
	period = match.arg(period)
	fixest_obj = inherits(object, 'fixest')
	if (!fixest_obj) stop("Please provide a valid fixest object.")
	mm = object$model_matrix_info[[1]]
	if (is.null(mm)) stop ("This function only works with fixest objects that were ",
												 "created with the i() operator. See `?fixest::i()`.")
	coefs = mm$coef_names_full
	ref_id = mm$ref_id[1]
	## Store our periods in a list to make the below lapply call easier
	if (period=="post") {
		idx = list("post" = (ref_id+1):length(coefs))
	} else if (period=="pre") { ## still need to handle the "both" option
		idx = list("pre" = 1:(ref_id-1)) # ref period is dropped from the model
	} else {
		idx = list('pre' = 1:(ref_id-1), 'post' = (ref_id+1):length(coefs))
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
			if (aggregation=="mean") hypothesis = paste0("(", hypothesis, ")/", length(coefs2))
			hypothesis = paste(hypothesis, "=", rhs)
			ret = marginaleffects::deltamethod(object, hypothesis = hypothesis, ...)
			if (abbr_term) ret$term = paste0(period2, "-treatment (", aggregation, ")")
			attr(ret, "hypothesis") = hypothesis
			return(ret)
		}
	)
	## Bind together and capture/re-assign and the hypothesis attribute (again,
	## mostly for the "both" case)
	hyp_attr = sapply(res, \(x) attributes(x)["hypothesis"])
	res = do.call("rbind", res)
	row.names(res) = NULL
	if (period=="both") {
		attributes(res) = utils::modifyList(attributes(res), hyp_attr)
		attributes(res)["hypothesis"] = NULL
	}
	return(res)
}
