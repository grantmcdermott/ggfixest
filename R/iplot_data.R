#' @title Internal function for grabbing and preparing iplot data
#'
#' @description Grabs the underlying data used to construct `fixest::iplot`,
#' with some added functionality and tweaks for the `ggiplot` equivalents.
#' @param object A model object of class `fixest` or `fixest_multi`, where
#' the `i()` operator has been used to construct an interaction, or set of
#' interactions.
#' @param .ci_level A number between 0 and 1 indicating the desired confidence
#' level, Defaults to 0.95.
#' @param .dict A dictionary (i.e. named character vector or a logical scalar).
#' Used for changing coefficient names. Defaults to the values in
#' `getFixest_dict()`. See the `?fixest::iplot` documentation for more
#' information.
#' @details This function is a wrapper around
#' `fixest::iplot(..., only.params = TRUE)`, but with various checks and tweaks
#' to better facilitate plotting with `ggplot2` and handling of complex object
#' types (e.g. lists of fixest_multi models)
#' @seealso [fixest::iplot()].
#' @return A ggplot2 object.
#' @import ggplot2
#' @export
#' @examples
#' library(fixest)
#'
#' est_did = feols(y ~ x1 + i(period, treat, 5) | id+period,
#'                 data = base_did)
#' iplot(est_did, only.params = TRUE) # The "base" version
#' iplot_data(est_did)                # The wrapper provided by this package
#'
#' # Illustrative fixest_multi case, where the sample has been split by odd and
#' # even ID numbers.
#' est_split = feols(y ~ x1 + i(period, treat, 5) | id+period,
#'                   data = base_did, split = ~id%%2)
#' iplot(est_split, only.params = TRUE) # The "base" version
#' iplot_data(est_split)                # The wrapper provided by this package
#'
iplot_data = function(object, .ci_level = 0.95, .dict = fixest::getFixest_dict()) {
	p = fixest::iplot(object, only.params = TRUE, ci_level = .ci_level, dict = .dict)
	d = p$prms
	if (inherits(object, 'fixest_multi')) {
		meta = attr(object, "meta")
		dep_vars = meta$all_names$lhs
		if (is.null(dep_vars)) {
			# dep_vars = unique(as.character(lapply(object, function(m) paste(m$call$fml[[2]]))))
			dep_vars = unique(as.character(lapply(object, function(m) paste(m$fml[[2]]))))
		}
		## Need to do a bit of finicky work, depending on whether the fixest_multi
		## object is just a multi-LHS object.
		# if (all.equal(dep_vars, names(object))) {
		if (!is.null(meta$all_names$lhs)) {
			if (any(c("rhs", "sample") %in% colnames(meta$tree))) {
				d$x = rep(p$labels, each = length(object)*length(dep_vars))
				if (!is.null(meta$all_names$rhs)) {
					d$id = factor(d$id, labels = rep(meta$all_names$rhs, each = length(dep_vars)))
				} else {
					d$id = factor(d$id, labels = rep(names(object), each = length(dep_vars)))
				}
			} else {
				d$x = rep(p$labels, each = length(object))
				d$id = factor(d$id, labels = names(object))
			}
		} else {
			d$x = rep(p$labels, each = length(object)*length(dep_vars))
			d$id = factor(d$id, labels = rep(names(object), each = length(dep_vars)))
		}
		d$dep_var = dep_vars
	} else {
		if (inherits(p$labels, 'integer')) p$labels = as.numeric(p$labels) ## catch
		if (!identical(d$x, p$labels)) d$x = factor(d$x, labels = p$labels)
		# d$dep_var = paste(object$call$fml[[2]])
		d$dep_var = paste(object$fml[[2]])
	}
	d$ci_level = .ci_level
	return(d)
}
