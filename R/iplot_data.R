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
#' @param .aggr_es A character string indicating whether the aggregated mean
#' post- (and/or pre-) treatment effect should be added as a column to the
#' returned data frame. Passed to `aggr_es(..., aggregation = "mean")` and
#' should be one of "none" (the default), "post", "pre", or "both".
#' @details This function is a wrapper around
#' `fixest::iplot(..., only.params = TRUE)`, but with various checks and tweaks
#' to better facilitate plotting with `ggplot2` and handling of complex object
#' types (e.g. lists of fixest_multi models)
#' @seealso [fixest::iplot()], [aggr_es()].
#' @return A data frame consisting of estimate values, confidence intervals,
#' relative x-axis positions, and other aesthetic information needed to draw
#' a ggplot2 object.
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
iplot_data = function(
	object,
	.ci_level = 0.95,
	.dict = fixest::getFixest_dict(),
	.aggr_es = c("none", "post", "pre", "both")
	) {
  .aggr_es = match.arg(.aggr_es)
  p = fixest::iplot(object, only.params = TRUE, ci_level = .ci_level, dict = .dict)
  d = p$prms

  if (inherits(object, "fixest_multi")) {
      meta = fixest::models(object)
      # Drop the sample.var column if it exists (not needed for plotting)
      if ("sample.var" %in% colnames(meta)) {
          meta = meta[, -which(colnames(meta) == "sample.var")]
      }
      ## catch for multi-fixest that have common lhs (and thus not in meta)
      if (is.null(meta$lhs)) {
          meta$lhs = paste(object[[1]]$fml)[2]
          # bring lhs column to front (optional but nice for consistency)
          id_idx = which(colnames(meta) == "id") ## should be 1st column but better to be safe
          lhs_idx = which(colnames(meta) == "lhs")
          meta = cbind(meta[, c(id_idx, lhs_idx)], meta[, -c(id_idx, lhs_idx), drop = FALSE])
      }

      ## iplot automatically jitters the x-axis positions of the different models
      ## within a fixest_multi object. We'll leave that up to ggplot2, so let's
      ## replace with the "absolute" x-axis positions.
      d = do.call(
          "rbind",
          lapply(
              split(d, d$id),
              function(dat) {
                  dat$x = p$lab
                  return(dat)
              }
          )
      )

      d = merge(d, meta, on = "id")
      d$id = factor(d$id, labels = gsub("sample\\.var: \\w+; ", "", names(object))) ## again, don't need sample.var

  } else {

	if (inherits(p$labels, "integer")) p$labels = as.numeric(p$labels) ## catch (for geom_ribbon)
    if (!identical(d$x, p$labels)) d$x = factor(d$x, labels = p$labels)
    d$lhs = deparse1(object$fml[[2]])

  }

  d$ci_level = .ci_level

  if (.aggr_es != "none") {
      ea = aggr_es(object, period = .aggr_es)
      ref_idx = which(d$is_ref)
      d$aggr_eff = 0
      if (.aggr_es %in% c("post", "both")) {
          if (ref_idx < nrow(d)) {
              d$aggr_eff[(ref_idx + 1):nrow(d)] = ea$estimate[which(ea$term == "post-treatment (mean)")]
          }
      }
      if (.aggr_es %in% c("pre", "both")) {
          if (ref_idx > 1) {
              d$aggr_eff[1:(ref_idx - 1)] = ea$estimate[which(ea$term == "pre-treatment (mean)")]
          }
      }
  }

  return(d)

}
