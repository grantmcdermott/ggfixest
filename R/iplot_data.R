#' @title Internal function for grabbing and preparing iplot data.
#'
#' @md
#' @description Grabs the underlying data used to construct `fixest::iplot`,
#' with some added functionality and tweaks for the `ggiplot` equivalents.
#' @param object A model object of class `fixest` or `fixest_multi`, where
#' the `i()` operator has been used to construct an interaction, or set of
#' interactions.
#' @param .ci_level A number between 0 and 1 indicating the desired confidence
#' level, Defaults to 0.95.
#' @param .keep Character vector used to subset the coefficients of interest.
#' Passed down to `fixest::iplot(..., keep = .keep)` and should take the form of
#' an acceptable regular expression.
#' @param .drop Character vector used to subset the coefficients of interest
#' (complement of `.keep`). Passed down to `fixest::iplot(..., drop = .drop)`
#' and should take the form of an acceptable regular expression.
#' @param .group A list, default is missing. Each element of the list reports
#'   the coefficients to be grouped while the name of the element is the group
#'   name. Passed down to `fixest::coefplot(..., group = .group)`. Example of
#'   valid uses:
#'   * group=list(group_name="pattern")
#'   * group=list(group_name=c("var_start", "var_end"))
#'   * group=list(group_name=1:2)
#'   * See the Details section of `?fixest::coefplot` for more.
#' @param .dict A dictionary (i.e. named character vector or a logical scalar).
#' Used for changing coefficient names. Defaults to the values in
#' `getFixest_dict()`. See the `?fixest::coefplot` documentation for more
#' information. Note: This argument applies dictionary changes directly to the
#' return object for `coefplot_data`. However, it is ignored for `iplot_data`,
#' since we want to preserve the numeric ordering for potential event study
#' plots. (And imposing an ordered factor would create its own downstream
#' problems in the case of continuous plot features like ribbons.) Instead, any
#' dictionary replacement for `ggiplot` is deferred to the actual plot call and
#' applied directly to the labels.
#' @param .internal.only.i Logical variable used for some internal function
#'   handling when passing on to coefplot/iplot.
#' @param .i.select Integer scalar, default is 1. In (gg)iplot, used to select
#'   which variable created with i() to select. Only used when there are several
#'   variables created with i. This is an index, just try increasing numbers to
#'   hopefully obtain what you want. Passed down to
#'   `fixest::iplot(..., i.select = .i.select)`
#' @param .aggr_es A keyword string or numeric sequence indicating whether the
#' aggregated mean treatment effects for some subset of the model should be
#' added as a column to the returned data frame. Passed to
#' `aggr_es(..., aggregation = "mean")`.
#' @param .vcov,.cluster,.se Alternative options for adjusting the standard
#' errors of the model object on the fly. See `summary.fixest` for details
#' (although note that the "." period prefix should be ignored in the latter's
#' argument documentation). Written here in superseding order; `.cluster` will
#' only be considered if `.vcov` is not null, etc.
#' @details This function is a wrapper around
#' `fixest::iplot(..., only.params = TRUE)`, but with various checks and tweaks
#' to better facilitate plotting with `ggplot2` and handling of complex object
#' types (e.g. lists of fixest_multi models)
#' @seealso [fixest::iplot()], [aggr_es()].
#' @return A data frame consisting of estimate values, confidence intervals,
#' relative x-axis positions, and other aesthetic information needed to draw
#' a ggplot2 object.
#' @import ggplot2
#' @importFrom fixest coefplot iplot
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
		.keep = NULL,
		.drop = NULL,
		.dict = fixest::getFixest_dict(),
		.internal.only.i = TRUE,
		.i.select = 1,
		# .aggr_es = c("none", "post", "pre", "both"),
		.aggr_es = NULL,
		.group = "auto",
		.vcov = NULL,
		.cluster = NULL,
		.se = NULL
	) {

	# .aggr_es = match.arg(.aggr_es)
	if (is.null(.aggr_es)) .aggr_es = "none"
	if (isFALSE(.internal.only.i)) {
		## No pre/post aggregation allowed for coefplot
		if (is.numeric(.aggr_es) || .aggr_es!="none") warning("The .aggr_es argument will be ignored with (gg)coefplot calls.\n")
		.aggr_es = "none"
	}

	if (isTRUE(.internal.only.i)) {
		# No grouping permitted for iplot
		if (!is.null(.group) && .group!="auto") warning("The .group argument will be ignored with (gg)iplot calls.\n")
		.group = NULL
	}

	# Catch VCOV adjustments (if any)
	if (!is.null(.vcov)) {
		object = summary(object, vcov = .vcov)
	} else if (!is.null(.cluster)) {
		object = summary(object, cluster = .cluster)
	} else if (!is.null(.se)) {
		object = summary(object, se = .se)
	}

  p = coefplot(object, only.params = TRUE, ci_level = .ci_level, dict = .dict, keep = .keep, drop = .drop, internal.only.i = .internal.only.i, i.select = .i.select)
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

  #
  ## Grouping variables with (gg)coefplot
  ##
  ## GM note: The next section of code, which is quite lengthy, is adapted from
  ## fixest::coefplot plotting internals. We need to implement those changes a
  ## bit further upstream to ensure that they are recorded in the return data
  ## frame object for this coefplot_data() function. (Put differently: Because
  ## ggplot2 understands data frames, we need to make sure that the grouping
  ## info and adjustments are encoded in the data frame before plot time.)
  #

  x_labels = d$estimate_names
  # x_labels_raw = d$estimate_names_raw
  x_labels_raw = d$estimate_names
  group = .group
  dict = .dict

  # group = auto ####
  is_iplot = .internal.only.i

  # The value of group = "auto" => renaming the labels
  if(identical(group, "auto") && is_iplot == FALSE){
  	# we change the names of interactions
  	qui = grepl(":", x_labels_raw)
  	if(any(qui)){
  		x_inter = gsub(":.+", "", x_labels_raw[qui])
  		tx_inter = table(x_inter)
  		qui_auto = names(tx_inter)[tx_inter >= 2]

  		group = list()

  		for(i in seq_along(qui_auto)){
  			var_left = qui_auto[i]
  			qui_select = substr(x_labels_raw, 1, nchar(var_left)) == var_left

  			x_select = x_labels_raw[qui_select]

  			is_inter = TRUE
  			n_trim = 0
  			group_regex = NULL
  			if(all(grepl("[^:]:[^:].*::", x_select))){
  				# treat:period::1
  				first_part = strsplit(x_select[1], "::")[[1]][1]
  				var_right = gsub(".+:", "", first_part)
  				n_max = nchar(first_part) + 2

  			} else if(all(grepl("::.*[^:]:[^:]", x_select))){
  				# period::1:treat
  				# Note that we always put 'treat' on the left
  				second_part = strsplit(x_select[1], "::")[[1]][2]
  				var_right = var_left
  				var_left = gsub(".+:", "", second_part)
  				n_max = nchar(var_right) + 2
  				n_trim = nchar(var_left) + 1

  				group_regex = paste0("%", escape_regex(var_right), "::.+:", escape_regex(var_left))

  			} else if(all(grepl("::", x_select))) {
  				is_inter = FALSE
  				# This is a fixest factor
  				n_max = nchar(var_left) + 2

  			} else {
  				# we need to find out by ourselves...
  				n = min(nchar(x_select[1:2]))
  				x_split = strsplit(substr(x_select[1:2], 1, n), "")
  				start_diff = which.max(x_split[[1]] != x_split[[2]])

  				ok = FALSE
  				for(n_max in n:(nchar(var_left) + 2)){
  					if(all(grepl(substr(x_select[1], 1, n_max), x_select, fixed = TRUE))){
  						ok = TRUE
  						break
  					}
  				}

  				if(!ok) next

  				var_right = gsub(".+:", "", substr(x_select[1], 1, n_max))
  			}

  			if(is_inter){
  				v_name = dict_apply(c(var_left, var_right), dict)
  				group_name =replace_and_make_callable("__x__ %*% (__y__ == ldots)", list(x = v_name[1], y = v_name[2]), text_as_expr = TRUE)

  				if(is.null(group_regex)){
  					group[[group_name]] = escape_regex(paste0("%", var_left, ":", var_right))
  				} else {
  					group[[group_name]] = group_regex
  				}

  			} else {
  				v_name = dict_apply(var_left, dict)
  				group_name =replace_and_make_callable("__x__", list(x = v_name), text_as_expr = TRUE)

  				group[[group_name]] = paste0("%^", escape_regex(var_left))

  			}

  			# We update the labels
  			x_labels[qui_select] = substr(x_select, n_max + 1, nchar(x_select) - n_trim)
  		}
  	}
  	## Skip this iplot dictionary step and defer to actual plotting step.
  	## Reason: We want to preserve the numeric x labs for correct order in
  	## later plot (and factors won't work since this makes lines and ribbon
  	## connections problematic).
  # } else if (is_iplot) {
  # 	# apply dictionary to iplot x values
  # 	d[["x"]] = dict_apply(d[["x"]], dict = dict)
  }

  IS_GROUP = !identical(group, "auto") && !missing(group) && !is.null(group) && length(group) > 0 && !is.null(x_labels)


  # This means there are groups!
  if(IS_GROUP){
  	d[["group_var"]] = ""

  	# We perform basic checks on the group
  	if(!is.list(group)) stop("Argument 'group' must be a list.")

  	for(i in seq_along(group)){
  		my_group = group[[i]]

  		# We just take care of the special group + cleaning case
  		# when group starts with ^^
  		if(length(my_group) == 1 && is.character(my_group) && substr(my_group, 1, 2) == "^^") {


  			if(grepl("^%", my_group)){
  				qui = grepl(gsub("^%", "", my_group), x_labels_raw)
	  		} else {
	  			qui = grepl(my_group, x_labels)
	  		}


	  		if(!any(qui)){
	  			warning("In argument 'group', the pattern: \"", my_group, "\", did not match any coefficient name.")
	  			group[[i]] = NULL
	  			if(length(group) == 0) IS_GROUP = FALSE
	  			next
	  		}

	  		group[[i]] = range(which(qui))
	  		x_labels = gsub(my_group, "", x_labels)
  		}
  		## end of ^^ special case check

  		group_name = names(group)[i]
  		my_group = group[[i]] # reset my_group var for checking and formatting

  		# formatting properly
  		if(is.numeric(my_group)){
  			g_start = min(my_group)
  			g_end = max(my_group)

  			if(any(my_group %% 1 != 0)){
  				stop("The elements of the argument 'group' must be integers (e.g. group=list(group_name=1:2)). Currently they are numeric but not integers. Alternatively, you could use coefficient names (see details).")
  			}

  			if(g_start < 1){
  				warning("The elements of the argument 'group' must be integers ranging from 1 to the number of coefficients. The value of ", g_start, " has been replaced by 1.")
  			}

  			# if(g_end > nrow(prms)){
  			if(g_end > nrow(d)){ ## GM changed
  				warning("The elements of the argument 'group' must be integers ranging from 1 to the number of coefficients (here ", g_end, "). The value of ", g_end, " has been replaced by ", g_end, ".")
  			}

  		} else {

  			if(!is.character(my_group)){
  				stop("The elements of the argument 'group' must be either: i) a character string of length 1 or 2, or ii) integers. Currently it is not character nor numeric.\nExample of valid use: group=list(group_name=\"pattern\"), group=list(group_name=c(\"var_start\", \"var_end\")), group=list(group_name=1:2))")
  			}

  			if(!length(my_group) %in% 1:2){
  				stop("The elements of the argument 'group' must be either: i) a character string of length 1 or 2, or ii) integers. Currently it is a character of length ", length(my_group), ".\nExample of valid use: group=list(group_name=\"pattern\"), group=list(group_name=c(\"var_start\", \"var_end\")), group=list(group_name=1:2))")
  			}

  			if(length(my_group) == 1){
  				# This is a pattern
  				if(grepl("^%", my_group)){
  					qui = grepl(gsub("^%", "", my_group), x_labels_raw)
  				} else {
  					qui = grepl(my_group, x_labels)
  				}

  				if(!any(qui)){
  					warning("In argument 'group', the pattern: \"", my_group, "\", did not match any coefficient name.")
  					next
  				}

  			} else {
  				# This is a pattern
  				check = c(FALSE, FALSE)
  				qui = rep(FALSE, length(x_labels))
  				for(i in 1:2){
  					val = my_group[i]
  					if(grepl("^%", val)){
  						val = gsub("^%", "", val)
  						check = val %in% x_labels_raw
  						qui = qui | x_labels_raw %in% val
  					} else {
  						check = val %in% x_labels
  						qui = qui | x_labels %in% val
  					}
  				}

  				check = my_group %in% x_labels
  				if(!all(check)){
  					warning("In argument 'group', the value", dreamerr::enumerate_items(my_group[check], "s.quote"), ", did not match any coefficient name.")
  					next
  				}

  			}

  			qui_nb = which(qui)

  			g_start = min(qui_nb)
  			g_end = max(qui_nb)
  		}

  		if(!is.null(group_name)){
  			if(grepl("^&", group_name)){
  				group_name = eval(str2lang(gsub("^&", "", group_name)))
  				group_var = gsub("==", "=", gsub("\\%\\*\\%", "\u00D7", gsub("ldots", "...", gsub("\"", "", sprintf("%s", deparse1(group_name))))))
  			} else {
  				group_var = group_name
  			}

	  		d[["group_var"]][qui] = group_var
  		} else {
  			d[["group_var"]][qui] = strrep(" ", i)
  		}

  	}

  	d[["x"]] = factor(x_labels, levels = unique(x_labels))
  	d[["group_var"]] = factor(d[["group_var"]], levels = unique(d[["group_var"]]))
  	attr(d, "has_groups") = TRUE

  }


  if (is.numeric(.aggr_es)) {
  	ea = aggr_es(object, period = .aggr_es)
  	d$aggr_eff = NA
  	# d$aggr_eff[match(.aggr_es, )]
  	d$aggr_eff[match(.aggr_es, d$estimate_names)] = ea$estimate[1]
  } else if (.aggr_es != "none") {
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

  row.names(d) = NULL

  return(d)

}



#' @describeIn iplot_data Internal function for grabbing and preparing coefplot data
coefplot_data = function(
	object,
	.ci_level = 0.95,
	.keep = NULL,
	.drop = NULL,
	.group = "auto",
	.dict = fixest::getFixest_dict(),
	.internal.only.i = FALSE,
	.i.select = 1,
	.aggr_es = "none",
	.vcov = NULL,
	.cluster = NULL,
	.se = NULL
) {

	iplot_data(
		object,
		.ci_level = .ci_level, .dict = .dict,
		.keep = .keep, .drop = .drop,
		.internal.only.i = .internal.only.i, .group = .group,
		.vcov = .vcov, .cluster = .cluster, .se = .se
		)

	}
