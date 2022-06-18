#' @title ggplots confidence intervals and point estimates
#'
#' @description Plots the `ggplot2` equivalent of `fixest::iplot()`. Many of the
#'   arguments are the same. As per the latter's description:
#'   This function plots the results of estimations (coefficients and confidence
#'   intervals). The function restricts the output to variables created with
#'   `i`, either interactions with factors or raw factors.
#' @param object A model object of class `fixest` or `fixest_multi`, or a list
#'   thereof.
#' @param geom_style Character string. One of `c('pointrange', 'errorbar', 'ribbon')`
#'   describing the preferred geometric representation of the coefficients.
#' @param multi_style Character string. One of `c('dodge', 'facet')`, defining
#'   how multi-model objects should be presented.
#' @param facet_args A list of arguments passed down to `ggplot::fact_wrap()`.
#'   E.g. `facet_args = list(ncol = 2, scales = 'free_y')`. Only used if
#'   `multi_style = 'facet'`.
#' @param theme ggplot2 theme. Defaults to `theme_linedraw()` with some minor
#'   adjustments, such as centered plot title. Can also be defined on an
#'   existing ggiplot object to redefine theme elements. See examples.
#' @param ... Arguments passed down, or equivalent, to the corresponding
#'   `fixest::iplot()` arguments. Currently used are:
#'   * `main`, `xlab`, and `ylab` for setting the plot title, x- and y-axis labels, respectively.
#'   * `zero` and `zero.par` for defining or adjusting the zero line.
#'   * `ref.line` and `ref.line.par` for defining or adjusting the vertical reference line.
#'   * `pt.pch` and `pt.join` for overriding the default point estimate shapes and joining them, respectively.
#'   * `col` for manually defining line, point, and ribbon colours.
#'   * `ci_level` for changing the desired confidence level (default = 0.95).
#'   Note that multiple levels are allowed, e.g. `ci_level = c(0.8, 0.95)`, but
#'   it is recommended to set the `geom_style` argument to either "errorbar" or
#'   "ribbon" for these to be visible.
#'   * `dict` a dictionary for overriding coefficient names.
#' @details This function generally tries to mimic the functionality and (where
#'   appropriate) arguments of `fixest::iplot()` as closely as possible.
#'   However, by leveraging the ggplot2 API and infrastructure, it is able to
#'   support some more complex plot arrangements out-of-the-box that would be
#'   more difficult to achieve using the base `iplot()` alternative.
#' @seealso [fixest::iplot()].
#' @return A ggplot2 object.
#' @import ggplot2
#' @export
#' @examples
#' # We'll also load fixest to estimate the actual models that we're plottig.
#' library(fixest)
#' library(ggiplot)
#'
#' # These examples borrow from the fixest::iplot() documentation and the
#' # introductory package vignette.
#'
#' #
#' ## Example 1: Vanilla TWFE
#' #
#'
#' data(base_did)
#' base_inter = base_did
#'
#' est_did = feols(y ~ x1 + i(period, treat, 5) | id+period, base_inter)
#' ggiplot(est_did)
#'
#' # Comparison with iplot defaults
#' iplot(est_did)
#' ggiplot(est_did, geom = 'errorbar') # closer iplot original
#'
#' # Many of the arguments work the same as in iplot()
#' iplot(est_did, pt.join = TRUE)
#' ggiplot(est_did, pt.join = TRUE, geom_style = 'errorbar')
#'
#' # Plots can be customized and tweaked easily
#' ggiplot(est_did, geom_style = 'ribbon')
#' ggiplot(est_did, geom_style = 'ribbon', col = 'orange')
#'
#' # Multiple confidence interval levels are supported, but the geom_style
#' # argument should be set to either "errorbar" or "ribbon" for these to be
#' # visible. (The "pointrange" default doesn't make the differences clear.)
#' ggiplot(est_did, geom_style = 'errorbar', ci_level = c(.8, .95))
#'
#' #
#' # Example 2: Multiple estimation (i)
#' #
#'
#' # We'll demonstrate using the staggered treatment example from the
#' # introductory fixest vignette.
#'
#' data(base_stagg)
#' est_twfe = feols(y ~ x1 + i(time_to_treatment, treated, ref = c(-1, -1000)) | id + year, base_stagg)
#' est_sa20 = feols(y ~ x1 + sunab(year_treated, year) | id + year, base_stagg)
#'
#' ggiplot(list('TWFE' = est_twfe, 'Sun & Abraham (2020)' = est_sa20),
#'         main = 'Staggered treatment', ref.line = -1, pt.join = TRUE)
#'
#' # If you don't like the presentation of 'dodged' models in a single frame,
#' # then it easy to facet them instead using multi_style = 'facet'.
#' ggiplot(list('TWFE' = est_twfe, 'Sun & Abraham (2020)' = est_sa20),
#'         main = 'Staggered treatment', ref.line = -1, pt.join = TRUE,
#'         multi_style = 'facet')
#'
#' #
#' # Example 3: Multiple estimation (ii)
#' #
#'
#' # An area where ggiplot shines is in complex multiple estimation cases, such
#' # as lists of fixest_multi objects. To illustrate, let's add a split variable
#' # (group) to our staggered dataset.
#' base_stagg_grp = base_stagg
#' base_stagg_grp$grp = ifelse(base_stagg_grp$id %% 2 == 0, 'Evens', 'Odds')
#'
#' # Now re-run our two regressions from earlier, but splitting the sample to
#' # generate fixest_multi objects.
#' est_twfe_grp = feols(y ~ x1 + i(time_to_treatment, treated, ref = c(-1, -1000)) |
#'                      id + year, base_stagg_grp, split = ~ grp)
#' est_sa20_grp = feols(y ~ x1 + sunab(year_treated, year) |
#'                      id + year, base_stagg_grp, split = ~ grp)
#'
#' # ggiplot combines with list of multi-estimation objects without a problem...
#' ggiplot(list('TWFE' = est_twfe_grp, 'Sun & Abraham (2020)' = est_sa20_grp),
#'         ref.line = -1, main = 'Staggered treatment: Split multi-sample')
#'
#' # ... but is even better when we use faceting instead of dodged errorbars.
#' # Let's use this an opportunity to construct a fancy plot that invokes some
#' # additional arguments and ggplot theming.
#' ggiplot(list('TWFE' = est_twfe_grp, 'Sun & Abraham (2020)' = est_sa20_grp),
#'         ref.line = -1,
#'         main = 'Staggered treatment: Split multi-sample',
#'         xlab = 'Time to treatment',
#'         multi_style = 'facet',
#'         geom_style = 'ribbon',
#'         theme = theme_minimal() +
#'            theme(text = element_text(family = 'HersheySans'),
#'                  plot.title = element_text(hjust = 0.5),
#'                  legend.position = 'none'))
#'
#' #
#' # Aside on theming and scale adjustments
#' #
#'
#' # Setting the theme inside the `ggiplot()` call is optional and not strictly
#' # necessary, since the ggplot2 API allows programmatic updating of existing
#' # plots. E.g.
#' last_plot() + labs(caption = 'Note: Super fancy plot brought to you by ggiplot')
#' last_plot() + theme_void() + scale_colour_brewer(palette = 'Set1')
#' # etc.
#'
#' #
#' # Aside on dictionaries
#' #
#'
#' # Dictionaries work similarly to iplot. Simple example:
#'
#' base_inter$letter = letters[base_inter$period]
#' est_letters = feols(y ~ x1 + i(letter, treat, 'e') | id+letter, base_inter)
#'
#' ggiplot(est_letters) # No dictionary
#'
#' # Dictionary for capitalising the letters
#' dict = LETTERS[1:10]; names(dict) = letters[1:10]
#'
#' # You can either set the dictionary directly in the plot call.
#' ggiplot(est_letters, dict=dict)
#'
#' # Or, set it globally using the setFixest_dict macro
#' setFixest_dict(dict)
#' ggiplot(est_letters)
#'
#' setFixest_dict() # reset
#'
ggiplot =
	function(object,
					 geom_style = c('pointrange', 'errorbar', 'ribbon'),
					 multi_style = c('dodge', 'facet'),
					 facet_args = NULL,
					 theme = NULL,
					 ...) {

		geom_style = match.arg(geom_style)
		multi_style = match.arg(multi_style)

		dots = list(...)
		## Defaults
		ci_level = if (!is.null(dots$ci_level)) dots$ci_level else 0.95
		main = if (!is.null(dots$main)) dots$main else NULL
		xlab = if (!is.null(dots$xlab)) dots$xlab else NULL
		ylab = if (!is.null(dots$ylab)) dots$ylab else NULL
		dict = if (!is.null(dots$dict)) dots$dict else fixest::getFixest_dict()
		col = if (!is.null(dots$col)) dots$col else NULL
		pt.pch = if (!is.null(dots$pt.pch)) dots$pt.pch else NULL
		pt.join = if (!is.null(dots$pt.join)) dots$pt.join else FALSE
		zero = if (!is.null(dots$zero)) dots$zero else TRUE
		zero.par = if (!is.null(dots$zero.par))	dots$zero.par else list(col = 'black',
																																		lty = 1,
																																		lwd = 0.3)
		ref.line = if (!is.null(dots$ref.line)) dots$ref.line else 'auto'
		ref.line.par = if (!is.null(dots$ref.line.par)) dots$ref.line.par else list(col = 'black',
																																								lty = 2,
																																								lwd = 0.3)

		iplot_data = function(object, .ci_level = ci_level, .dict = dict) {
			p = fixest::iplot(object, only.params = TRUE, ci_level = .ci_level, dict = .dict)
			d = p$prms
			if (class(object)=='fixest_multi') {
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
				if (class(p$labels)=='integer') p$labels = as.numeric(p$labels) ## catch
				if (!identical(d$x, p$labels)) d$x = factor(d$x, labels = p$labels)
				# d$dep_var = paste(object$call$fml[[2]])
				d$dep_var = paste(object$fml[[2]])
			}
			d$ci_level = .ci_level
			return(d)
		}

		if (class(object) %in% c('fixest', 'fixest_multi')) {
			if (length(ci_level)==1) {
				data = iplot_data(object)
			} else {
				data = lapply(ci_level, function(ci_l) iplot_data(object, .ci_level = ci_l))
				data = do.call("rbind", data)
			}

			data$group = data$id
			if (class(object)=='fixest_multi') {
				if (length(unique(data$dep_var)) > 1) {
					if (all(data$dep_var==data$id)) {
						fct_vars = ~ dep_var
					} else {
						fct_vars = ~ dep_var + id
					}
					n_fcts = length(unique(data$dep_var))
				} else {
					fct_vars = ~ id
					n_fcts = length(unique(data$id))
				}
			} else {
				multi_style = 'none'
			}
		}
		# if (class(object) %in% c('fixest', 'fixest_multi')) {
		# 	if (class(object)=='fixest_multi') {
		# 		data = do.call('rbind', lapply(object, iplot_data))
		# 		fct_vars = ~ id
		# 	} else {
		# 		data = iplot_data(object)
		# 		multi_style = 'none'
		# 	}
		# 	data$group = data$id
		# }

		if (class(object)=='list') {
			# data = lapply(object, iplot_data)
			if (length(ci_level)==1) {
				data = lapply(object, iplot_data)
			} else {
				data = lapply(ci_level, function(ci_l) lapply(object, iplot_data, .ci_level = ci_l))
				data = do.call(function(...) Map("rbind", ...), data)
			}
			nms = names(object)
			if (is.null(nms)) {
				if ('fixest' %in% unlist(lapply(object, class))) {
					nms = paste('Model', seq_along(object))
				} else {
					nms = paste('Group', seq_along(object))
				}
			}

			nms = as.character(mapply(rep, nms, sapply(data, nrow)))
			data = do.call('rbind', data)
			data$group = nms
			rownames(data) = NULL
			if (length(unique(data$id))==1) {
				fct_vars = ~ group
				n_fcts = length(unique(data$group))
			} else {
				fct_vars = ~ id + group
				n_fcts = length(unique(data$group)) * length(unique(data$id))
			}
			if (length(unique(data$dep_var)) > 1) {
				fct_vars = stats::update(fct_vars, ~ dep_var + .)
				n_fcts = n_fcts * length(unique(data$dep_var))
			}
			if (is.null(facet_args$ncol)) facet_args$ncol = length(unique(data$group))
		}

		if (is.null(xlab)) xlab = sub('::.*', '', data$estimate_names_raw[1])
		if (!is.null(ref.line)) {
			if (ref.line=='auto')	ref.line = data$x[which(data$is_ref)[1]]
		}
		if (is.null(ylab)) ylab = paste0('Estimate and ', oxford(paste0(ci_level*100, '%')), ' Conf. Int.')
		if (is.null(main)) main = paste0('Effect on ', oxford(unique(data$dep_var)))

		if (multi_style=='facet') {
			facet_defaults = formals(facet_wrap)
			facet_defaults$facets = fct_vars
			if (!is.null(facet_args)) {
				facet_rep_ind = match(names(facet_args), names(facet_defaults))
				facet_defaults = replace(facet_defaults, facet_rep_ind, facet_args)
			}
			facet_args = facet_defaults
		}

		if (!is.null(pt.pch)) {
			# data$group = factor(data$group)
			ugroups = unique(data$group)
			pt_values_df = data.frame(group = ugroups, values = pt.pch)[1:length(ugroups), ]
			pt_values = pt_values_df$values
			names(pt_values) = pt_values_df$group
		}

		ptsize = 2.5
		if (multi_style=='facet') {
			ptsize = ptsize - 0.25*n_fcts
		}

		if (multi_style=='none') {
			if (is.null(col)) {
				gg = ggplot(data, aes(x = .data$x, y = .data$estimate,
															ymin = .data$ci_low, ymax = .data$ci_high,
															group = .data$ci_level))
			} else {
				gg = ggplot(data, aes(x = .data$x, y = .data$estimate,
															ymin = .data$ci_low, ymax = .data$ci_high,
															col = col, fill = col,
															group = .data$ci_level))
			}
			} else {
				gg = ggplot(data, aes(x = .data$x, y = .data$estimate,
															ymin = .data$ci_low, ymax = .data$ci_high,
															fill = .data$group, col = .data$group,
															shape = .data$group,
															group = .data$ci_level))
			}

		gg =
			gg +
			geom_vline(xintercept=ref.line, col=ref.line.par$col, lwd=ref.line.par$lwd, lty=ref.line.par$lty) +
			{
				if (zero) {
					geom_hline(yintercept=0, col=zero.par$col, lwd=zero.par$lwd, lty=zero.par$lty)
				}
			} +
			{
				if (geom_style %in% c('pointrange', 'errorbar') & multi_style %in% c('none', 'facet')) {
					if (geom_style=='pointrange') {
						geom_linerange()
					} else if (multi_style=='facet') {
						geom_errorbar(width = 0.2*n_fcts)
					} else {
						geom_errorbar(width = 0.2)
					}
				}
			} +
			{
				if (geom_style %in% c('pointrange', 'errorbar') & multi_style=='dodge') {
					if (geom_style=='pointrange') {
						geom_linerange(position = position_dodge2(width = 0.5, padding = 0.5))
					} else {
						geom_errorbar(width = 0.2, position = position_dodge(width = 0.5))
					}
				}
			} +
			{
				if (geom_style=='ribbon') {
					if (multi_style=='dodge') {
						geom_ribbon(alpha = 0.4, col = NA, position = position_dodge2(width = 0.5, padding = 0.5))
					} else {
						geom_ribbon(alpha = 0.4, col = NA)
					}
				}
			} +
			{
				if (geom_style=='ribbon' || pt.join) {
					if (multi_style=='dodge') {
						geom_line(position = position_dodge2(width = 0.5, padding = 0.5))
					} else {
						geom_line()
					}

				}
			} +
			{
				if (!(!is.null(pt.pch) && is.na(pt.pch))) {
					if (multi_style=='none' && !is.null(pt.pch)) {
						if (multi_style=='dodge') {
							geom_point(shape = pt.pch, size = ptsize, position = position_dodge2(width = 0.5, padding = 0.5))
						} else {
							geom_point(shape = pt.pch, size = ptsize)
						}
					} else {
						if (multi_style=='dodge') {
							geom_point(size = ptsize, position = position_dodge2(width = 0.5, padding = 0.5))
						} else {
							geom_point(size = ptsize)
						}
					}
				}
			} +
			scale_color_brewer(palette = 'Set2', aesthetics = c('colour', 'fill')) +
			{
				if (!is.null(col)) {
					if (multi_style=='none') {
						list(scale_colour_manual(values = col, aesthetics = c('colour', 'fill')),
								 guides(col = 'none', fill = 'none'))
					} else {
						scale_colour_manual(values = col, aesthetics = c('colour', 'fill'))
					}
				}
			} +
			{
				if (!is.null(pt.pch) && !is.na(pt.pch)) {
					if (multi_style=='none') {
						list(scale_shape_manual(values = pt_values), guides(shape = 'none'))
					} else {
						scale_shape_manual(values = pt_values)
					}
				}
			} +
			labs(x = xlab, y = ylab, title = main) +
			{
				if (multi_style=='facet') facet_wrap(facets = facet_args$facets,
																						 nrow = facet_args$nrow,
																						 ncol = facet_args$ncol,
																						 scales = facet_args$scales,
																						 shrink = facet_args$shrink,
																						 labeller = facet_args$labeller,
																						 as.table = facet_args$as.table,
																						 switch = facet_args$switch,
																						 drop = facet_args$drop,
																						 dir = facet_args$dir,
																						 strip.position = facet_args$strip.position)
			}

		if (!is.null(theme)) {
			gg =
				gg +
				theme

		} else {
			gg =
				gg +
				theme_linedraw() +
				theme(#panel.grid.minor = element_blank(),
					plot.title = element_text(hjust = 0.5),
					legend.position = 'bottom', legend.title = element_blank())
		}

		return(gg)
	}
