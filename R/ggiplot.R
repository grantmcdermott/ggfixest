#' @describeIn ggcoefplot This function plots the results of estimations
#'   (coefficients and confidence intervals). The function `ggiplot` restricts
#'   the output to variables created with i, either interactions with factors or
#'   raw factors.
#'
#' @export
ggiplot = function(
	object,
	geom_style = c('pointrange', 'errorbar', 'ribbon'),
	multi_style = c('dodge', 'facet'),
	aggr_eff = NULL,
	aggr_eff.par = list(col = 'grey50', lwd = 1, lty = 1),
	facet_args = NULL,
	theme = NULL,
	...
	) {

  geom_style = match.arg(geom_style)
  multi_style = match.arg(multi_style)
  # aggr_eff = match.arg(aggr_eff)
  if (is.null(aggr_eff)) aggr_eff = "none"
  aggr_eff.par = utils::modifyList(list(col = "grey50", lwd = 1, lty = 1), aggr_eff.par)

  dots = list(...)
  ## Defaults
  is_iplot = if (!is.null(dots[["is_iplot"]])) dots[["is_iplot"]] else TRUE
  group = if (!is.null(dots[["group"]])) dots[["group"]] else "auto"
  keep = if (!is.null(dots[["keep"]])) dots[["keep"]] else NULL
  drop = if (!is.null(dots[["drop"]])) dots[["drop"]] else NULL
  i.select = if (!is.null(dots[["i.select"]])) dots[["i.select"]] else 1
  ci_level = if (!is.null(dots[["ci_level"]])) dots[["ci_level"]] else 0.95
  ci.width = if (!is.null(dots[["ci.width"]])) dots[["ci.width"]] else 0.2
  ci.fill.par  = list(col = 'lightgray', alpha = 0.3) ## Note: The col arg is going be ignored anyway
  if (!is.null(dots[['ci.fill.par']])) ci.fill.par = utils::modifyList(ci.fill.par, dots[['ci.fill.par']])
  main         = if (!is.null(dots[['main']])) dots[['main']] else NULL
  xlab         = if (!is.null(dots[['xlab']])) dots[['xlab']] else NULL
  ylab         = if (!is.null(dots[['ylab']])) dots[['ylab']] else NULL
  dict         = if (!is.null(dots[['dict']])) dots[['dict']] else fixest::getFixest_dict()
  col          = if (!is.null(dots[['col']])) dots[['col']] else NULL
  pt.pch       = if (!is.null(dots[['pt.pch']])) dots[['pt.pch']] else NULL
  pt.size      = if (!is.null(dots[['pt.size']])) dots[['pt.size']] else 2.5    # Hard-coded as 2.5 based on dicussion here: https://github.com/grantmcdermott/ggiplot/pull/27#issuecomment-1837850786
  pt.join      = if (!is.null(dots[['pt.join']])) dots[['pt.join']] else FALSE
  ## hold off deciding zero line until we have seen the data
  # zero         = if (!is.null(dots[['zero']])) dots[['zero']] else TRUE
  # zero.par = list(col = 'black', lty = 1, lwd = 0.3)i
  # if (!is.null(dots[['zero.par']])) zero.par = utils::modifyList(zero.par, dots[['zero.par']])
  ref.line     = if (!is.null(dots[['ref.line']])) dots$ref.line else 'auto'
  if (isFALSE(ref.line)) ref.line = NULL
  ref.line.par = list(col = "black", lty = 2, lwd = 0.3)
  if (!is.null(dots[["ref.line.par"]])) ref.line.par = utils::modifyList(ref.line.par, dots[["ref.line.par"]])

  # VCOV adjustments (if any)
  vcov    = if (!is.null(dots[['vcov']])) dots[['vcov']] else NULL
  cluster = if (!is.null(dots[['cluster']])) dots[['cluster']] else NULL
  se      = if (!is.null(dots[['se']])) dots[['se']] else NULL

  # The next few blocks grab the underlying iplot/coefplot data, contingent on the
  # object that was passed into the function (i.e. fixest, fixest_multi, or
  # list)
  iplot_data_func = ifelse(isTRUE(is_iplot), iplot_data, coefplot_data)

  if (inherits(object, c("fixest", "fixest_multi"))) {

      if (length(ci_level) == 1) {
          data = iplot_data_func(
          	object,
          	.ci_level = ci_level, .dict = dict, .aggr_es = aggr_eff,
          	.keep = keep, .drop = drop, .group = group, .i.select = i.select,
          	.vcov = vcov, .cluster = cluster, .se = se
          	)
      } else {
          data = lapply(
              ci_level,
              function(ci_l) iplot_data_func(
              	object,
              	.ci_level = ci_l, .dict = dict, .aggr_es = aggr_eff,
              	.keep = keep, .drop = drop, .group = group, .i.select = i.select,
              	.vcov = vcov, .cluster = cluster, .se = se
              	)
          )
          data = do.call("rbind", data)
      }

      data$group = data$id
      if (inherits(object, "fixest_multi")) {
          fct_vars = colnames(fixest::models(object)[, -1, drop = FALSE]) # drop id col
          fct_vars = setdiff(fct_vars, "sample.var") # also drop sample.var col
          fct_vars = stats::reformulate(fct_vars)
          n_fcts = length(unique(data$id))
      } else {
          multi_style = "none"
      }
  }

  if (inherits(object, "list")) {
      if (length(ci_level) == 1) {
          data = lapply(
              object, iplot_data_func,
              .ci_level = ci_level, .dict = dict, .aggr_es = aggr_eff,
              .group = group, .i.select = i.select,
              .vcov = vcov, .cluster = cluster, .se = se
          )
      } else {
          data = lapply(ci_level, function(ci_l) {
              lapply(
              	object, iplot_data_func,
              	.ci_level = ci_l,
              	.dict = dict, .aggr_es = aggr_eff,
              	.group = group, .i.select = i.select,
              	.vcov = vcov, .cluster = cluster, .se = se
              )
          })
          data = do.call(function(...) Map("rbind", ...), data)
      }

      nms = names(object)
      if (is.null(nms)) {
          if ("fixest" %in% unlist(lapply(object, class))) {
              nms = paste("Model", seq_along(object))
          } else {
              nms = paste("Group", seq_along(object))
          }
      }
      for (zz in seq_along(data)) {
          data[[zz]]$group = nms[[zz]]
      }
      rm(zz)
      data = do.call("rbind", data)
      rownames(data) = NULL
      if (length(unique(data$id)) == 1) {
          fct_vars = ~group
          n_fcts = length(unique(data$group))
      } else {
          fct_vars = ~ id + group
          n_fcts = length(unique(data$group)) * length(unique(data$id))
      }
      if (length(unique(data$lhs)) > 1) {
          fct_vars = stats::update(fct_vars, ~ lhs + .)
          n_fcts = n_fcts * length(unique(data$lhs))
      }
      if (is.null(facet_args$ncol)) facet_args$ncol = length(unique(data$group))
  }

  # Prep data for nested grouping
  has_groups = (!is.null(attributes(data)[["has_groups"]]) && isTRUE(attributes(data)[["has_groups"]]))
  if (isTRUE(has_groups)) {

  	key <- unique(data[c("x", "group_var")])
  	key <- key[order(key[["group_var"]], key[["x"]]), , drop = FALSE]
  	xlimits <- as.character(key[["x"]])

  	rle <- rle(as.character(key[["group_var"]]))
  	keep <- nzchar(rle$values)
  	end <- cumsum(rle$lengths)
  	start <- end - rle$lengths + 1L

  	key <- legendry::key_range_manual(
  		start = xlimits[start[keep]],
  		end   = xlimits[end[keep]],
  		name  = rle$values[keep],
  		level = 1L
  	)

  	data[["group_var"]] <- NULL
  }

  yrange = range(c(data[["ci_low"]], data[["ci_high"]]), na.rm = TRUE)
  spans_zero = any(yrange > 0) && any(yrange < 0)
  zero = if (!is.null(dots[['zero']])) {
  	dots[['zero']] }
  else if (is_iplot || spans_zero) {
  	TRUE
  } else {
  	FALSE
  }
  zero.par = list(col = 'black', lty = 1, lwd = 0.3)
  if (!is.null(dots[['zero.par']])) zero.par = utils::modifyList(zero.par, dots[['zero.par']])



  if (multi_style == "dodge") ci.width = ci.width * n_fcts

  if (is.null(xlab) & isTRUE(is_iplot)) xlab = sub("::.*", "", data$estimate_names_raw[1])
  if (!is.null(ref.line)) {
      if (ref.line == "auto" && isTRUE(is_iplot)) ref.line = data$x[which(data$is_ref)[1]]
  }
  if (is.null(ylab)) ylab = paste0("Estimate and ", oxford(paste0(ci_level * 100, "%")), " Conf. Int.")
  if (is.null(main)) main = paste0("Effect on ", oxford(unique(data$lhs)))

  if (multi_style == "facet") {
      facet_defaults = formals(facet_wrap)
      facet_defaults$facets = fct_vars
      if (!is.null(facet_args)) {
          facet_rep_ind = match(names(facet_args), names(facet_defaults))
          facet_defaults = replace(facet_defaults, facet_rep_ind, facet_args)
      }
      facet_args = facet_defaults
  }

  if (!is.null(pt.pch)) {
      ugroups = unique(data$group)
      pt_values_df = data.frame(group = ugroups, values = pt.pch)[seq_along(ugroups), ]
      pt_values = pt_values_df$values
      names(pt_values) = pt_values_df$group
  }


  if (multi_style == "facet") {
      pt.size = pt.size - 0.25 * n_fcts
  }

  if (multi_style == "none") {
      if (is.null(col)) {
          gg = ggplot(
              data,
              aes(
                  x = .data$x, y = .data$estimate,
                  ymin = .data$ci_low, ymax = .data$ci_high
              )
          )
      } else {
          gg = ggplot(
              data,
              aes(
                  x = .data$x, y = .data$estimate,
                  ymin = .data$ci_low, ymax = .data$ci_high,
                  col = col, fill = col
              )
          )
      }
  } else {
      gg = ggplot(
          data,
          aes(
              x = .data$x, y = .data$estimate,
              ymin = .data$ci_low, ymax = .data$ci_high,
              fill = .data$group, col = .data$group,
              shape = .data$group
          )
      )
  }


  gg =
      gg +
      geom_vline(xintercept = ref.line, col = ref.line.par$col, lwd = ref.line.par$lwd, lty = ref.line.par$lty) +
      {
          if (zero) {
              geom_hline(yintercept = 0, col = zero.par$col, lwd = zero.par$lwd, lty = zero.par$lty)
          }
      } +
      {
          if (is.numeric(aggr_eff) || aggr_eff != "none") {
              geom_line(aes(y = aggr_eff), col = aggr_eff.par$col, lwd = aggr_eff.par$lwd, lty = aggr_eff.par$lty)
          }
      } +
   {
       if (geom_style %in% c("pointrange", "errorbar") & multi_style %in% c("none", "facet")) {
           if (geom_style == "pointrange" || length(ci_level) != 1) {
               if (length(ci_level) == 1) {
                   geom_linerange()
               } else {
                   list(
                       geom_linerange(data = ~ subset(.x, ci_level == max(ci_level))),
                       geom_errorbar(data = ~ subset(.x, ci_level != max(ci_level)), width = ci.width)
                   )
               }
           } else if (multi_style == "facet") {
               geom_errorbar(width = ci.width * n_fcts)
           } else {
               geom_errorbar(width = ci.width)
           }
       }
   } +
   {
       if (geom_style %in% c("pointrange", "errorbar") & multi_style == "dodge") {
           if (geom_style == "pointrange" || length(ci_level) != 1) {
               if (length(ci_level) == 1) {
                   geom_linerange(position = position_dodge2(width = ci.width, padding = ci.width))
               } else {
                   if (inherits(object, "list") & length(object) > 1) {
                       list(
                           geom_linerange(
                               data = ~ subset(.x, ci_level == max(ci_level)),
                               position = position_dodge2(width = ci.width, padding = ci.width)
                           ),
                           geom_errorbar(
                               data = ~ subset(.x, ci_level != max(ci_level)),
                               width = ci.width,
                               position = position_dodge2(width = ci.width, padding = ci.width)
                           )
                       )
                   } else {
                       list(
                           geom_linerange(
                               data = ~ subset(.x, ci_level == max(ci_level)),
                               position = position_dodge2(width = ci.width, padding = ci.width)
                           ),
                           geom_errorbar(
                               data = ~ subset(.x, ci_level != max(ci_level)),
                               width = ci.width, position = position_dodge(width = ci.width)
                           )
                       )
                   }
               }
           } else {
               geom_errorbar(width = ci.width, position = position_dodge(width = ci.width))
           }
       }
   } +
   {
       if (geom_style == "ribbon") {
           if (multi_style == "dodge") {
               if (length(ci_level) == 1) {
                   geom_ribbon(
                       alpha = ci.fill.par[["alpha"]], col = NA,
                       position = position_dodge2(width = ci.width, padding = ci.width)
                   )
               } else {
                   lapply(
                       ci_level,
                       function(ci) {
                           geom_ribbon(
                               data = ~ subset(.x, ci_level == ci),
                               alpha = ci.fill.par[["alpha"]], col = NA,
                               position = position_dodge2(width = ci.width, padding = ci.width)
                           )
                       }
                   )
               }
           } else {
               if (length(ci_level) == 1) {
                   geom_ribbon(alpha = ci.fill.par[["alpha"]], col = NA)
               } else {
                   geom_ribbon(
                       alpha = ci.fill.par[["alpha"]], col = NA, aes(group = .data$ci_level)
                   )
               }
           }
       }
   } +
   {
       if (geom_style == "ribbon" || pt.join) {
           if (multi_style == "dodge") {
               if (length(ci_level) == 1) {
                   geom_line(
                       # aes(group = paste0(.data$group, .data$id)),
                       # position = position_dodge(width = ci.width)
                       position = position_dodge2(width = ci.width, padding = ci.width)
                   )
               } else {
                   geom_line(
                       data = ~ subset(.x, ci_level == max(ci_level)),
                       aes(group = paste0(.data$group, .data$id)),
                       position = position_dodge2(width = ci.width, padding = ci.width)
                   )
               }
           } else {
               if (length(ci_level) == 1) {
                   geom_line()
               } else {
                   geom_line(aes(group = .data$ci_level))
               }
           }
       }
   } +
   {
       if (!(!is.null(pt.pch) && is.na(pt.pch))) {
           if (multi_style == "none" && !is.null(pt.pch)) {
               if (multi_style == "dodge") {
                   if (length(ci_level) == 1) {
                       geom_point(
                           shape = pt.pch, size = pt.size,
                           position = position_dodge2(width = ci.width, padding = ci.width)
                       )
                   } else {
                       geom_point(
                           data = ~ subset(.x, ci_level == max(ci_level)),
                           shape = pt.pch, size = pt.size,
                           position = position_dodge2(width = ci.width, padding = ci.width)
                       )
                   }
               } else {
                   geom_point(shape = pt.pch, size = pt.size)
               }
           } else {
               if (multi_style == "dodge") {
                   if (length(ci_level) == 1) {
                       geom_point(
                           size = pt.size,
                           position = position_dodge2(width = ci.width, padding = ci.width)
                       )
                   } else {
                       geom_point(
                           data = ~ subset(.x, ci_level == max(ci_level)),
                           size = pt.size,
                           position = position_dodge2(width = ci.width, padding = ci.width)
                       )
                   }
               } else {
                   geom_point(size = pt.size)
               }
           }
       }
   } +
   scale_color_brewer(palette = "Set2", aesthetics = c("colour", "fill")) +
   {
	   if (!is.null(col)) {
		   if (multi_style == "none") {
			   list(
				   scale_colour_manual(values = col, aesthetics = c("colour", "fill")),
				   guides(col = "none", fill = "none")
			   )
		   } else {
			   scale_colour_manual(values = col, aesthetics = c("colour", "fill"))
		   }
	   }
   } +
   {
       if (!is.null(pt.pch) && !is.na(pt.pch)) {
           if (multi_style == "none") {
               list(scale_shape_manual(values = pt_values), guides(shape = "none"))
           } else {
               scale_shape_manual(values = pt_values)
           }
       }
   } +
   {
       if (is.numeric(data$x)) {
       		labels_func = ifelse(is_iplot, function(x) dict_apply(x, dict = dict), waiver)
           if (length(unique(data$x)) == 2) { # nicer gridlines for 2x2 case
               scale_x_continuous(breaks = unique(data$x), labels = labels_func)
           } else {
               scale_x_continuous(breaks = scales::breaks_pretty(), labels = labels_func)
           }
       }
   } +
   labs(x = xlab, y = ylab, title = main) + {
   	if (has_groups) {
   		scale_x_discrete(
   			limits = xlimits,
   			guide = legendry::guide_axis_nested(
   				key = key,
   				theme = theme(legendry.bracket = element_line(linewidth = 0.5))
   			)
   		)
   	}
   } +
   {
       if (multi_style == "facet") {
           facet_wrap(
               facets = facet_args$facets,
               nrow = facet_args$nrow,
               ncol = facet_args$ncol,
               scales = facet_args$scales,
               shrink = facet_args$shrink,
               labeller = facet_args$labeller,
               as.table = facet_args$as.table,
               drop = facet_args$drop,
               dir = facet_args$dir,
               strip.position = facet_args$strip.position
           )
       }
   }


  if (!is.null(theme)) {
      gg = gg + theme
  } else {
      gg = gg +
          theme_linedraw() +
          theme(
              panel.grid.minor = element_line(linetype = "22", colour = "lightgray", linewidth = 0.3),
              panel.grid.major = element_line(linetype = "22", colour = "lightgray", linewidth = 0.3),
              plot.title = element_text(hjust = 0.5),
              legend.position = "bottom", legend.title = element_blank()
          )
  }

  return(gg)

}

