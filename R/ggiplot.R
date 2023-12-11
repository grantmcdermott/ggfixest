#' @title Draw coefficient plots and interaction plots from `fixest` regression
#'   objects.
#'
#' @description Draws the `ggplot2` equivalents of `fixest::coefplot` and
#'   `fixest::iplot`. These "gg" versions do their best to recycle the same
#'   arguments  and plotting logic as their original base counterparts. But they
#'   also support additional features via the `ggplot2` API and infrastructure.
#'   The overall goal remains the same as the original functions. To wit:
#'   `ggcoefplot` plots the results of estimations (coefficients and confidence
#'   intervals). The function `ggiplot` restricts the output to variables
#'   created with `i`, either interactions with factors or raw factors.
#' @md
#' @param object A model object of class `fixest` or `fixest_multi`, or a list
#'   thereof.
#' @param geom_style Character string. One of `c('pointrange', 'errorbar', 'ribbon')`
#'   describing the preferred geometric representation of the coefficients. Note
#'   that ribbon plots not supported for `ggcoefplot`, since we cannot guarantee
#'   a continuous relationship among the coefficients.
#' @param multi_style Character string. One of `c('dodge', 'facet')`, defining
#'   how multi-model objects should be presented.
#' @param aggr_eff A character string indicating whether the aggregated mean
#'   post- (and/or pre-) treatment effect should be plotted alongside the
#'   individual period effects. Should be one of "none" (the default), "post",
#'   "pre", or "both".
#' @param aggr_eff.par List. Parameters of the aggregated treatment effect line,
#'   if plotted. The default values are `col = 'gray50'`, `lwd = 1`, `lty = 1`.
#' @param facet_args A list of arguments passed down to `ggplot::fact_wrap()`.
#'   E.g. `facet_args = list(ncol = 2, scales = 'free_y')`. Only used if
#'   `multi_style = 'facet'`.
#' @param theme ggplot2 theme. Defaults to `theme_linedraw()` with some minor
#'   adjustments, such as centered plot title. Can also be defined on an
#'   existing ggiplot object to redefine theme elements. See examples.
#' @param ... Arguments passed down to, or equivalent to, the corresponding
#'   `fixest::coefplot`/`fixest::iplot` arguments. Note that some of these
#'   require list objects. Currently used are:
#'   * `keep` and `drop` for subsetting variables using regular expressions. The `fixest::iplot` help page includes more detailed examples, but these should generally work as you expect. One useful regexp trick worth mentioning briefly for event studies with many pre-/post-periods is `drop = "[[:digit:]]{2}"`. This will cause the plot to zoom in around single digit pre-/post-periods.
#'   * `group` a list indicating variables to group over. Each element of the list reports the coefficients to be grouped while the name of the element is the group name. Each element of the list can be either: i) a character vector of length 1, ii) of length 2, or iii) a numeric vector. Special patterns such as "^^var_start" can be used to more appealing plotting, where group labels are separated from their subsidiary labels. This can be especially useful for plotting interaction terms. See the Details section of `fixest::coefplot` for more information.
#'   * `i.select` Integer scalar, default is 1. In `ggiplot`, used to select which variable created with `i()` to select. Only used when there are several variables created with `i`. See the Details section of `fixest::iplot` for more information.
#'   * `main`, `xlab`, and `ylab` for setting the plot title, x- and y-axis labels, respectively.
#'   * `zero` and `zero.par` for defining or adjusting the zero line. For
#'   example, `zero.par = list(col = 'orange')`.
#'   * `ref.line` and `ref.line.par` for defining or adjusting the vertical
#'   reference line. For example, `ref.line.par = list(col = 'red', lty = 4)`.
#'   * `pt.pch` and `pt.join` for overriding the default point estimate shapes and joining them, respectively.
#'   * `col` for manually defining line, point, and ribbon colours.
#'   * `ci_level` for changing the desired confidence level (default = 0.95).
#'   Note that multiple levels are allowed, e.g. `ci_level = c(0.8, 0.95)`.
#'   * `ci.width` for changing the width of the extremities of the confidence
#'   intervals. Only used if `geom_style = "errorbar"` (or if multiple CI levels
#'   are requested for the default pointrange style). The default value is 0.2.
#'   * `ci.fill.par` for changing the confidence interval fill. Only used when
#'   `geom_style = "ribbon"` and currently only affects the alpha (transparency)
#'   channel. For example, we can make the CI band lighter with
#'   `ci.fill.par = list(alpha = 0.2)` (the default alpha is 0.3).
#'   * `dict` a dictionary for overriding coefficient names.
#' @details These functions generally try to mimic the functionality and (where
#'   appropriate) arguments of `fixest::coefplot` and `fixest::iplot` as
#'   closely as possible. However, by leveraging the ggplot2 API and
#'   infrastructure, they are able to support some more complex plot
#'   arrangements out-of-the-box that would be more difficult to achieve using
#'   the base `coefplot`/`iplot` alternatives.
#' @seealso [fixest::coefplot()], [fixest::iplot()].
#' @return A ggplot2 object.
#' @import ggplot2 fixest
#' @export
#' @examples
#' library(ggfixest)
#'
#' ##
#' # Author note: The examples that follow deliberately follow the original
#' #   examples from the coefplot/iplot help pages. A few "gg-" specific
#' #   features are sprinkled within, with the final set of examples in
#' #   particular highlighting unique features of this package.
#'
#'
#' #
#' # Example 1: Basic use and stacking two sets of results on the same graph
#' #
#'
#' # Estimation on Iris data with one fixed-effect (Species)
#' est = feols(Petal.Length ~ Petal.Width + Sepal.Length + Sepal.Width | Species, iris)
#'
#' ggcoefplot(est)
#'
#' # Show multiple CIs
#' ggcoefplot(est, ci_level = c(0.8, 0.95))
#'
#' # By default, fixest model standard errors are clustered by the first fixed
#' # effect (here: Species).
#' # But we can easily switch to "regular" standard-errors
#' est_std = summary(est, se = "iid")
#'
#' # You can plot both results at once in the same plot frame...
#' ggcoefplot(list("Clustered" = est, "IID" = est_std))
#' # ... or as separate facets
#' ggcoefplot(list("Clustered" = est, "IID" = est_std), multi_style = "facet") +
#' 	theme(legend.position = "none")
#'
#'
#' #
#' # Example 2: Interactions
#' #
#'
#'
#' # Now we estimate and plot the "yearly" treatment effects
#'
#' data(base_did)
#' base_inter = base_did
#'
#' # We interact the variable 'period' with the variable 'treat'
#' est_did = feols(y ~ x1 + i(period, treat, 5) | id + period, base_inter)
#'
#' # In the estimation, the variable treat is interacted
#' #  with each value of period but 5, set as a reference
#'
#' # ggcoefplot will show all the coefficients:
#' ggcoefplot(est_did)
#'
#'
#' # Note that the grouping of the coefficients is due to 'group = "auto"'
#'
#' # If you want to keep only the coefficients
#' # created with i() (ie the interactions), use ggiplot
#' ggiplot(est_did)
#'
#' # We can see that the graph is different from before:
#' #  - only interactions are shown,
#' #  - the reference is present,
#' # => this is fully flexible
#'
#' ggiplot(est_did, ci_level = c(0.8, 0.95))
#' ggiplot(est_did, ref.line = FALSE, pt.join = TRUE, geom_style = "errorbar")
#' ggiplot(est_did, geom_style = "ribbon", col = "orange")
#' # etc
#'
#' # We can also use a dictionary to replace label values. The dicionary should
#' # take the form of a named vector or list, e.g. c("old_lab1" = "new_lab1", ...)
#'
#' # Let's create a "month" variable
#' all_months = c("aug", "sept", "oct", "nov", "dec", "jan",
#' 							 "feb", "mar", "apr", "may", "jun", "jul")
#' # Turn into a dictionary by providing the old names
#' # Note the implication that treatment occured here in December (5 month in our series)
#' dict = all_months; names(dict) = 1:12
#' # Pass our new dictionary to our ggiplot call
#' ggiplot(est_did, pt.join = TRUE, geom_style = "errorbar", dict = dict)
#'
#' #
#' # What if the interacted variable is not numeric?
#'
#' # let's re-use our all_months vector from the previous example, but add it
#' # directly to the dataset
#' base_inter$period_month = all_months[base_inter$period]
#'
#' # The new estimation
#' est = feols(y ~ x1 + i(period_month, treat, "oct") | id+period, base_inter)
#' # Since 'period_month' of type character, iplot/coefplot both sort it
#' ggiplot(est)
#'
#' # To respect a plotting order, use a factor
#' base_inter$month_factor = factor(base_inter$period_month, levels = all_months)
#' est     = feols(y ~ x1 + i(month_factor, treat, "oct") | id + period, base_inter)
#' ggiplot(est)
#'
#' # dict -> c("old_name" = "new_name")
#' dict = all_months; names(dict) = 1:12; dict
#' ggiplot(est_did, dict = dict)
#'
#' #
#' # Example 3: Setting defaults
#' #
#'
#' # The customization logic of ggcoefplot/ggiplot works differently than the
#' # original base fixest counterparts, so we don't have "gg" equivalents of
#' # setFixest_coefplot and setFixest_iplot. However, you can still invoke some
#' # global fixest settings like setFixest_dict(). SImple example:
#'
#' base_inter$letter = letters[base_inter$period]
#' est_letters = feols(y ~ x1 + i(letter, treat, 'e') | id+letter, base_inter)
#'
#' # Set global dictionary for capitalising the letters
#' dict = LETTERS[1:10]; names(dict) = letters[1:10]
#' setFixest_dict(dict)
#'
#' ggiplot(est_letters)
#'
#' setFixest_dict() # reset
#'
#' #
#' # Example 4: group + cleaning
#' #
#'
#' # You can use the argument group to group variables
#' # You can further use the special character "^^" to clean
#' #  the beginning of the coef. name: particularly useful for factors
#'
#' est = feols(Petal.Length ~ Petal.Width + Sepal.Length +
#' 							Sepal.Width + Species, iris)
#'
#' # No grouping:
#' ggcoefplot(est)
#'
#' # now we group by Sepal and Species
#' ggcoefplot(est, group = list(Sepal = "Sepal", Species = "Species"))
#'
#' # now we group + clean the beginning of the names using the special character ^^
#' ggcoefplot(est, group = list(Sepal = "^^Sepal.", Species = "^^Species"))
#'
#'
#' #
#' # Example 5: Some more ggcoefplot/ggiplot extras
#' #
#'
#' # We'll demonstrate using the staggered treatment example from the
#' # introductory fixest vignette.
#'
#' data(base_stagg)
#' est_twfe = feols(
#'   y ~ x1 + i(time_to_treatment, treated, ref = c(-1, -1000)) | id + year,
#'   base_stagg
#' )
#' est_sa20 = feols(
#'   y ~ x1 + sunab(year_treated, year) | id + year,
#'   data = base_stagg
#' )
#'
#' # Plot both regressions in a faceted plot
#' ggiplot(
#'   list('TWFE' = est_twfe, 'Sun & Abraham (2020)' = est_sa20),
#'   main = 'Staggered treatment', ref.line = -1, pt.join = TRUE
#' )
#'
#' # So far that's no different than base iplot (automatic legend  aside). But an
#' # area where ggiplot shines is in complex multiple estimation cases, such as
#' # lists of fixest_multi objects. To illustrate, let's add a split variable
#' # (group) to our staggered dataset.
#' base_stagg_grp = base_stagg
#' base_stagg_grp$grp = ifelse(base_stagg_grp$id %% 2 == 0, 'Evens', 'Odds')
#'
#' # Now re-run our two regressions from earlier, but splitting the sample to
#' # generate fixest_multi objects.
#' est_twfe_grp = feols(
#'   y ~ x1 + i(time_to_treatment, treated, ref = c(-1, -1000)) | id + year,
#'   data = base_stagg_grp, split = ~ grp
#' )
#' est_sa20_grp = feols(
#'   y ~ x1 + sunab(year_treated, year) | id + year,
#'   data = base_stagg_grp, split = ~ grp
#' )
#'
#' # ggiplot combines the list of multi-estimation objects without a problem...
#' ggiplot(list('TWFE' = est_twfe_grp, 'Sun & Abraham (2020)' = est_sa20_grp),
#' 				ref.line = -1, main = 'Staggered treatment: Split multi-sample')
#'
#' # ... but is even better when we use facets instead of dodged errorbars.
#' # Let's use this an opportunity to construct a fancy plot that invokes some
#' # additional arguments and ggplot theming.
#' ggiplot(
#'   list('TWFE' = est_twfe_grp, 'Sun & Abraham (2020)' = est_sa20_grp),
#'   ref.line = -1,
#'   main = 'Staggered treatment: Split multi-sample',
#'   xlab = 'Time to treatment',
#'   multi_style = 'facet',
#'   geom_style = 'ribbon',
#'   facet_args = list(labeller = labeller(id = \(x) gsub(".*: ", "", x))),
#'   theme = theme_minimal() +
#'     theme(
#'       text = element_text(family = 'HersheySans'),
#'       plot.title = element_text(hjust = 0.5),
#'       legend.position = 'none'
#'     )
#' )
#'
#' #
#' # Aside on theming and scale adjustments
#' #
#'
#' # Setting the theme inside the `ggiplot()` call is optional and not strictly
#' # necessary, since the ggplot2 API allows programmatic updating of existing
#' # plots. E.g.
#' last_plot() +
#' 	labs(caption = 'Note: Super fancy plot brought to you by ggiplot')
#' last_plot() +
#' 	theme_grey() +
#' 	theme(legend.position = 'none') +
#' 	scale_fill_brewer(palette = 'Set1', aesthetics = c("colour", "fill"))
#' # etc.
#'
ggiplot = function(
	object,
	geom_style = c('pointrange', 'errorbar', 'ribbon'),
	multi_style = c('dodge', 'facet'),
	aggr_eff = c('none', 'post', 'pre', 'both'),
	aggr_eff.par = list(col = 'grey50', lwd = 1, lty = 1),
	facet_args = NULL,
	theme = NULL,
	...
	) {

  geom_style = match.arg(geom_style)
  multi_style = match.arg(multi_style)
  aggr_eff = match.arg(aggr_eff)
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
  pt.join      = if (!is.null(dots[['pt.join']])) dots[['pt.join']] else FALSE
  ## hold off deciding zero line until we have seen the data
  # zero         = if (!is.null(dots[['zero']])) dots[['zero']] else TRUE
  # zero.par = list(col = 'black', lty = 1, lwd = 0.3)i
  # if (!is.null(dots[['zero.par']])) zero.par = utils::modifyList(zero.par, dots[['zero.par']])
  ref.line     = if (!is.null(dots[['ref.line']])) dots$ref.line else 'auto'
  if (isFALSE(ref.line)) ref.line = NULL
  ref.line.par = list(col = "black", lty = 2, lwd = 0.3)
  if (!is.null(dots[["ref.line.par"]])) ref.line.par = utils::modifyList(ref.line.par, dots[["ref.line.par"]])


  # The next few blocks grab the underlying iplot/coefplot data, contingent on the
  # object that was passed into the function (i.e. fixest, fixest_multi, or
  # list)
  iplot_data_func = ifelse(isTRUE(is_iplot), iplot_data, coefplot_data)

  if (inherits(object, c("fixest", "fixest_multi"))) {

      if (length(ci_level) == 1) {
          data = iplot_data_func(object, .ci_level = ci_level, .dict = dict, .aggr_es = aggr_eff, .keep = keep, .drop = drop, .group = group, .i.select = i.select)
      } else {
          data = lapply(
              ci_level,
              function(ci_l) iplot_data_func(object, .ci_level = ci_l, .dict = dict, .aggr_es = aggr_eff, .keep = keep, .drop = drop, .group = group, .i.select = i.select)
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
              .ci_level = ci_level, .dict = dict, .aggr_es = aggr_eff, .group = group, .i.select = i.select
          )
      } else {
          data = lapply(ci_level, function(ci_l) {
              lapply(object, iplot_data_func,
                  .ci_level = ci_l,
                  .dict = dict, .aggr_es = aggr_eff, .group = group, .i.select = i.select
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
  	data[["x"]] = interaction(
  		data[["x"]], data[["group_var"]],
  		sep = "___", drop = TRUE#, lex.order = TRUE
  		)
  	data[["group_var"]] = NULL
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

  ptsize = 2.5
  if (multi_style == "facet") {
      ptsize = ptsize - 0.25 * n_fcts
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
          if (aggr_eff != "none") {
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
                       aes(group = paste0(.data$group, .data$id)),
                       position = position_dodge(width = ci.width)
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
                           shape = pt.pch, size = ptsize,
                           position = position_dodge2(width = ci.width, padding = ci.width)
                       )
                   } else {
                       geom_point(
                           data = ~ subset(.x, ci_level == max(ci_level)),
                           shape = pt.pch, size = ptsize,
                           position = position_dodge2(width = ci.width, padding = ci.width)
                       )
                   }
               } else {
                   geom_point(shape = pt.pch, size = ptsize)
               }
           } else {
               if (multi_style == "dodge") {
                   if (length(ci_level) == 1) {
                       geom_point(
                           size = ptsize,
                           position = position_dodge2(width = ci.width, padding = ci.width)
                       )
                   } else {
                       geom_point(
                           data = ~ subset(.x, ci_level == max(ci_level)),
                           size = ptsize,
                           position = position_dodge2(width = ci.width, padding = ci.width)
                       )
                   }
               } else {
                   geom_point(size = ptsize)
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
   		scale_x_discrete(guide = ggh4x::guide_axis_nested(delim = "___"))
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

  if (has_groups) {
  	gg = gg +
  		theme(ggh4x.axis.nestline = element_line(linewidth = 0.5))
  }

  return(gg)

}




#' @describeIn ggiplot This function plots the results of estimations
#'   (coefficients and confidence intervals). The function `ggiplot` restricts
#'   the output to variables created with i, either interactions with factors or
#'   raw factors.
#' @export
ggcoefplot = function(
		object,
		geom_style = c('pointrange', 'errorbar'),
		multi_style = c('dodge', 'facet'),
		facet_args = NULL,
		theme = NULL,
		...
		) {


  geom_style = match.arg(geom_style)
  multi_style = match.arg(multi_style)

	ggiplot(
		object = object,
		geom_style = geom_style,
		multi_style = multi_style,
		facet_args = facet_args,
		theme = theme,
		is_iplot = FALSE,
		...
		)

	}
