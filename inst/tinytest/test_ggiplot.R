library("tinytest")
using("tinyviztest")

#
# Datasets and models ----

data("base_did", package = "fixest")
data("base_stagg", package = "fixest")
base_stagg_grp = base_stagg
base_stagg_grp$grp = ifelse(base_stagg_grp$id %% 2 == 0, 'Evens', 'Odds')

est = fixest::feols(
    fml = y ~ x1 + i(period, treat, 5) | id + period,
    data = base_did
    )

est_twfe = fixest::feols(
    y ~ x1 + i(time_to_treatment, treated, ref = c(-1, -1000)) | id + year,
    base_stagg
    )

est_twfe_grp = fixest::feols(
    y ~ x1 + i(time_to_treatment, treated, ref = c(-1, -1000)) | id + year,
    base_stagg_grp,
    split = ~ grp
    )

est_sa20_grp = fixest::feols(
    y ~ x1 + sunab(year_treated, year) | id + year,
    base_stagg_grp,
    split = ~ grp
    )

# NB: 1st test runs will fail, but write the targets to file. 2nd run(s) should
# pass.

#
## Simple ggiplot ----

p1 = function() ggiplot(est)
p2 = function() ggiplot(est, geom_style = "errorbar")
p3 = function() ggiplot(est, geom_style = "ribbon", pt.pch = NA)
p4 = function() ggiplot(est, ci_level = c(.8,.95))
p5 = function() ggiplot(est, ci_level = c(.8,.95), geom_style = 'ribbon', pt.pch = NA)
p6 = function() ggiplot(list(est))
expect_vdiff(p1, "ggiplot_simple")
expect_vdiff(p2, "ggiplot_simple_errorbar")
expect_vdiff(p3, "ggiplot_simple_ribbon")
expect_vdiff(p4, "ggiplot_simple_mci")
expect_vdiff(p5, "ggiplot_simple_mci_ribbon")
expect_vdiff(p6, "ggiplot_list")

#
## Staggered treatment DiD (common use-case) ----

p7 = function() ggiplot(
    est_twfe,
    main = 'Staggered treatment', ref.line = -1, pt.join = TRUE,
    ci_level = c(.8,.95)
    )
p8 = function() ggiplot(
    list('TWFE' = est_twfe),
    main = 'Staggered treatment', ref.line = -1, pt.join = TRUE,
    geom_style = "ribbon", pt.pch = NA,
    ci_level = c(.8,.95)
    )
expect_vdiff(p7, "ggiplot_stagg_mci")
expect_vdiff(p8, "ggiplot_stagg_mci_ribbon")

#
# Multi plots (single panel) ----

est_sa20 = fixest::feols(
    y ~ x1 + sunab(year_treated, year) | id + year,
    base_stagg
    )

p9 = function() ggiplot(
    list('TWFE' = est_twfe, 'Sun & Abraham (2020)' = est_sa20)
    )
p10 = function() ggiplot(
    list(est_twfe, est_sa20)
    )
p11 = function() ggiplot(
    list('TWFE' = est_twfe, 'Sun & Abraham (2020)' = est_sa20),
    geom_style = "ribbon"
    )
p12 = function() ggiplot(
    list('TWFE' = est_twfe, 'Sun & Abraham (2020)' = est_sa20),
    main = 'Staggered treatment', ref.line = -1, pt.join = TRUE,
    ci_level = c(.8, .95)
    )
p13 = function() ggiplot(
    list('TWFE' = est_twfe, 'Sun & Abraham (2020)' = est_sa20),
    main = 'Staggered treatment', ref.line = -1, pt.join = TRUE,
    ci_level = c(.8, .95), geom_style = 'ribbon'
    )
expect_vdiff(p9, "ggiplot_multi_single")
expect_vdiff(p10, "ggiplot_multi_single_unnamed")
expect_vdiff(p11, "ggiplot_multi_single_ribbon")
expect_vdiff(p12, "ggiplot_multi_single_kitchen")
expect_vdiff(p13, "ggiplot_multi_single_kitchen_ribbon")

#
# Multi plots (facetted) ----

p14 = function() ggiplot(
    list('TWFE' = est_twfe, 'Sun & Abraham (2020)' = est_sa20),
    main = 'Staggered treatment', ref.line = -1, pt.join = TRUE,
    ci_level = c(.8, .95), multi_style = 'facet'
    )
p15 = function() ggiplot(
    list('TWFE' = est_twfe, 'Sun & Abraham (2020)' = est_sa20),
    main = 'Staggered treatment', ref.line = -1, pt.join = TRUE,
    ci_level = c(.8, .95), multi_style = 'facet', 
    geom_style = 'ribbon'
    )
expect_vdiff(p14, "ggiplot_multi_facet")
expect_vdiff(p15, "ggiplot_multi_facet_ribbon")

#
# Multi plots and split samples (complex) ----

p16 = function() ggiplot(
    list('TWFE' = est_twfe_grp, 'Sun & Abraham (2020)' = est_sa20_grp),
    main = 'Staggered treatment: Split mutli-sample', 
    ref.line = -1, 
    pt.join = TRUE)
p17 = function() ggiplot(
    list('TWFE' = est_twfe_grp, 'Sun & Abraham (2020)' = est_sa20_grp),
    main = 'Staggered treatment: Split mutli-sample',
    ref.line = -1, pt.join = TRUE,
    ci_level = c(.8, .95)
    )
p18 = function() ggiplot(
    list('TWFE' = est_twfe_grp, 'Sun & Abraham (2020)' = est_sa20_grp),
    main = 'Staggered treatment: Split mutli-sample',
    ref.line = -1,
    xlab = 'Time to treatment',
    multi_style = 'facet',
    geom_style = 'ribbon',
    ci_level = c(.8, .95),
    theme = theme_minimal() + theme(
        text = element_text(family = 'HersheySans'),
        plot.title = element_text(hjust = 0.5),
        legend.position = 'none'
        )
    )
expect_vdiff(p16, "ggiplot_multi_complex")
expect_vdiff(p17, "ggiplot_multi_complex_mci")
expect_vdiff(p18, "ggiplot_multi_complex_kitchen")