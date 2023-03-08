library(ggiplot)
library("tinytest")
using("tinysnapshot")

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

p1 = ggiplot(est)
p2 = ggiplot(est, geom_style = "errorbar")
p3 = ggiplot(est, geom_style = "ribbon", pt.pch = NA)
p4 = ggiplot(est, ci_level = c(.8,.95))
p5 = ggiplot(est, ci_level = c(.8,.95), geom_style = 'ribbon', pt.pch = NA)
p6 = ggiplot(list(est))
expect_snapshot_plot(p1, label = "ggiplot_simple")
expect_snapshot_plot(p2, label = "ggiplot_simple_errorbar")
expect_snapshot_plot(p3, label = "ggiplot_simple_ribbon")
expect_snapshot_plot(p4, label = "ggiplot_simple_mci")
expect_snapshot_plot(p5, label = "ggiplot_simple_mci_ribbon")
expect_snapshot_plot(p6, label = "ggiplot_list")

#
## Staggered treatment DiD (common use-case) ----

p7 = ggiplot(
    est_twfe,
    main = 'Staggered treatment', ref.line = -1, pt.join = TRUE,
    ci_level = c(.8,.95)
    )
p8 = ggiplot(
    list('TWFE' = est_twfe),
    main = 'Staggered treatment', ref.line = -1, pt.join = TRUE,
    geom_style = "ribbon", pt.pch = NA,
    ci_level = c(.8,.95)
    )
expect_snapshot_plot(p7, label = "ggiplot_stagg_mci")
expect_snapshot_plot(p8, label = "ggiplot_stagg_mci_ribbon")

#
# Multi plots (single panel) ----

est_sa20 = fixest::feols(
    y ~ x1 + sunab(year_treated, year) | id + year,
    base_stagg
    )

p9 = ggiplot(
    list('TWFE' = est_twfe, 'Sun & Abraham (2020)' = est_sa20)
    )
p10 = ggiplot(
    list(est_twfe, est_sa20)
    )
p11 = ggiplot(
    list('TWFE' = est_twfe, 'Sun & Abraham (2020)' = est_sa20),
    geom_style = "ribbon"
    )
p12 = ggiplot(
    list('TWFE' = est_twfe, 'Sun & Abraham (2020)' = est_sa20),
    main = 'Staggered treatment', ref.line = -1, pt.join = TRUE,
    ci_level = c(.8, .95)
    )
p13 = ggiplot(
    list('TWFE' = est_twfe, 'Sun & Abraham (2020)' = est_sa20),
    main = 'Staggered treatment', ref.line = -1, pt.join = TRUE,
    ci_level = c(.8, .95), geom_style = 'ribbon'
    )
expect_snapshot_plot(p9,  label = "ggiplot_multi_single")
expect_snapshot_plot(p10, label = "ggiplot_multi_single_unnamed")
expect_snapshot_plot(p11, label = "ggiplot_multi_single_ribbon")
expect_snapshot_plot(p12, label = "ggiplot_multi_single_kitchen")
expect_snapshot_plot(p13, label = "ggiplot_multi_single_kitchen_ribbon")

#
# Multi plots (facetted) ----

p14 = ggiplot(
    list('TWFE' = est_twfe, 'Sun & Abraham (2020)' = est_sa20),
    main = 'Staggered treatment', ref.line = -1, pt.join = TRUE,
    ci_level = c(.8, .95), multi_style = 'facet'
    )
p15 = ggiplot(
    list('TWFE' = est_twfe, 'Sun & Abraham (2020)' = est_sa20),
    main = 'Staggered treatment', ref.line = -1, pt.join = TRUE,
    ci_level = c(.8, .95), multi_style = 'facet',
    geom_style = 'ribbon'
    )
expect_snapshot_plot(p14, label = "ggiplot_multi_facet")
expect_snapshot_plot(p15, label = "ggiplot_multi_facet_ribbon")

#
# Multi plots and split samples (complex) ----

p16 = ggiplot(
    list('TWFE' = est_twfe_grp, 'Sun & Abraham (2020)' = est_sa20_grp),
    main = 'Staggered treatment: Split mutli-sample',
    ref.line = -1,
    pt.join = TRUE)
p17 = ggiplot(
    list('TWFE' = est_twfe_grp, 'Sun & Abraham (2020)' = est_sa20_grp),
    main = 'Staggered treatment: Split mutli-sample',
    ref.line = -1, pt.join = TRUE,
    ci_level = c(.8, .95)
    )
p18 = ggiplot(
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
expect_snapshot_plot(p16, label = "ggiplot_multi_complex")
expect_snapshot_plot(p17, label = "ggiplot_multi_complex_mci")
expect_snapshot_plot(p18, label = "ggiplot_multi_complex_kitchen")
