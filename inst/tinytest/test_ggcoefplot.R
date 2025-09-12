source("tinysnapshot_helpers.R")
using("tinysnapshot")
if (Sys.info()["sysname"] != "Linux") exit_file("Linux snapshots")

library(ggfixest)

# NB: 1st test runs will fail, but write the targets to file. 2nd run(s) should
# pass.

#
## Simple ggcoeflot ----

est = feols(Petal.Length ~ Petal.Width + i(Species), iris)
p = ggcoefplot(est)
expect_snapshot_plot(p, label = "ggcoefplot_simple")

#
## i-based Interactions ----

est_i = feols(mpg ~ 0 + i(cyl, wt):disp + i(am, hp), mtcars)
p_i1 = ggcoefplot(est_i)
expect_snapshot_plot(p_i1, label = "ggcoefplot_interactions")
p_i2 = ggcoefplot(est_i, ci_level = c(.8, .95))
expect_snapshot_plot(p_i2, label = "ggcoefplot_interactions_multici")

#
## Multi estimation (with interactions) ----

est_multi = feols(c(mpg, hp) ~ i(cyl, wt), mtcars)
p_multi = ggcoefplot(est_multi)
expect_snapshot_plot(p_multi, label = "ggcoefplot_multi")
p_multi_facet = ggcoefplot(est_multi, multi_style = "facet")
expect_snapshot_plot(p_multi_facet, label = "ggcoefplot_multi_facet")


#
## Grouping ----

est_grp = feols(Petal.Length ~ Petal.Width + Sepal.Length + Sepal.Width + Species, iris)

p_grp1 = ggcoefplot(est_grp) # no groups
expect_snapshot_plot(p_grp1, label = "ggcoefplot_group_none")
p_grp2 = ggcoefplot(est_grp, group = list("Sepal", "Species")) # group, no names
expect_snapshot_plot(p_grp2, label = "ggcoefplot_group_nonames")
p_grp3 = ggcoefplot(est_grp, group = list(Sepal = "Sepal", Species = "Species")) # group + names
expect_snapshot_plot(p_grp3, label = "ggcoefplot_groupnames")
p_grp4 = ggcoefplot(est_grp, group = list(Sepal = "^^Sepal.", Species = "^^Species")) # group + ^^names
expect_snapshot_plot(p_grp4, label = "ggcoefplot_group_names_prefix")

#
## DiD (mostly to check auto grouping) ----

data("base_did", package = "fixest")
est_did = feols(y ~ x1 + i(period, treat, 5) | id + period, base_did)
p_did = ggcoefplot(est_did)
expect_snapshot_plot(p_did, label = "ggcoefplot_did")


#
## vcov adjustment (passed through ...) ----

p_did_iid_summ = ggcoefplot(summary(est_did, vcov = "iid")) # manual approach
p_did_iid      = ggcoefplot(est_did, vcov = "iid") # passed through "..."
expect_snapshot_plot(p_did_iid_summ, label = "ggcoefplot_did_iid")
expect_snapshot_plot(p_did_iid, label = "ggcoefplot_did_iid") # should be identical

