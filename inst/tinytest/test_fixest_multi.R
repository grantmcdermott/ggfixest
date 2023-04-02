library(fixest)
library(ggiplot)

library(tinytest)
using("tinysnapshot")
options(tinysnapshot_os = "Darwin")
options(tinysnapshot_device = "svglite")

multi_lhs = feols(c(mpg, wt) ~ i(vs, drat), mtcars)

p1a = ggiplot(multi_lhs, ref.line = 0.5)
p1b = ggiplot(multi_lhs, multi_style = "facet", ref.line = 0.5)
p1c = ggiplot(multi_lhs, multi_style = "facet", geom_style = "ribbon", ref.line = 0.5)
expect_snapshot_plot(p1a, label = "ggiplot_multi_lhs")
expect_snapshot_plot(p1b, label = "ggiplot_multi_lhs_facet")
expect_snapshot_plot(p1c, label = "ggiplot_multi_lhs_facet_ribbon")

multi_csw = feols(mpg ~  csw(disp, qsec) + i(vs, drat), mtcars)

p2a = ggiplot(multi_csw, ref.line = 0.5)
p2b = ggiplot(multi_csw, multi_style = "facet", ref.line = 0.5)
expect_snapshot_plot(p2a, label = "ggiplot_multi_csw")
expect_snapshot_plot(p2b, label = "ggiplot_multi_csw_facet")

multi_lhs_csw = feols(c(mpg, wt) ~  csw(disp, qsec) + i(vs, drat), mtcars)

p3a = ggiplot(multi_lhs_csw, ref.line = 0.5)
p3b = ggiplot(multi_lhs_csw, multi_style = "facet", ref.line = 0.5)
expect_snapshot_plot(p3a, label = "ggiplot_multi_lhs_csw")
expect_snapshot_plot(p3b, label = "ggiplot_multi_lhs_csw_facet")
