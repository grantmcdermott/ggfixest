
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggfixest

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/ggfixest)](https://CRAN.R-project.org/package=ggfixest)
[![R-universe status
badge](https://grantmcdermott.r-universe.dev/badges/ggfixest)](https://grantmcdermott.r-universe.dev)
[![R-CMD-check](https://github.com/grantmcdermott/ggfixest/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/grantmcdermott/ggfixest/actions/workflows/R-CMD-check.yaml)
[![Docs](https://img.shields.io/badge/docs-homepage-blue.svg)](https://grantmcdermott.com/ggfixest/index.html)
<!-- badges: end -->

The **ggfixest** package provides dedicated **ggplot2** plotting methods
for **fixest** objects. Specifically, it provides drop-in “gg”
equivalents of the latter’s
[`coefplot`](https://lrberge.github.io/fixest/reference/coefplot.html)
and [`iplot`](https://lrberge.github.io/fixest/reference/coefplot.html)
base plotting functions.

The goal of **ggfixest** is to produce nice looking coefficient plots
and interaction plots—including [event
study](https://theeffectbook.net/ch-EventStudies.html) plots—with
minimal effort and scope for further customization.

## Installation

The stable version of **ggfixest** is available on CRAN.

``` r
install.packages("ggfixest")
```

Or, you can grab the latest development version from R-universe.

``` r
install.packages("ggfixest", repos = "https://grantmcdermott.r-universe.dev")
```

## Quickstart

The [package website](https://grantmcdermott.com/ggfixest/) provides a
number of examples in the help documentation. (Also available by typing
`?ggcoefplot` or `?ggiplot` in your R console.) But here are a few
quickstart examples to whet your appetite.

Start by loading the **ggfixest** packages.

``` r
library(ggfixest)
#> Loading required package: ggplot2
#> Loading required package: fixest
```

Note this automatically loaded **ggplot2** and **fixest** too as
required dependencies. As the package name suggests, **ggfixest** *only*
supports **fixest** model objects.[^1]

### Coefficient plots

Use `ggcoefplot` to draw basic coefficient plots.

``` r
est = feols(
  Petal.Length ~ Petal.Width + Sepal.Length + Sepal.Width + Species, 
  data = iris
)

# coefplot(est) ## base version
ggcoefplot(est) ## this package
```

<img src="man/figures/README-coefplot1-1.png" width="100%" />

The above plot call and output should look very familiar to regular
**fixest** users. Like its base equivalent, `ggcoefplot` can be heavily
customized and contains various shortcuts for common operations. For
example, we can use regex to control the coefficient grouping logic.

``` r
ggcoefplot(est, group = list(Sepal = "^^Sepal.", Species = "^^Species"))
```

<img src="man/figures/README-coefplot2-1.png" width="100%" />

### Event study plots

The `ggiplot` function is a special case of `ggocoefplot` that only
plots coefficients with factor levels or interactions (specifically,
those created with the
[`i`](https://lrberge.github.io/fixest/reference/i.html) operator). This
is especially useful for producing event study plots in a
difference-in-differences (DiD) setup.

``` r
est_did = feols(y ~ x1 + i(period, treat, 5) | id+period, base_did)

# iplot(est_did) ## base version
ggiplot(est_did) ## this package
```

<img src="man/figures/README-es1-1.png" width="100%" />

Again, the above plot call and output should look very familiar to
regular **fixest** users. But note that `ggiplot` supports several
features that are not available in the base `iplot` version. For
example, plotting multiple confidence intervals and aggregate treatments
effects.

``` r
ggiplot(
    est_did,
    ci_level = c(.8, .95),
    aggr_eff = "post", aggr_eff.par = list(col = "orange")
)
```

<img src="man/figures/README-es2-1.png" width="100%" />

And you can get quite fancy, combining lists of complex multiple
estimation objects with custom themes, and so on.

``` r
base_stagg_grp = base_stagg
base_stagg_grp$grp = ifelse(base_stagg_grp$id %% 2 == 0, 'Evens', 'Odds')

est_twfe_grp = feols(
    y ~ x1 + i(time_to_treatment, treated, ref = c(-1, -1000)) | id + year,
    data = base_stagg_grp, split = ~grp
)

est_sa20_grp = feols(
    y ~ x1 + sunab(year_treated, year) | id + year,
    data = base_stagg_grp, split = ~grp
)

ggiplot(
    list("TWFE" = est_twfe_grp, "Sun & Abraham (2020)" = est_sa20_grp),
    ref.line = -1,
    main = "Staggered treatment: Split mutli-sample",
    xlab = "Time to treatment",
    multi_style = "facet",
    geom_style = "ribbon",
    facet_args = list(labeller = labeller(id = \(x) gsub(".*: ", "", x))),
    theme = theme_minimal() +
        theme(
            text = element_text(family = "HersheySans"),
            plot.title = element_text(hjust = 0.5),
            legend.position = "none"
        )
)
```

<img src="man/figures/README-es3-1.png" width="100%" />

For more `ggiplot` examples and comparisons with its base counterpart,
see the detailed
[vignette](http://grantmcdermott.com/ggfixest/articles/ggiplot.html) on
the package homepage (or, by typing `vignette("ggiplot")` in your R
console).

[^1]: For other model classes, a more generic visualization package/tool
    like
    [**see**](https://easystats.github.io/see/articles/parameters.html)
    or
    [**modelsummary**](https://modelsummary.com/vignettes/modelplot.html)
    would be more appropriate.
