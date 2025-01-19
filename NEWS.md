# ggfixest 0.1.0.99 (development version)

### Bug fixes

- Fix missing "hypothesis" attribute for `aggr_es` objects. (#43)
- Fix dodge misalignment between points and lines with multi fixest objects. (#44)

### Internals

- Replace `ggh4x` dependency with `legendry`. (#41 @teunbrand)

### Misc

- Minor website and documentation improvements.

# ggfixest 0.1.0

First CRAN release!

### New features

- The `aggr_es` function now supports numeric sequences for aggregating a
specific subset of periods, in addition to the existing keyword strings like
"pre" or "post". This functionality also passes through to the higher order
plotting functions that call `aggr_es` under the hood. For example,
`ggiplot(est, aggr_eff = 6:8)`. (#33)
- Users can now adjust standard errors for model objects on-the-fly at plot
time, by passing an appropriate argument, e.g. `ggcoefplot(est, vcov = "hc1")`.
These on-the-fly adjustments are done via `summary.fixest`, and so the effect is
just the same as passing an adjusted object directly, e.g.
`ggcoefplot(summary(est, vcov = "hc1"))`. However, it may prove more convenient
for simultaneously adjusting a list of multiple models, e.g. 
`ggcoefplot(list(est1, est2, est3), vcov = "hc1")`. (#35)

# ggfixest 0.0.3

### Breaking change

- The package name has been changed to **ggfixest** (#29). 

# ggiplot 0.0.2

### New features

- Support for `ggcoefplot`, a ggplot equivalent of `coefplot` (#28).
- Support `pt.size` argument for controlling the size of point markers (#27).
Thanks @jcvdav.
- Support `keep` and `drop` arguments for subsetting coefficients (#22).

### Bug fixes and breaking changes

- Fix naming mismatch in multiple estimation with different time periods (#10).
Thanks @brockmwilson.
- Slight tweak to default theme, which now uses dotted grid lines to more
closely match `iplot()` (#e5cf0b0).
- Correctly parse formula-transformed dependent variable names, e.g. `log(y`
(#20).
- The confidence intervals for some figures may be slightly wider due to
upstream changes in fixest (#25; see also
https://github.com/lrberge/fixest/pull/408).

### Internals

- Add a (visual) test suite (#12 with several increments thereafter). Thanks to
@vincentarelbundock for ~~tinyviztest~~
[tinysnapshot](https://github.com/vincentarelbundock/tinysnapshot)!
- Switch to `marginaleffects::hypotheses()` internally for `aggr_es()` to match
the upstream changes in **marginaleffects**.
- Simplify multi_fixest object parsing (#19).
- Minor documentation improvements.

# ggiplot 0.0.1

* Tweaks to plot output, including integer breaks on x-axis (where appropriate)
and allow additional user-level control (e.g. CI alpha or width levels)
* Support multiple confidence levels (#2, #5)
* Support multiple LHS variables (#1)
* Added a `NEWS.md` file to track changes to the package.
