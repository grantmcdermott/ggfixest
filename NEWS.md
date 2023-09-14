# ggiplot 0.0.1.9010 (development version)

## New features

- Support `keep` and `drop` arguments for subsetting coefficients (#22).

## Bug fixes and breaking changes

- Fix naming mismatch in multiple estimation with different time periods (#10).
Thanks @brockmwilson.
- Slight tweak to default theme, which now uses dotted grid lines to more
closely match `iplot()` (#e5cf0b0).
- Correctly parse formula-transformed dependent variable names, e.g. `log(y`
(#20).
- The confidence intervals for some figures may be slightly wider due to
upstream changes in fixest (#25; see also
https://github.com/lrberge/fixest/pull/408).

## Internals

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
