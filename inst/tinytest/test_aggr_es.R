library(ggfixest)
library(tinytest)

#
# Datasets and models ----

data("base_did", package = "fixest")

est = fixest::feols(
	fml = y ~ x1 + i(period, treat, 5) | id + period,
	data = base_did
)

aggr_post = aggr_es(est)                             # default post
aggr_cum  = aggr_es(est, aggregation = "cumulative") # cumulative instead of mean effects
aggr_pre  = aggr_es(est, period = "pre")             # pre period instead of post
aggr_both = aggr_es(est, period = "both")            # pre & post periods separately
aggr_rhs1 = aggr_es(est, period = "pre", rhs = -1)   # pre period with H0 value of 1

#
# Known output ----

aggr_post_known = data.frame(
	term = "post-treatment (mean)",
	estimate = 3.906554122950695,
	std.error = 0.8598575665281263,
	statistic = 4.543257249830702,
	p.value = 5.5391585244779775e-06,
	s.value = 17.461901741925015,
	conf.low = 2.2212642607213144,
	conf.high = 5.591843985180075
)


aggr_cum_known = data.frame(
	term = "post-treatment (cumulative)",
	estimate = 19.532770614753474,
	std.error = 4.299287821377354,
	statistic = 4.543257261733131,
	p.value = 5.539158211582727e-06,
	s.value = 17.461901823419783,
	conf.low = 11.106321325682188,
	conf.high = 27.95921990382476
)

aggr_pre_known = data.frame(
	term = "pre-treatment (mean)",
	estimate = -1.1798706992411545,
	std.error = 0.8561963882056884,
	statistic = -1.3780374637106132,
	p.value = 0.16819172163573184,
	s.value = 2.5718213967199377,
	conf.low = -2.857984783817578,
	conf.high = 0.49824338533526924
)

aggr_both_known =
	data.frame(
		term = c("pre-treatment (mean)", "post-treatment (mean)"),
		estimate = c(-1.1798706992411545, 3.906554122950695),
		std.error = c(0.8561963882056884, 0.8598575665281263),
		statistic = c(-1.3780374637106132, 4.543257249830702),
		p.value = c(0.16819172163573184, 5.5391585244779775e-06),
		s.value = c(2.5718213967199377, 17.461901741925015),
		conf.low = c(-2.857984783817578, 2.2212642607213144),
		conf.high = c(0.49824338533526924, 5.591843985180075)
	)

aggr_rhs1_known = data.frame(
	term = "pre-treatment (mean)",
	estimate = -0.1798706992411545,
	std.error = 0.8561963882056884,
	statistic = -0.21008112358206216,
	p.value = 0.8336043579365446,
	s.value = 0.26256527509602834,
	conf.low = -1.8579847838175783,
	conf.high = 1.4982433853352692
)


#
# tests ----
tol = 1e-4

for (col in c("term", "estimate", "std.error", "statistic", "p.value", "s.value",
							"conf.low", "conf.high")) {
	expect_equivalent(aggr_post[[col]], aggr_post_known[[col]], tolerance = tol)
	expect_equivalent(aggr_cum[[col]], aggr_cum_known[[col]], tolerance = tol)
	expect_equivalent(aggr_pre[[col]], aggr_pre_known[[col]], tolerance = tol)
	expect_equivalent(aggr_both[[col]], aggr_both_known[[col]], tolerance = tol)
	expect_equivalent(aggr_rhs1[[col]], aggr_rhs1_known[[col]], tolerance = tol)
}
