library(ggiplot)
library(tinytest)

#
# Datasets and models ----

data("base_did", package = "fixest")

est = fixest::feols(
	fml = y ~ x1 + i(period, treat, 5) | id + period,
	data = base_did
)

est_log = fixest::feols(
	fml = log(y) ~ x1 + i(period, treat, 5) | id + period,
	data = base_did,
	subset = ~ y >= 0
)


#
# Known output ----

# est |> iplot_data() |> dput()
iplot_data_est_known = structure(
	list(
		estimate = c(-1.40304548628387, -1.24751078444385,  -0.273205962244887, -1.79572056399201, 0, 0.784452028314083,  3.59889733159794, 3.81176621201594, 4.73142620090947, 6.60622884191604 ),
		ci_low = c(-3.57912808041509, -3.39003475863827, -2.44275880868113,  -3.92810996023553, 0, -1.23115156062664, 1.43987271260987, 1.36670728845449,  2.581124921206, 4.41010037012033),
		ci_high = c(0.773037107847341,  0.895013189750577, 1.89634688419136, 0.336668832251501, 0, 2.80005561725481,  5.75792195058601, 6.2568251355774, 6.88172748061295, 8.80235731371175 ),
		estimate_names = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
		estimate_names_raw = c("period::1:treat",  "period::2:treat", "period::3:treat", "period::4:treat", "period::5:treat",  "period::6:treat", "period::7:treat", "period::8:treat", "period::9:treat",  "period::10:treat"),
		is_ref = c(FALSE, FALSE, FALSE, FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE),
		x = c(1, 2, 3, 4, 5, 6, 7,  8, 9, 10),
		id = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
		y = c(-1.40304548628387,  -1.24751078444385, -0.273205962244887, -1.79572056399201, 0,  0.784452028314083, 3.59889733159794, 3.81176621201594, 4.73142620090947,  6.60622884191604),
		lhs = c("y", "y", "y", "y", "y", "y", "y",  "y", "y", "y"),
		ci_level = c(0.95, 0.95, 0.95, 0.95, 0.95, 0.95,  0.95, 0.95, 0.95, 0.95)
	),
	row.names = c("1", "2", "3", "4", "5",  "6", "7", "8", "9", "10"),
	class = "data.frame"
)


# est_log |> iplot_data() |> dput()
iplot_data_est_log_known = structure(
	list(
		estimate = c(0.141945718569522, -0.417790820616548,  0.429214017066127, -0.224501302353741, 0, 0.521077110055194,  0.929997803535568, 0.605730868274549, 1.00494204591818, 1.17999113089822 ),
		ci_low = c(-0.743573500791204, -1.44015069947186, -0.503987905005802,  -1.13189620988709, 0, -0.314671256964655, 0.175122124662237,  -0.175983200454837, 0.258815344677153, 0.350246804524612),
		ci_high = c(1.02746493793025,  0.604569058238765, 1.36241593913806, 0.682893605179609, 0, 1.35682547707504,  1.6848734824089, 1.38744493700394, 1.7510687471592, 2.00973545727183 ),
		estimate_names = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
		estimate_names_raw = c("period::1:treat",  "period::2:treat", "period::3:treat", "period::4:treat", "period::5:treat",  "period::6:treat", "period::7:treat", "period::8:treat", "period::9:treat",  "period::10:treat"),
		is_ref = c(FALSE, FALSE, FALSE, FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE),
		x = c(1, 2, 3, 4, 5, 6, 7,  8, 9, 10),
		id = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1), y = c(0.141945718569522,  -0.417790820616548, 0.429214017066127, -0.224501302353741, 0,  0.521077110055194, 0.929997803535568, 0.605730868274549, 1.00494204591818,  1.17999113089822),
		lhs = c("log(y)", "log(y)", "log(y)", "log(y)",  "log(y)", "log(y)", "log(y)", "log(y)", "log(y)", "log(y)"),
		ci_level = c(0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95,  0.95, 0.95)
	),
	row.names = c("1", "2", "3", "4", "5", "6",  "7", "8", "9", "10"),
	class = "data.frame"
)

#
# tests ----
tol = 1e-6

for (col in c("estimate", "ci_low", "ci_high", "estimate_names",
							"estimate_names_raw", "is_ref", "x", "id", "y", "lhs", "ci_level")) {
	expect_equivalent(est[[col]], iplot_data_est_known[[col]], tolerance = tol)
	expect_equivalent(est_log[[col]], iplot_data_est_log_known[[col]], tolerance = tol)
}
