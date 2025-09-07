library(ggfixest)
library(tinytest)

#
# Datasets and models ----

data("base_did", package = "fixest")

est = fixest::feols(
	fml = y ~ x1 + i(period, treat, 5) | id + period,
	data = base_did,
	vcov = "cluster"
)

est_log = fixest::feols(
	fml = log(y) ~ x1 + i(period, treat, 5) | id + period,
	data = base_did,
	subset = ~ y >= 0,
	vcov = "cluster"
)


iplot_data_est = iplot_data(est)
iplot_data_est_log = iplot_data(est_log)

#
# Known output ----

# est |> iplot_data() |> dput()
iplot_data_est_known = structure(
	list(
		estimate = c(-1.40304548628387, -1.24751078444385,  -0.273205962244887, -1.79572056399201, 0, 0.784452028314083,  3.59889733159794, 3.81176621201594, 4.73142620090947, 6.60622884191604 ),
		ci_low = c(-3.60401957599237, -3.41454238816103, -2.46757561261468,  -3.95250166364274, 0, -1.25420738946099, 1.41517633761945, 1.33873906080257, 2.55652832963455, 4.38497957631853),
		ci_high = c(0.79792860342463,  0.919520819273338, 1.92116368812491, 0.361060535658718, 0, 2.82311144608915,  5.78261832557643, 6.28479336322932, 6.9063240721844, 8.82747810751355),
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
		estimate = c(0.141945718569522, -0.417790820616548,  0.429214017066127, -0.224501302353741, 0, 0.521077110055194,  0.929997803535568, 0.605730868274549, 1.00494204591818, 1.17999113089822),
		ci_low = c(-0.753702664910143, -1.45184513904486, -0.51466249593292,  -1.14227560283322, 0, -0.324231108616199, 0.166487349184158,  -0.184924971478124, 0.250280645886709, 0.34075563112618),
		ci_high = c(1.03759410204919,  0.616263497811766, 1.37309053006517, 0.693272998125739, 0, 1.36638532872659,  1.69350825788698, 1.39638670802722, 1.75960344594965, 2.01922663067026),
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
	expect_equivalent(iplot_data_est[[col]], iplot_data_est_known[[col]], tolerance = tol)
	expect_equivalent(iplot_data_est_log[[col]], iplot_data_est_log_known[[col]], tolerance = tol)
}
