oxford = function(some_vec) {
	if (length(some_vec)>2) {
		paste0("[", paste(utils::head(some_vec, -1), collapse = ", "), ", & ", utils::tail(some_vec, 1), "]")
	} else if (length(some_vec)==2) {
		paste0("[", some_vec[1], " & ", some_vec[2], "]")
	} else {
		paste(some_vec)
	}
}
