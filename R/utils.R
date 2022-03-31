oxford = function(x) {
	if (length(x)>2) {
		paste0("[", paste(head(x, -1), collapse = ", "), ", & ", tail(x, 1), "]")
	} else if (length(x)==2) {
		paste0("[", x[1], " & ", x[2], "]")
	} else {
		paste(x)
	}
}
