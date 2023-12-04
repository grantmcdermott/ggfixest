#' @keywords internal
oxford = function(some_vec) {
	if (length(some_vec)>2) {
		paste0("[", paste(utils::head(some_vec, -1), collapse = ", "), ", & ", utils::tail(some_vec, 1), "]")
	} else if (length(some_vec)==2) {
		paste0("[", some_vec[1], " & ", some_vec[2], "]")
	} else {
		paste(some_vec)
	}
}

##
## Unexported functions borrowed from fixest

#' @keywords internal
escape_regex = function(x){
	# escape special characters in regular expressions => to make it as "fixed"

	res = gsub("((?<=[^\\\\])|(?<=^))(\\$|\\.|\\+|\\(|\\)|\\[|\\]|\\?|\\^)", "\\\\\\2", x, perl = TRUE)
	res
}


#' @keywords internal
dict_apply = function(x, dict = NULL){

	dreamerr::check_arg(dict, "NULL named character vector no na", .message = "The argument `dict` must be a dictionnary, ie a named vector (eg dict=c(\"old_name\"=\"new_name\")")

	if(missing(dict) || length(dict) == 0){
		return(x)
	}

	# We make the dictionary names space agnostic, adds a handful of us only
	if(any(grepl(" ", x, fixed = TRUE))){
		x_tmp = gsub(" ", "", x, fixed = TRUE)
	} else {
		x_tmp = x
	}

	if(any(grepl(" ", names(dict), fixed = TRUE))){
		names(dict) = gsub(" ", "", names(dict), fixed = TRUE)
		if(anyDuplicated(names(dict))){
			dup = duplicated(names(dict))
			stop("The dictionary `dict` contains several items with the same names, it concerns ",
					 dreamerr::enumerate_items(names(dict)[dup]), " (note that spaces are ignored).")
		}
	}

	who = x_tmp %in% names(dict)
	x[who] = dict[as.character(x_tmp[who])]
	x
}


#' @keywords internal
replace_and_make_callable = function(text, varlist, text_as_expr = FALSE){
	# used to make a character string callable and substitutes variables inside text with the variables
	# in varlist
	# ex: "Interacted with __var__" becomes "Interacted with x_beta"
	# or: "&paste(\"Interacted with \", x[beta])"

	if(length(text) > 1) stop("Internal problem: length of text should not be greater than 1.")

	text_split = strsplit(paste0(text, " "), "__")[[1]]

	if(length(text_split) < 3){
		# Nothing to be done!
		return(text)
	} else {
		# We need to replace the variables
		is_var = seq_along(text_split) %% 2 == 0
		my_variables = text_split[is_var]

		if(length(varlist) == 0 || any(!my_variables %in% names(varlist))){
			info = "No special variable is available for this estimation."
			if(length(varlist) > 0){
				info = paste0("In this estimation, the only special variable", dreamerr::enumerate_items(paste0("__", names(varlist), "__"), "s.is.start"), ". ")
			}

			# warning(info, enumerate_items(paste0("__", setdiff(my_variables, names(varlist)), "__"), "is"), " not valid, thus ignored.", call. = FALSE)

			return("")

			not_var = !my_variables %in% names(varlist)
			is_var[is_var][not_var] = FALSE
			my_variables = intersect(my_variables, names(varlist))
			if(length(my_variables) == 0){
				return(text)
			}
		}

		my_variables_values = varlist[my_variables]

		if(any(lengths(varlist[my_variables]) > 1)){
			qui = which(lengths(varlist[my_variables]) > 1)[1]
			n_val = lengths(varlist[my_variables])[qui]
			warning("The special variable __", my_variables[qui], "__ takes ", n_val, " values, only the first is used.", call. = FALSE)
			my_variables_values = sapply(my_variables_values, function(x) x[1])
		} else {
			my_variables_values = unlist(my_variables_values)
		}

		# we prepare the result (we drop the last space)
		n = length(text_split)
		text_new = text_split
		text_new[n] = gsub(" $", "", text_new[n])
		if(nchar(text_new[n]) == 0){
			text_new = text_new[-n]
			is_var = is_var[-n]
		}


		# Do the variables contain expressions?
		is_expr = grepl("^&", my_variables_values)
		if(any(is_expr)){

			expr_drop = function(x){
				if(grepl("^&", x)){
					res = gsub("^&", "", x)
					if(grepl("^expression\\(", res)){
						res = gsub("(^expression\\()|(\\)$)", "", res)
					} else if(grepl("^substitute\\(", res)){
						res = deparse(eval(str2lang(res)))
					}
				} else {
					res = x
				}
				res
			}

			my_vars = sapply(my_variables_values, expr_drop)

			if(text_as_expr){
				text_new[is_var][is_expr] = my_vars[is_expr]

				if(all(is_expr)){
					text_new = paste0("&expression(", paste(text_new, collapse = " "), ")")
				} else {
					my_var_no_expr = paste0('"', my_vars, '"')[!is_expr]
					new_names = paste0("x___", seq_along(my_var_no_expr))
					text_new[is_var][!is_expr] = new_names
					text_new = paste0("&substitute(", paste(text_new, collapse = " "), ", list(", paste0(new_names, "=", my_var_no_expr, collapse = ", "), "))")
				}
			} else {
				text_new = paste0('"', text_new, '"')
				text_new[is_var][is_expr] = my_vars[is_expr]

				if(all(is_expr)){
					text_new = paste0("&expression(paste(", paste(text_new, collapse = ", "), "))")
				} else {
					my_var_no_expr = paste0('"', my_vars, '"')[!is_expr]
					new_names = paste0("x___", seq_along(my_var_no_expr))
					text_new[is_var][!is_expr] = new_names
					text_new = paste0("&substitute(paste(", paste(text_new, collapse = ", "), "), list(", paste0(new_names, "=", my_var_no_expr, collapse = ", "), "))")
				}

			}

			return(text_new)
		} else {
			# They don't contain expressions => fine, we just replace with the variables
			if(text_as_expr){
				my_vars = paste0('"', my_variables_values, '"')
				new_names = paste0("x___", seq_along(my_vars))
				text_new[is_var] = new_names
				text_new = paste0("&substitute(", paste(text_new, collapse = ""), ", list(", paste0(new_names, "=", my_vars, collapse = ", "), "))")
			} else {
				text_new[is_var] = my_variables_values
				text_new = paste(text_new, collapse = "")
			}

			return(text_new)
		}

	}
}
