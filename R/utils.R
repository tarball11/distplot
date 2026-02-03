# Returns the rows in the tbl that have values of x closest to the given vals
get_closest_x <- function(tbl, val) {
	l <- c()
	for(i in 1:length(val)) {
		l <- append(l, which.min(abs(tbl$x - val[i])))
	}

	dplyr::slice(tbl, l)
}
