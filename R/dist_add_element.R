dist_add_curve <- function(data = NULL,
													 linetype = "solid",
													 size = 1,
													 color = "black",
													 ...) {

	ggplot2::geom_line(data = data,
										 linetype = linetype,
										 size = size,
										 color = color,
										 ...)
}




StatDistShading <- ggplot2::ggproto(
	"StatDistShading",
	ggplot2::Stat,
	required_aes = c("x", "y"),

	compute_group = function(data,
													 scales,
													 shade_type = c('above', 'below', 'between', 'tails'),
													 limits = NULL) {
		stopifnot(!missing(shade_type))
		shade_type <- match.arg(shade_type)
		stopifnot(length(shade_type) == 1)

		stopifnot(!is.null(limits), is.numeric(limits))
		if(shade_type %in% c('above', 'below')) stopifnot(length(limits) == 1)
		if(shade_type %in% c('between', 'tails')) stopifnot(length(limits) == 2)

		grid <- data

		if(shade_type == 'above') {
			grid$y <- ifelse(grid$x > limits, grid$y, 0)

		} else if(shade_type == 'below') {
			grid$y <- ifelse(grid$x < limits, grid$y, 0)

		} else if(shade_type == 'between') {
			limits = sort(limits)

			grid$y <- ifelse(dplyr::between(grid$x, limits[1], limits[2]), grid$y, 0)

		} else if(shade_type == 'tails') {
			limits = sort(limits)

			grid$y <- ifelse(dplyr::between(grid$x, limits[1], limits[2]), 0, grid$y)
		}

		grid
	}
)

dist_add_shading <- function(mapping = ggplot2::aes(x = x, y = y),
														 data = NULL,
														 shade_type = c('above', 'below', 'between', 'tails'),
														 limits = NULL,
														 fill = 'red',
														 alpha = 0.70,
														 ...) {
		ggplot2::layer(
			stat = StatDistShading,
			data = data,
			mapping = mapping,
			geom = "area",
			position = "identity",
			params = list(shade_type = shade_type,
										limits = limits,
										fill = fill,
										alpha = alpha,
										...))
}


StatDistSegment <- ggplot2::ggproto(
	"StatDistSegment",
	ggplot2::Stat,
	required_aes = c("x", "y"),

	compute_group = function(data,
													 scales,
													 params,
													 seg_x) {
		stopifnot(length(seg_x) >= 1)

		grid <- get_closest_x(data[c("y", "x")], seg_x)
		names(grid) <- c("yend", "x")
		grid$xend <- grid$x
		grid$y <- 0

		grid
	}
)

dist_add_segment <- function(mapping = ggplot2::aes(x = x, y = y),
														 data = NULL,
														 seg_x,
														 linetype = "solid",
														 size = 1,
														 color = "black",
														 ...) {
	ggplot2::layer(
		stat = StatDistSegment,
		data = data,
		mapping = mapping,
		geom = "segment",
		position = "identity",
		params = list(seg_x = seg_x,
									linetype = linetype,
									size = size,
									color = color,
									...)
	)
}


# Workhorse function that draws a curve along with optional shading and segments
dist_add_elements <- function(data = NULL,
															linetype = "solid",
															size = 1,
															color = "black",
															shade_type = NULL,
															shade_limits = NULL,
															shade_fill = "red",
															shade_alpha = 0.7,
															seg_x = NULL,
															seg_linetype = "solid",
															seg_size = 1,
															seg_color = "black",
															...) {

	if(!is.null(data)) stopifnot(is.data.frame(data))
	stopifnot(is.character(linetype))
	stopifnot(is.numeric(size))
	stopifnot(is.character(color))

	l <- list()

	if(!is.null(shade_type)) {
		l$shading <- dist_add_shading(mapping = ggplot2::aes(x = x, y = y),
																	data = data,
																	shade_type = shade_type,
																	limits = shade_limits,
																	fill = shade_fill,
																	alpha = shade_alpha)
	}

	if(!is.null(seg_x)) {
		l$seg <- dist_add_segment(mapping = ggplot2::aes(x = x, y = y),
															data = data,
															seg_x = seg_x,
															linetype = seg_linetype,
															size = seg_size,
															color = seg_color)
	}

	l$curve <- dist_add_curve(mapping = ggplot2::aes(x = x, y = y),
														data = data,
														linetype = linetype,
														size = size,
														color = color,
														...)
	l
}
