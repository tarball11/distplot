#' Add Distribution Elements to a Plot
#'
#' Workhorse functions to add individual elements to an existing plot. These are
#' generally intended for internal use by the higher level functions (e.g.,
#' `plot_dist()`, `add_dist()`, etc.), but can be useful to users in some contexts.
#'
#' `dist_add_elements()` will optionally add the distribution curve, shading,
#' and vertical line segments at once, in an order that ensures that the curve
#' does not get overlapped by the shading or line segments. The other functions
#' add one individual element (curve: `dist_add_curve()`, shading:
#' `dist_add_shading()`, vertical line segment: `dist_add_segment()`), allowing
#' for more flexibility or the possibility of multiple layers..
#'
#'
#' @param data Optional data frame containing columns named `x` and `y` (e.g.,
#'   created with one of the `gen_*_tbl()` functions, such as
#'   `gen_norm_dist()`). The `x`column contains the full range of values across
#'   the x-axis, and the `y` column contains the density value for that value of
#'   `x`. If not provided, will use the data already associated with the plot
#'   object.
#' @param mapping `[ggplot2::aes()]` mapping of parameters (defaults to `ggplot2::aes(x = x, y = y)`)
#' @param linetype,linewidth,color Parameters passed to [ggplot2::geom_line()] to
#'   draw the curve (defaults to a solid black line).
#' @param shade_type Character scalar. Type of shading to apply to a specific
#'   area under the curve. Options are `below`, `above`, `between`, `tails`. If
#'   no shading is desired, set to `NULL` (the default).
#' @param shade_limits Numeric. When applying shading, specifies the lower/upper
#'   limit of the shading to be applied. Use a single value when `shade_type` is
#'   set to `below` or `above`, and a vector of length 2 when `shade_type` is
#'   set to `between` or `tails`.
#' @param shade_fill,shade_alpha Parameters passed to [ggplot2::geom_area()] to
#'   shade the area under the curve. (defaults to red with some translucency).
#' @param seg_x Numeric vector of `x` values where the segments should be drawn.
#'   The segments will go from zero (x-axis) to the height of the density curve.
#' @param seg_linetype,seg_linewidth,seg_color Parameters passed to
#'   [ggplot2::geom_line()] to draw the line segments (defaults to a solid black
#'   line).
#' @param ... Additional arguments passed to `[ggplot2::ggplot2()]` geoms
#'   ([ggplot2::geom_line()], [ggplot2::geom_area()],
#'   [ggplot2::geom_segment()]).
#'
#' @return ggplot2 object.
#' @export
#'
#' @examples
#' # Generate some data and (relatively) blank plot:
#' norm.tbl <- gen_normal_tbl(M = 0, SD = 1)
#' p <- plot_dist(data = norm.tbl)
#'
#' p + dist_add_elements(data = gen_normal_tbl(M = 1),
#' 											linetype = 'dotted', shade_type = 'above', shade_limits = 1,
#' 											seg_x = 1, seg_linetype = 'dotted')
#'
#' p + dist_add_curve(data = gen_normal_tbl(M = 2))
#'
#' p + dist_add_shading(shade_type = 'below', shade_limits = 1, shade_fill = "red")
#'
#' p + dist_add_segment(seg_x = -2:2)
#'
#' p +
#' 	dist_add_shading(shade_type = 'below', shade_limits = -2, shade_fill = 'blue') +
#' 	dist_add_shading(shade_type = 'between', shade_limits = c(-2,-1), shade_fill = 'red') +
#' 	dist_add_shading(shade_type = 'between', shade_limits = c(-1,0), shade_fill = 'blue') +
#' 	dist_add_shading(shade_type = 'between', shade_limits = c(0,1), shade_fill = 'red') +
#' 	dist_add_shading(shade_type = 'between', shade_limits = c(1,2), shade_fill = 'blue') +
#' 	dist_add_shading(shade_type = 'above', shade_limits = 2, shade_fill = 'red') +
#' 	dist_add_segment(seg_x = -2:2)
#'
dist_add_elements <- function(data = NULL,
															linetype = "solid",
															linewidth = 1,
															color = "black",
															shade_type = NULL,
															shade_limits = NULL,
															shade_fill = "red",
															shade_alpha = 0.7,
															seg_x = NULL,
															seg_linetype = "solid",
															seg_linewidth = 1,
															seg_color = "black",
															...) {

	if(!is.null(data)) stopifnot(is.data.frame(data))
	stopifnot(is.character(linetype))
	stopifnot(is.numeric(linewidth))
	stopifnot(is.character(color))

	l <- list()

	if(!is.null(shade_type)) {
		l$shading <- dist_add_shading(mapping = ggplot2::aes(x = x, y = y),
																	data = data,
																	shade_type = shade_type,
																	shade_limits = shade_limits,
																	shade_fill = shade_fill,
																	shade_alpha = shade_alpha)
	}

	if(!is.null(seg_x)) {
		l$seg <- dist_add_segment(mapping = ggplot2::aes(x = x, y = y),
															data = data,
															seg_x = seg_x,
															seg_linetype = seg_linetype,
															seg_linewidth = seg_linewidth,
															seg_color = seg_color)
	}

	l$curve <- dist_add_curve(mapping = ggplot2::aes(x = x, y = y),
														data = data,
														linetype = linetype,
														linewidth = linewidth,
														color = color,
														...)
	l
}


#' @rdname dist_add_elements
#'
#' @export
#'
dist_add_curve <- function(data = NULL,
													 linetype = "solid",
													 linewidth = 1,
													 color = "black",
													 ...) {

	ggplot2::geom_line(data = data,
										 linetype = linetype,
										 linewidth = linewidth,
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

#' @rdname dist_add_elements
#'
#' @export
#'
dist_add_shading <- function(mapping = ggplot2::aes(x = x, y = y),
														 data = NULL,
														 shade_type = c('above', 'below', 'between', 'tails'),
														 shade_limits = NULL,
														 shade_fill = 'red',
														 shade_alpha = 0.70,
														 ...) {
		ggplot2::layer(
			stat = StatDistShading,
			data = data,
			mapping = mapping,
			geom = "area",
			position = "identity",
			params = list(shade_type = shade_type,
										limits = shade_limits,
										fill = shade_fill,
										alpha = shade_alpha,
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

#' @rdname dist_add_elements
#'
#' @export
#'
dist_add_segment <- function(mapping = ggplot2::aes(x = x, y = y),
														 data = NULL,
														 seg_x,
														 seg_linetype = "solid",
														 seg_linewidth = 1,
														 seg_color = "black",
														 ...) {
	ggplot2::layer(
		stat = StatDistSegment,
		data = data,
		mapping = mapping,
		geom = "segment",
		position = "identity",
		params = list(seg_x = seg_x,
									linetype = seg_linetype,
									linewidth = seg_linewidth,
									color = seg_color,
									...)
	)
}
