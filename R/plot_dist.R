#' Generate Distribution Plot
#'
#' [plot_dist()] produces a ggplot of a distribution, automatically applying
#' theming ([distribution_thm()]) and scaling defaults
#' ([distribution_scaling()]). [add_dist()] adds a distribution layer to an
#' existing plot.
#'
#' Both [plot_dist()] and [add_dist()] create a basic distribution plot by
#' taking the provided data frame and passing it to [ggplot2::ggplot()], then
#' adding the requested layers in a sensible order. [plot_dist()] creates a new
#' plot, whereas [add_dist()] can only add layers to an existing plot. Examples
#' show the ways this can be extended.
#'
#'
#' @param data Data frame containing columns named `x` and `y` (e.g., created
#'   with one of the `gen_*dist*_tbl()` functions). The `x`column contains the
#'   full range of values across the x-axis, and the `y` column contains the
#'   density value for that value of `x`.
#' @param x_min Numeric scalar. Lower limit of the x-axis scale.
#' @param x_max Numeric scalar. Upper limit of the x-axis scale.
#' @param x_breaks Numeric vector. Vector of values to use for the x-axis. If
#'   not provided, inferred to be integers ranging from x_min to x_max. Set to
#'   `NULL` to remove values.
#' @param n Numeric scalar. Number of points along the curve to draw (i.e., the
#'   smoothness of the curve). Note: higher values will increase processor time
#'   and increase the size of image file.
#' @param linetype,size,color Parameters passed to [ggplot2::geom_line()] to
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
#' @param seg_linetype,seg_size,seg_color Parameters passed to
#'   [ggplot2::geom_line()] to draw the line segments (defaults to a solid black
#'   line).
#'
#' @return A `ggplot` object.
#' @export
#'
#' @examples
#'
#' # Generate some data:
#'
#' plot_dist()
#'
#' # Can easily add shading and line segments (e.g., shading the tails):
#' lims = qnorm(p = c(0.025, 0.975))
#' p<- plot_dist(shade_type = 'tails', shade_limits = lims, shade_fill = "red",
#'               seg_x = lims, seg_size = 0.75, seg_linetype = 'dashed')
#' p
#'
#' # You shade multiple areas of the same distribution by adding an individual
#' # shading elements via dist_add_shading()
#' p + dist_add_shading(shade_type = 'between', limits = lims, fill = 'blue')
#'
#' # You can also layer shading of different regions:
#' plot_dist(shade_type = 'below', shade_limits = -1, shade_fill = 'blue') +
#'  dist_add_shading(shade_type = 'below', limits = 1, fill = "red")
#'
#'
#' # If you wish to show multiple distributions with different parameters,
#' # use add_dist() to an existing plot.
#' plot_dist() + add_dist(M = 1)
#'
#' # These can each have their own shading and segments:
#' plot_dist(shade_type = 'below', shade_limits = -1, shade_fill = 'blue') +
#'  add_dist(M = 1, shade_type = 'below', shade_limits = 1, shade_fill = "red")
#'
#' # You can also create an empty ggplot and add a normal curve to it.
#' # Note that you will have to add the theme and scaling manually:
#' p <- ggplot2::ggplot() + add_dist()
#' p
#' p + distribution_thm() + distribution_scaling(x_min = -4, x_max = 4)
#'
#' # However, you cannot add individual elements to an empty plot because the
#' # plot does not have any x-y data; only the added layer does.
#' \dontrun{p + dist_add_shading(shade_type = 'below', limits = 1, fill = "red")}
#'
plot_dist <- function(data,
											x_min = -4,
											x_max = 4,
											x_breaks = seq(x_min, x_max, by=1),
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
											seg_color = "black") {

	# Check argument validity
	stopifnot(is.data.frame(data))
	stopifnot(is.numeric(data$x), is.numeric(data$y))
	stopifnot(is.numeric(x_min), is.numeric(x_max))
	stopifnot(x_min <= x_max)

	# Create the plot
	ggplot2::ggplot(data = data,
									mapping = ggplot2::aes(x = x, y = y)) +
	# Add the theme
		distribution_thm() +
	# Add the scaling
		distribution_scaling(x_min = x_min,
												 x_max = x_max,
												 x_breaks = x_breaks) +
	# Add the curve, shading, and segments
		dist_add_elements(linetype = linetype,
											size = size,
											color = color,
											shade_type = shade_type,
											shade_limits = shade_limits,
											shade_fill = shade_fill,
											shade_alpha = shade_alpha,
											seg_x = seg_x,
											seg_linetype = seg_linetype,
											seg_size = seg_size,
											seg_color = seg_color)

}


#' @rdname plot_dist
#'
#' @export
#'
add_dist <- function(data,
										 x_min = -4,
										 x_max = 4,
										 x_breaks = seq(x_min, x_max, by=1),
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
										 seg_color = "black") {

	# Check argument validity
	stopifnot(is.data.frame(data))
	stopifnot(is.numeric(data$x), is.numeric(data$y))
	stopifnot(is.numeric(x_min), is.numeric(x_max))
	stopifnot(x_min <= x_max)

	dist_add_elements(data = data,
										linetype = linetype,
										size = size,
										color = color,
										shade_type = shade_type,
										shade_limits = shade_limits,
										shade_fill = shade_fill,
										shade_alpha = shade_alpha,
										seg_x = seg_x,
										seg_linetype = seg_linetype,
										seg_size = seg_size,
										seg_color = seg_color)
}
