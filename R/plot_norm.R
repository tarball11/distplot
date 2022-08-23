#' Generate Normal Distribution Data
#'
#' Given a mean and standard deviation value, generates a `tibble` with a range
#' of values of `x` and their corresponding normal density values as `y`. This
#' function is useful for generating distribution plots.
#'
#'
#' @param M Numeric scalar. Mean of the distribution.
#' @param SD Numeric scalar. Standard Deviation of the distribution.
#' @param sd_range Numeric scalar. Range of standard deviations to plot above
#'   and below `M`. If `x_min` and `x_max` are set, this is overwritten.
#' @param x_min Numeric scalar. Lower limit of the x-axis scale.
#' @param x_max Numeric scalar. Upper limit of the x-axis scale.
#' @param padding Numeric scalar. How much padding should be added to the range
#'   of `x` (expressed as proportion)? Defaults to 10% (0.1).
#' @param n Numeric scalar. Length of sequence for the range of `x` values to
#'   generate.
#'
#' @return `[tibble::tibble()]` object.
#' @export
#'
#' @examples
#' # Generate density table for standard normal distribution:
#' gen_normal_tbl()
#'
gen_normal_tbl <- function(M = 0,
													 SD = 1,
													 sd_range = 4,
													 x_min = M - (SD * sd_range),
													 x_max = M + (SD * sd_range),
													 padding = 0.1,
													 n = 1001) {
	# Check argument validity
	stopifnot(is.numeric(M), is.numeric(SD))
	stopifnot(length(M) == 1, length(SD) == 1)
	stopifnot(SD > 0)
	stopifnot(is.numeric(sd_range), sd_range > 0)
	stopifnot(is.numeric(x_min), is.numeric(x_max))
	stopifnot(x_min <= x_max)
	stopifnot(is.numeric(padding), padding < 1, padding >= 0)
	stopifnot(is.numeric(n), n >= 10)

	tibble::tibble(x = seq(x_min*(1 + padding), x_max*(1 + padding), length.out=n),
								 y = stats::dnorm(x = x, mean = M, sd = SD))
}

#' Generate Plot of Normal Distribution
#'
#' [plot_norm()] produces a ggplot of the normal distribution (generated
#' internally by a call to [gen_normal_tbl()]), automatically applying theming
#' ([distribution_thm()]) and scaling defaults ([distribution_scaling()]).
#' [add_normal()] adds a normal distribution layer to an existing plot.
#'
#' Both [plot_norm()] and [add_normal()] create a basic normal distribution plot
#' by creating a data frame and passing it to [ggplot2::ggplot()], then adding
#' the requested layers in a sensible order. [plot_norm()] creates a new plot,
#' whereas [add_normal()] can only add layers to an existing plot. Examples show
#' the ways this can be extended.
#'
#'
#' @param M Numeric scalar. Mean of the distribution.
#' @param SD Numeric scalar. Standard Deviation of the distribution.
#' @param sd_range Numeric scalar. Range of standard deviations to plot above
#'   and below `M`. If `x_min` and `x_max` are set, this is overwritten.
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
#' @param n Numeric scalar. Number of points along the curve to draw (i.e., the
#'   smoothness of the curve). Note: higher values will increase processor time
#'   and increase the size of image file.
#'
#' @return A `[ggplot2::ggplot()]` object.
#'
#' @seealso [gen_normal_tbl()], [distribution_thm()], [distribution_scaling()], [plot_dist()], [add_dist()]
#'  @export
#'
#' @examples
#'
#' # By default, produces a standard normal distribution:
#' plot_norm()
#'
#' # Can easily add shading and line segments (e.g., shading the tails):
#' lims = qnorm(p = c(0.025, 0.975))
#' p<- plot_norm(shade_type = 'tails', shade_limits = lims, shade_fill = "red",
#'               seg_x = lims, seg_size = 0.75, seg_linetype = 'dashed')
#' p
#'
#' # You shade multiple areas of the same distribution by adding an individual
#' # shading elements via dist_add_shading()
#' p + dist_add_shading(shade_type = 'between', limits = lims, fill = 'blue')
#'
#' # You can also layer shading of different regions:
#' plot_norm(shade_type = 'below', shade_limits = -1, shade_fill = 'blue') +
#'  dist_add_shading(shade_type = 'below', limits = 1, fill = "red")
#'
#'
#' # If you wish to show multiple distributions with different parameters,
#' # use add_normal() to an existing plot.
#' plot_norm() + add_normal(M = 1)
#'
#' # These can each have their own shading and segments:
#' plot_norm(shade_type = 'below', shade_limits = -1, shade_fill = 'blue') +
#'  add_normal(M = 1, shade_type = 'below', shade_limits = 1, shade_fill = "red")
#'
#' # You can also create an empty ggplot and add a normal curve to it.
#' # Note that you will have to add the theme and scaling manually:
#' p <- ggplot2::ggplot() + add_normal()
#' p
#' p + distribution_thm() + distribution_scaling(x_min = -4, x_max = 4)
#'
#' # However, you cannot add individual elements to an empty plot because the
#' # plot does not have any x-y data; only the added layer does.
#' \dontrun{p + dist_add_shading(shade_type = 'below', limits = 1, fill = "red")}
#'
#' # plot_norm() is essentially a wrapper for gen_normal_tbl() and plot_dist():
#' gen_normal_tbl() %>% plot_dist()
#'
#' # add_norm() is essentially a wrapper for gen_normal_tbl() and add_dist():
#' ggplot2::ggplot() + add_dist(data = gen_normal_tbl())
#'
plot_norm <- function(M = 0,
											SD = 1,
											sd_range = 4,
											x_min = M - (SD * sd_range),
											x_max = M + (SD * sd_range),
											x_breaks = seq(x_min, x_max, by=SD),
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
											n = 1001) {

	# Check argument validity
	stopifnot(is.numeric(M), is.numeric(SD))
	stopifnot(length(M) == 1, length(SD) == 1)
	stopifnot(SD > 0)
	stopifnot(is.numeric(sd_range), sd_range > 0)
	stopifnot(is.numeric(x_min), is.numeric(x_max))
	stopifnot(x_min <= x_max)
	stopifnot(is.numeric(n), n >= 10)

	# Generate density values
	tbl <- gen_normal_tbl(M = M,
								 SD = SD,
								 sd_range = sd_range,
								 x_min = x_min,
								 x_max = x_max,
								 n = n)

	# Create the plot
	plot_dist(data = tbl,
						x_min = x_min,
						x_max = x_max,
						x_breaks = x_breaks,
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

#' @rdname plot_norm
#'
#' @export
#'
add_normal <- function(M = 0,
											 SD = 1,
											 sd_range = 4,
											 x_min = M - (SD * sd_range),
											 x_max = M + (SD * sd_range),
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
											 n = 1001) {

	# Check argument validity
	stopifnot(is.numeric(M), is.numeric(SD), SD > 0)
	stopifnot(is.numeric(sd_range), sd_range > 0)
	stopifnot(is.numeric(x_min), is.numeric(x_max))
	stopifnot(x_min <= x_max)

	# Generate density values
	tbl <- gen_normal_tbl(M = M,
												SD = SD,
												sd_range = sd_range,
												x_min = x_min,
												x_max = x_max,
												n = n)

	add_dist(data = tbl,
					 x_min = x_min,
					 x_max = x_max,
					 x_breaks = x_breaks,
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
