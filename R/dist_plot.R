#' ggplot Theme for Distributions
#'
#' Bare bones theme (based on [ggplot2::theme_classic()]) for plotting
#' distributions with sensible defaults. Makes it easy to add/remove the ticks
#' and line for the x- and y-axes. For plotting functions this is automatically
#' applied, along with other tweaks to the x- and y-axis scales. Apply theme by
#' adding function to an existing ggplot object (see examples).
#'
#' @param x_ticks Logical. Should the plot include ticks on the x-axis?
#' @param x_line Logical. Should the plot show the horizontal line of the
#'   x-axis? (If set to `FALSE`, recommend setting `x_ticks` to `FALSE` as
#'   well.)
#' @param y_ticks Logical. Should the plot include ticks on the y-axis?
#' @param y_line Logical. Should the plot show the vertical line of the y-axis?
#'   (If set to `FALSE`, recommend setting `x_ticks` to `FALSE` as well.)
#' @param ... Additional parameters passed to [ggplot2::theme_classic()].
#'
#' @return `[ggplot::theme()]` object.
#' @export
#'
#' @examples
#'
#' p <- tibble::tibble(x = seq(-4, 4, length.out = 1001), y = dnorm(x)) %>%
#' ggplot2::ggplot(ggplot2::aes(x = x, y = y)) +
#' ggplot2::geom_line()
#' p + distribution_thm()
#'
#' p + distribution_thm(y_line = TRUE, y_ticks = TRUE)
#'
distribution_thm <- function(x_ticks = TRUE,
														 x_line = TRUE,
														 y_ticks = FALSE,
														 y_line = FALSE,
														 ...) {
	stopifnot(is.logical(x_ticks), is.logical(x_line))
	stopifnot(is.logical(y_ticks), is.logical(y_line))

	l <- list(ggplot2::theme_classic(...),
						ggplot2::theme(axis.title = ggplot2::element_text(face='bold.italic'),
													 axis.text.x = ggplot2::element_text(color = 'black')))

	# If requested, remove the x- and y-axis ticks/labels
	if(!x_ticks)
		l <- c(l, list(ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
																	axis.text.x = ggplot2::element_blank())))

	if(!x_line)
		l <- c(l, list(ggplot2::theme(axis.line.x = ggplot2::element_blank(),
																	axis.title.x = ggplot2::element_blank())))

	if(!y_ticks)
		l <- c(l, list(ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
																	axis.text.y = ggplot2::element_blank())))

	if(!y_line)
		l <- c(l, list(ggplot2::theme(axis.line.y = ggplot2::element_blank(),
																	axis.title.y = ggplot2::element_blank())))

	return(l)
}


#' Distribution Scale Aesthetics
#'
#' Applies sensible defaults to the properties of the x- and y-axis scales when
#' plotting distributions, given range of x. Apply values by adding function to
#' an existing ggplot object (see examples). Primarily used internally as part
#' of the plotting functions (e.g., [plot_norm()]).
#'
#' @param x_min Numeric scalar. Lower limit of the x-axis scale.
#' @param x_max Numeric scalar. Upper limit of the x-axis scale.
#' @param x_breaks Numeric vector. Vector of values to use for the x-axis. If
#'   not provided, inferred to be integers ranging from x_min to x_max. Set to
#'   `NULL` to remove values.
#' @param xlab Character scalar. Title of x-axis.
#' @param ylab Character scalar. Title of y-axis. (Note: if using
#'   [distribution_thm()], setting `y_line` set to `FALSE` will suppress the
#'   y-axis title regardless of the value of `ylab` set here.)
#'
#' @export
#'
#' @examples
#'
#' p <- tibble::tibble(x = seq(-4, 4, length.out = 1001), y = dnorm(x)) %>%
#' ggplot2::ggplot(ggplot2::aes(x = x, y = y)) +
#' ggplot2::geom_line() + distribution_thm()
#' p + distribution_scaling(x_min = -3.5, x_max = 3.5, x_breaks = -3:3)
#'
#' p + distribution_scaling(x_min = -4, x_max = 4, x_lab = 'Z')
#'
distribution_scaling <- function(x_min,
																 x_max,
																 x_breaks = seq(x_min, x_max, by=1),
																 xlab = '',
																 ylab = '') {
	stopifnot(is.numeric(x_min), length(x_min) == 1)
	stopifnot(is.numeric(x_max), length(x_max) == 1)
	if(!is.null(x_breaks)) stopifnot(is.numeric(x_breaks))
	stopifnot(is.character(as.character(xlab)), length(xlab) == 1)
	stopifnot(is.character(as.character(ylab)), length(ylab) == 1)

	list(ggplot2::scale_y_continuous(name = ylab,
			 														expand = ggplot2::expansion(mult = c(0, .1))),
			 ggplot2::scale_x_continuous(name = xlab,
			 														breaks = x_breaks,
			 														expand = c(0.05,0)),
			 ggplot2::coord_cartesian(xlim = c(x_min, x_max)))
}

