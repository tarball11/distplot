% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_norm.R
\name{gen_normal_tbl}
\alias{gen_normal_tbl}
\title{Generate Normal Distribution Data}
\usage{
gen_normal_tbl(
  M = 0,
  SD = 1,
  sd_range = 4,
  x_min = M - (SD * sd_range),
  x_max = M + (SD * sd_range),
  padding = 0.1,
  n = 1001
)
}
\arguments{
\item{M}{Numeric scalar. Mean of the distribution.}

\item{SD}{Numeric scalar. Standard Deviation of the distribution.}

\item{sd_range}{Numeric scalar. Range of standard deviations to plot above
and below \code{M}. If \code{x_min} and \code{x_max} are set, this is overwritten.}

\item{x_min}{Numeric scalar. Lower limit of the x-axis scale.}

\item{x_max}{Numeric scalar. Upper limit of the x-axis scale.}

\item{padding}{Numeric scalar. How much padding should be added to the range
of \code{x} (expressed as proportion)? Defaults to 10\% (0.1).}

\item{n}{Numeric scalar. Length of sequence for the range of \code{x} values to
generate.}
}
\value{
\verb{[tibble::tibble()]} object.
}
\description{
Given a mean and standard deviation value, generates a \code{tibble} with a range
of values of \code{x} and their corresponding normal density values as \code{y}. This
function is useful for generating distribution plots.
}
\examples{
# Generate density table for standard normal distribution:
gen_normal_tbl()

}
