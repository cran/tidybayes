% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/density_bins.R
\name{density_bins}
\alias{density_bins}
\alias{histogram_bins}
\title{Density bins and histogram bins as data frames}
\usage{
density_bins(x, n = 101, ...)

histogram_bins(x, n = 30, breaks = n, ...)
}
\arguments{
\item{x}{A numeric vector}

\item{n}{Number of bins}

\item{...}{Additional arguments passed to \code{\link[=density]{density()}} or
\code{\link[=hist]{hist()}}.}

\item{breaks}{Used to set bins for \code{histogram_bins}. Can be number of bins (by default it is set to the value
of \code{n}) or a method for setting bins. See the \code{breaks} argument of \code{\link[=hist]{hist()}}.}
}
\value{
A data frame representing bins and their densities with the
following columns: \item{mid}{Bin midpoint} \item{lower}{Lower endpoint of
each bin} \item{upper}{Upper endpoint of each bin} \item{density}{Density
estimate of the bin}
}
\description{
Generates a data frame of bins representing the kernel density (or
histogram) of a vector, suitable for use in generating predictive
distributions for visualization. These functions were originally
designed for use with the now-deprecated \code{predict_curve()}, and
may be deprecated in the future.
}
\details{
These functions are simple wrappers to \code{\link[=density]{density()}} and
\code{\link[=hist]{hist()}} that compute density estimates and return their results
in a consistent format: a data frame of bins suitable for use with
the now-deprecated \code{\link[=predict_curve]{predict_curve()}}.

\code{density_bins} computes a kernel density estimate using
\code{\link[=density]{density()}}.

\code{histogram_bins} computes a density histogram using \code{\link[=hist]{hist()}}.
}
\examples{
\dontrun{

library(ggplot2)
library(dplyr)
library(brms)
library(modelr)

theme_set(theme_light())

m_mpg = brm(mpg ~ hp * cyl, data = mtcars)

step = 1
mtcars \%>\%
  group_by(cyl) \%>\%
  data_grid(hp = seq_range(hp, by = step)) \%>\%
  add_predicted_draws(m_mpg) \%>\%
  summarise(density_bins(.prediction), .groups = "drop") \%>\%
  ggplot() +
  geom_rect(aes(
    xmin = hp - step/2, ymin = lower, ymax = upper, xmax = hp + step/2,
    fill = ordered(cyl), alpha = density
  )) +
  geom_point(aes(x = hp, y = mpg, fill = ordered(cyl)), shape = 21, data = mtcars) +
  scale_alpha_continuous(range = c(0, 1)) +
  scale_fill_brewer(palette = "Set2")

}
}
\seealso{
See \code{\link[=add_predicted_draws]{add_predicted_draws()}} and \code{\link[=stat_lineribbon]{stat_lineribbon()}} for a better approach. These
functions may be deprecated in the future.
}
\author{
Matthew Kay
}
\keyword{manip}
