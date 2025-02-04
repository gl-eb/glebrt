% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/breaks_limits.R
\name{breaks_limits}
\alias{breaks_limits}
\title{Breaks Including Limits}
\usage{
breaks_limits(x, n = 5, ...)
}
\arguments{
\item{x}{(numeric) Data mapped onto numeric axis}

\item{n}{(numeric) Desired number of breaks. Passed onto}

\item{...}{Other arguments passed on to `labeling::extended()`}
}
\description{
Return a vector of breaks for a numeric axis.
Based on `scales::breaks_extended()`, but will include the (approximate) axis limits as breaks
}
\examples{
x <- seq(1, 21)
breaks_limits(x)
}
