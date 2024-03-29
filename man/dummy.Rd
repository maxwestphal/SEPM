% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dummy.R
\name{dummy}
\alias{dummy}
\title{Generate (uncorrelated) dummy data}
\usage{
dummy(target = "accuracy", n = c(100, 50), theta = seq(0.7, 0.9, 0.05))
}
\arguments{
\item{target}{character, e.g. "accuarcy" or "sensspec"}

\item{n}{integer, sample size(s)}

\item{theta}{numeric, scale parameter}
}
\value{
dummy data
}
\description{
Generate (uncorrelated) dummy data
}
\details{
See \code{\link{dummy_accuracy}} or \code{\link{dummy_sensspec}}.
}
