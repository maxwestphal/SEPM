% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/select.R
\name{select}
\alias{select}
\title{Statistical Evaluation of Prediction Models (SEPM)}
\usage{
select(estimation, method = "rank", ..., args = NULL, weights = 1,
  n.test = NA)
}
\arguments{
\item{estimation}{SEPM.estimation object derived via estimate()}

\item{method}{character, specifying how models (algorithms) for the evaluation study shall be
selected. See}

\item{...}{further named arguments to selection method}

\item{args}{list of named arguments to selection method}

\item{weights}{vector of numeric weights (default 1). Specifies how performance/error is weighted
accross distinct subpopulation for model selection. Only relevent for co-primary endpoints and
ignored otherwise.}

\item{n.test}{integer, (assumed) sample size of evaluation study used for power calculation. If
n.test = NA (default), no power estimation is conducted.}
}
\value{
SEPM.selection with the following attributes: model (model indices), model.names, power.
}
\description{
Select models based on hold-out-validation during the learning phase.
}
\details{
abcdef
}
\examples{
# TODO: EXAMPLE
}
