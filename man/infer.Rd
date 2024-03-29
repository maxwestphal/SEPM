% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/infer.R
\name{infer}
\alias{infer}
\title{Hypothesis testing regarding model performances}
\usage{
infer(estimation, method = "maxT", transform = "none")
}
\arguments{
\item{estimation}{object of class SEPM.estimation (see \code{\link{estimate}}).}

\item{method}{character; specify the (multiple) test used (default: "maxT", currently the only choice)}

\item{transform}{character; specify a known transform, e.g. "none" (default), "logit" or
"asin.sqrt". Alternativly a object of class SEPM.transform created via
\code{\link{define_transform}}).}
}
\value{
An SEPM.inference object, including inference results, parameters estimates and
hypothesis.
}
\description{
This functions allows hypothesis testing regarding a hypothesis system based on some previously
derived parameter estimates (see \code{\link{estimate}}).
}
\examples{
set.seed(1337)
y <- rep(1:0, times=c(3,7))
yhat <- cbind(model1 = rep(1:0, 5),
             model2 = rep(0, 10),
             model3 = rep(1:0, times=c(2,8)))
define_hypothesis("accuracy", threshold = 0.75) \%>\%
 compare(predictions = yhat, labels = y) \%>\%
 estimate() \%>\%
 infer() \%>\%
 summary()
}
