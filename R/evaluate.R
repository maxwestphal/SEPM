#' Statistical Evaluation of Prediction Models
#'
#' Conduct the complete evaluation pipeline: \code{hypothesis \%>\% compare(...) \%>\% estimate(...) \%>\%
#' infer(...)}.
#'
#' @param hypothesis an \code{SEPM.hypothesis}, see \code{\link{define_hypothesis}}.
#' @param ... arguments passed to compare which need to be named, see \code{\link{compare}}.
#' @param control character specifying known estimation method or \code{SEPM.control} object,
#' see \code{\link{estimate_control}}.
#' @param args a named list including further arguments to control$mapping used for the estimation, see
#' \code{\link{estimate}}.
#' @param method statistical test employed, default: "maxT" (currently the only option)
#' @param transform transform of parameter estimates: "raw" (default), "logit", "asin.sqrt".
#' Alternativly a custom transform may be specified via \code{\link{define_transform}}.
#' @param seed random seed passed to \code{\link{infer}}, should NOT be changed!
#' @param messages indicate if info messages should be displayed or not (default: TRUE).
#'
#' @return An SEPM.evaluation object, including inference results, parameters estimates and
#' hypothesis.
#'
#' @details evaluate(hypothesis, ...) is a wrapper for \code{hypothesis \%>\% \link{compare}(...)
#' \%>\% \link{estimate}(...) \%>\% \link{infer}(...)}. The \code{hypothesis} still needs to be defined beforehand via
#' \code{\link{define_hypothesis}}.
#'
#' @examples
#' set.seed(1)
#' theta <- seq(0.7, 0.9, 0.05)
#' n <- 200
#' l <- rep(0:1, each=n/2)
#' p <- sapply(theta, function(p) ifelse(rbinom(n, 1, p)==1,  l, 1-l))
#' h <- define_hypothesis("accuracy", threshold = 0.8)
#' e <- evaluate(hypothesis = h, predictions = p, labels = l)
#' summary(e)
#'
#' @export
evaluate <- function(hypothesis,
                     ...,
                     estimate = "default",
                     estimate.args = NULL,
                     infer = "maxT",
                     transform = "none",
                     seed = 1,
                     messages = TRUE)
{
  eval_expr(
    hypothesis %>%
      SEPM::compare(...) %>%
      SEPM::estimate(method = estimate, args = estimate.args) %>%
      SEPM::infer(method = infer, transform = transform, seed = seed),
    messages = messages, warnings = TRUE)
}

