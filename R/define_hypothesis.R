#' Define a hypothesis (system)
#'
#' This function enables the definition of a hypothesis (system) for a subsequent
#' evaluation study of multiple prediction models.
#'
#' @param target either a character corresponding to a known target (e.g. "accuracy"), call
#' define_hypothesis() for a list of known targets. Alternativly target may be a custom SEPM.target,
#' see ?define_target.
#' @param threshold a numeric threshold defining the null level of the performance or error measure
#' specified by objective$target.
#' @param comparator integer, specifying which of the candidate models is the comparator.
#' Alternatively the model name of the comparator may be specified as a character string.
#' @param delta numeric, added to the right hand side of the hypothesis, to allow, e.g.
#' non-inferiority designs (delta < 0).
#' @param alternative character, specifying the alternative hypothesis, must be either
#' "two.sided" (default), "greater" or "less". May be abbreviated.
#' @param alpha numeric, specifying the significance level (between 0 and 0.5)
#'
#' @details Only threshold or comparator can be specified, not both.
#' If target$co.primary is TRUE, threshold needs to have length 2 where the first element is associated
#' for the threshold in the positive class (sensitiviy) and the second element corresponds to the
#' negative class threshold (specificity).
#' If threshold is specified as \eqn{\theta_0}, the null hypothesis is given as
#' \eqn{H_m: \theta_m \le \theta_0 + \delta} for all candidate models \eqn{m} (for alternative =
#' "greater"). If comparator is specified, e.g. as 1, the null is given as
#' \eqn{H_m: \theta_m - \theta_1 \le \delta} for m > 1, i.e. the number of hypotheses is the number
#' of models minus one. Similarly for alternative = "less" or "two.sided".
#'
#' @return An SEPM.hypothesis, a list with all relevant attributes that defines the goal of the
#' evaluation study.
#'
#' @examples
#' tar <- define_target("accuracy")
#' tar
#' define.hypothesis(tar, threshold=0.8)
#' @export
define_hypothesis <- function(target,
                              threshold,
                              comparator,
                              delta = 0,
                              alternative = c("two.sided", "less", "greater"),
                              alpha = 0.05){
  ## Process target:
  target <- define_target(target)
  if(is.null(target)){return(invisible())}
  ## Check args and add hypothesis part to SEPM.target:
  if(is.SEPM.target(target)){
    alternative <- match.arg(alternative)
    hyp <- construct_hypothesis(threshold, comparator, delta, alternative, alpha, target$co.primary)
    out <- c(target, hyp)
    class(out) <- append(class(out), "SEPM.hypothesis")
    return(out)
  }
  stop("target needs to be a character specifying a known target or a SEPM.target object!")
}
