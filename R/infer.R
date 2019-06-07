#' Hypothesis testing regarding model performances
#'
#' This functions allows hypothesis testing regarding a hypothesis system based on some previously
#' derived parameter estimates (see \code{\link{estimate}}).
#'
#' @param estimation object of class SEPM.estimation (see \code{\link{estimate}}).
#' @param method character; specify the (multiple) test used (default: "maxT", currently the only choice)
#' @param transform character; specify a known transform, e.g. "none" (default), "logit" or
#' "asin.sqrt". Alternativly a object of class SEPM.transform created via
#' \code{\link{define_transform}}).
#' @param seed integer; specify the random seed. For reproducibility, this should NOT be changed.
#'
#' @return An SEPM.evaluation object, including inference results, parameters estimates and
#' hypothesis.
#'
#' @details
#'
#' The overall goal is control of the type 1 error rate (rate of false positive claims over
#' hypothetical repetitions of the evaluation study).
#' Hence, this function is NOT designed for 'trial and error'!
#' Iterating over different models, performance measures, benchmarks etc. will surely lead to
#' an increased rate of errouneous conclusions drawn from the evaluation study. Ideally, this
#' function should only called once per evaluation study. The provided examples may be helpful to
#' learn how to call this function correctly.
#'
#' The random seed is controlled explicitly (and set to 1 by default). The reason for this is the
#' slight dependence of some internal functions on the random seed (e.g. qmvnorm). Hence different
#' seeds give (slightly) different results. Thus seed should NOT be changed (unless, for some reason,
#' numerical issues are encountered).
#'
#' @examples
#' set.seed(1)
#' y <- rep(0:1, each=100)
#' yhat <- sapply(seq(0.7, 0.9, 0.05),
#'  function(p){ifelse(rbinom(200, 1, p)==1, y, 1-y)})
#' e <- define.hypothesis("accuracy", 0.8) %>%
#'  compare(predictions=yhat, labels=y) %>%
#'  estimate() %>% test()
#' summary(e)
#' @export
infer <- function(estimation,
                  method = 'maxT',
                  transform = "none",
                  seed = 1)
{
  if(!is.SEPM.estimation(estimation)){
    stop("estimation needs to be an SEPM.estimation object, see ?estimate.")
  }

  e <- estimation$estimation[-length(estimation$estimation)]
  h <- estimation$hypothesis
  t <- define_transform(transform)

  set.seed(seed)

  # Case (1): single endpoint
  if(!h$co.primary){
    result <- test_se(e=e[[1]], h=h, t=t, m=method)
  }

  # Case (2): co-primary endpoints
  if(h$co.primary){
    result <- test_cp(e=e, h=h, t=t, m=method)
  }

  control <- list(method = method, transform = unclass(t), seed=seed)
  #attr(result, "control") <- control

  out <- list(inference = c(result, list(control = control)),
              estimation = estimation$estimation,
              hypothesis = h)
  class(out) <- append(class(out), "SEPM.inference")
  return(out)
}

### backwards compatability
#' export
#test <- infer

