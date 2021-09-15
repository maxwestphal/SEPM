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
#' @param lfc
#'
#' @return An SEPM.inference object, including inference results, parameters estimates and
#' hypothesis.
#'
#' @examples
#' set.seed(1)
#' y <- rep(0:1, each=100)
#' yhat <- sapply(seq(0.7, 0.9, 0.05),
#'  function(p){ifelse(rbinom(200, 1, p)==1, y, 1-y)})
#' define.hypothesis("accuracy", 0.8) %>%
#'  compare(predictions=yhat, labels=y) %>%
#'  estimate() %>%
#'  infer() %>%
#'  summary()
#' @export
infer <- function(estimation,
                  method = 'maxT',
                  transform = "none",
                  lfc="t")
{
  if(!is.SEPM.estimation(estimation)){
    stop("estimation needs to be an SEPM.estimation object, see ?estimate.")
  }

  e <- estimation$estimation[-length(estimation$estimation)]
  h <- estimation$hypothesis
  t <- define_transform(transform)

  # Case (1): single endpoint
  if(!h$co.primary){
    result <- test_se(e=e[[1]], h=h, t=t, m=method)
  }

  # Case (2): co-primary endpoints
  if(h$co.primary){
    result <- test_cp(e=e, h=h, t=t, m=method, lfc=lfc)
  }

  control <- list(method = method, transform = unclass(t), seed=seed)

  out <- list(inference = c(result, list(control = control)),
              estimation = estimation$estimation,
              hypothesis = h)
  class(out) <- append(class(out), "SEPM.inference")
  return(out)
}
