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
#' @param lfc character, either "t" or "d"
#'
#' @return An SEPM.inference object, including inference results, parameters estimates and
#' hypothesis.
#'
#' @examples
#' set.seed(1337)
#' y <- rep(1:0, times=c(3,7))
#' yhat <- cbind(model1 = rep(1:0, 5),
#'              model2 = rep(0, 10),
#'              model3 = rep(1:0, times=c(2,8)))
#' define_hypothesis("accuracy", threshold = 0.75) %>%
#'  compare(predictions = yhat, labels = y) %>%
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

  ## Case (1): single endpoint
  if(!h$co.primary){
    result <- test_se(e=e[[1]], h=h, t=t, m=method)
  }

  ## Case (2): co-primary endpoints
  if(h$co.primary){
    result <- test_cp(e=e, h=h, t=t, m=method, lfc=lfc)
  }

  control <- list(method = method, transform = unclass(t))

  out <- list(inference = c(result, list(control = control)),
              estimation = estimation$estimation,
              hypothesis = h)
  class(out) <- append(class(out), "SEPM.inference")
  return(out)
}
