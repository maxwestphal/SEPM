#' Statistical Evaluation of Prediction Models (SEPM)
#'
#' Select models based on hold-out-validation during the learning phase.
#'
#' @param estimation SEPM.estimation object derived via estimate()
#' @param method character, specifying how models (algorithms) for the evaluation study shall be
#' selected. See
#' @param ... further named arguments to selection method
#' @param args list of named arguments to selection method
#' @param weights vector of numeric weights (default 1). Specifies how performance/error is weighted
#' accross distinct subpopulation for model selection. Only relevent for co-primary endpoints and
#' ignored otherwise.
#' @param n.test integer, (assumed) sample size of evaluation study used for power calculation. If
#' n.test = NA (default), no power estimation is conducted.
#'
#' @details abcdef
#'
#' @return SEPM.selection with the following attributes: model (model indices), model.names, power.
#'
#' @examples # TODO: EXAMPLE
#' @export
#' @importFrom Matrix bdiag
select <- function(estimation, #
                   method = "rank",
                   ...,
                   args = NULL,
                   weights = 1,
                   n.test = NA){
  args <- c(list(), list(...), args)
  e <- estimation(estimation); h <- hypothesis(estimation)

  control <- select_control(method); control$args <- args

  check.args.select(weights, n.test, h)

  M <- length(e[[1]]$model.name)
  w <- rep(weights, length.out=length(e)); w <- w/sum(w)
  W <- do.call(cbind, lapply(w, function(x)diag(rep(x, M))))
  e.comb <- as.numeric(do.call(cbind, lapply(e, function(x)t(x$theta.hat))))
  cov.comb <- as.matrix(Matrix::bdiag(lapply(e, function(x)x$sigma.hat)))
  estimate <- as.numeric(W %*% e.comb)
  cov <- W %*% cov.comb %*% t(W)

  result <- list(model.name=e[[1]]$model.name)
  result$selected <- do.call(control$mapping, c(list(estimate=estimate, cov=cov, h=h), args))
  result$M <- M; result$S <- sum(result$selected)
  result$n.test <- n.test; result$power <- NA


  if(!is.na(n.test)){
    result$power <- estimate_power(s=result$selected, e=e, h=h, n=n.test)
  }

  s <- list(result = result,
            control = c(unclass(control), weights = weights))

  out <- c(list(selection = s), unclass(estimation))

  class(out) <- append(class(out), "SEPM.selection")
  return(out)
}





# ## TRAIN CANDIDATE MODELS BASED ON TRAINING DATA
# ...
#
# ## MODEL SELECTION
# define.hypothesis() %>%
#   compare(predictions=val.pred, labels=val.labels) %>%
#   estimate() %>%
#   select()
#
# ## RETRAIN SELECTED MODELS ON LEARNING DATA
# ...
#
# ## EXAMPLE PIPELINE
# define.objective() %>%
#   define.hypothesis() %>%
#   compare(predictions=eval.pred, labels=eval.labels) %>%
#   estimate() %>%
#   evaluate()
