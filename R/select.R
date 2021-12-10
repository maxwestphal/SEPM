#' Statistical Evaluation of Prediction Models (SEPM)
#'
#' Select models based on hold-out-validation during the learning phase.
#'
#' @param comparison SEPM.comparison
#' @param method character, specifying how models (algorithms) for the evaluation study shall be
#' selected. See
#' @param ... further named arguments to selection method
#' @param args list of named arguments to selection method
#'
#' @details Select models based on empirical performances prior to evaluation study, see
#' \code{\link{select_best}}, \code{\link{select_close}}, \code{\link{select_optimal}}
#'
#' @return indices of selected model
#'
#' @export
#' @importFrom Matrix bdiag
select <- function(comparison,
                   method = c("best", "close", "optimal", "user"),
                   ...,
                   args = list()){
  method <- match.arg(method)
  args <- c(list(comparison=comparison), list(...), args)

  do.call(paste0("select_", method), args=args)
}

select_user <- function(comparison,
                        sel=NULL,
                        ...){
  stopifnot(!is.null(sel))
  adm <- 1:ncol(comparison$comparison[[1]])
  stopifnot(all(sel %in% adm))
  return(sel)
}

#' Select prediction models close to the best
#'
#' @param comparison SEPM.comparison
#' @param k numeric, multiplier
#' @param mode character, "weighted" or "min"
#' @param threshold numeric, length corresponds to number of subgroups
#' @param max_models integer, maximum number of models to be selected
#' @param regu numeric, regularization parameter
#' @param break_ties logical, default: TRUE
#' @param ... further arguments (currently ignored)
#'
#' @return a vector of selected model indices
#' @export
select_close <- function(comparison,
                         k=1,
                         mode = c("weighted", "min"),
                         threshold = c(0.5, 0.5),
                         max_models = Inf,
                         regu=1,
                         break_ties = TRUE,
                         ...){
  mode <- match.arg(mode)

  max_models <- min(ncol(comparison$comparison[[1]]), round(max_models))

  n <- sapply(comparison$comparison, nrow) + (2*regu)

  if(comparison$hypothesis$target == "accuracy"){}

  w <- switch(comparison$hypothesis$target,
              1,
              accuracy = 1,
              sensspec = t2w(threshold))

  est <- lapply(comparison$comparison, function(x){(colSums(x)+regu)/(nrow(x)+2*regu)})
  stopifnot(length(threshold) == length(est))
  delta <- switch(mode,
                  weighted = apply(sapply(1:length(est), function(g) est[[g]]*w[g]), 1, sum),
                  min = apply(sapply(1:length(est), function(g) est[[g]]-threshold[g]), 1, min))

  se <- sapply(1:length(est), function(g){ sqrt( est[[g]]*(1-est[[g]])/n[g] ) })
  opt <- switch(comparison$hypothesis$type,
                error=min(delta),
                performance=max(delta))
  best <- unname(which(delta == opt))
  if(break_ties){best <- sample(rep(best, 2), 1)}

  sel <- switch(mode,
                weighted = which(abs(delta - delta[best]) <= k*sqrt(sum((w^2)* (se[best, ])^2)) ),
                min = which(apply(sapply(1:length(est), function(g){
                  abs(est[[g]] - est[[g]][best]) <= k*se[best, g]} ), 1, sum) == length(est)) )

  ord <- order(rank(delta[sel], ties.method = ifelse(break_ties, "random", "average")),
               decreasing = ifelse(comparison$hypothesis$type == "performance", TRUE, FALSE))
  sel_ord <- sel[ord][1:min(max_models, length(sel))]

  return(unname(sel_ord))
}

t2w <- function(threshold){
  w1 <- 1/((1-threshold[1])/(1-threshold[2]) + 1)
  return(c(w1, 1-w1))
}

#' Return empirically best prediction model
#'
#' @param comparison SEPM.comparison
#' @param mode character, "weighted" or "min"
#' @param threshold numeric, length corresponds to number of subgroups
#' @param regu numeric, regularization parameter
#' @param break_ties logical, default: TRUE
#' @param ... further arguments (currently ignored)
#'
#' @return a vector of selected model indices
#' @export
select_best <- function(comparison,
                        mode = c("weighted", "min"),
                        threshold = c(0.5, 0.5),
                        regu=1,
                        break_ties = TRUE,
                        ...){
  mode <- match.arg(mode)

  w <- switch(comparison$hypothesis$target,
              1,
              accuracy = 1,
              sensspec = t2w(threshold))

  est <- lapply(comparison$comparison, function(x){(colSums(x)+regu)/(nrow(x)+2*regu)})
  stopifnot(length(threshold) == length(est))
  delta <- switch(mode,
                  weighted = apply(sapply(1:length(est), function(g) est[[g]]*w[g]), 1, sum),
                  min = apply(sapply(1:length(est), function(g) est[[g]]-threshold[g]), 1, min))
  opt <- switch(comparison$hypothesis$type,
                error=min(delta),
                performance=max(delta))
  best <- unname(which(delta == opt))
  if(break_ties){best <- sample(rep(best, 2), 1)}
  return(best)
}

#' Select prediction models in an optimal fashion for the evaluation study
#'
#' @param comparison SEPM.comparison
#' @param method_select "few" or "max"
#' @param method_order "param" or "sample"
#' @param target_order character, see \code{SIMPle::order_dist}
#' @param combine_order character, see \code{SIMPle::order_dist}
#' @param max_models integer, maximum number of models
#' @param n_eval evaluation sample size
#' @param prev_eval evaluation prevalence
#' @param n_val validation sample size
#' @param prev_val validation prevalence
#' @param rdm logical, simple random sampling (TRUE) or case-control sampling (FALSE)
#' @param threshold numeric, length identical to number of groups
#' @param batch_size integer
#' @param max_iter integer
#' @param target_tol numeric, tolerance parameter
#' @param method_ext "basic" or "cov"
#' @param method_pred "mbeta_approx" or "rmvbin"
#' @param return_simvals logical
#' @param steady_plot logical
#' @param save_plot logical
#' @param info_plot logical
#' @param ylim_plot numeric, length 2
#' @param ... further arguments (currently ignored)
#'
#' @return a vector of selected model indices
#' @export
select_optimal <- function(comparison,
                           method_select = "few",
                           method_order = "param",
                           target_order = "prob",
                           combine_order = "min",
                           max_models = Inf,
                           n_eval = 100,
                           prev_eval = 0.5,
                           n_val = NA,
                           prev_val = NA,
                           rdm = TRUE,
                           threshold = c(0.75, 0.75),
                           batch_size = 10,
                           max_iter = 50,
                           target_tol = 1e-4,
                           method_ext = "basic",
                           method_pred = "mbeta_approx",
                           return_simvals=FALSE,
                           steady_plot=FALSE,
                           save_plot=FALSE,
                           info_plot=FALSE,
                           ylim_plot=NULL,
                           ...){

  ## prep:
  comp <- comparison$comparison
  G <- length(comp)
  m <- ncol(comp[[1]])

  max_models <- min(m, max_models)

  ## calc posterior:
  dist <-
    SIMPle::define_dist(nu=2, mean=0.5, corr=0, mode="reduced",
                        vars = m, groups = G) %>%
    SIMPle::update_dist(data=comp)

  ## reduce dist:
  ord <- SIMPle::order_dist(dist,
                            method = method_order,
                            target = target_order,
                            combine = combine_order,
                            threshold = threshold,
                            size = calc_size(G, n_eval, prev_eval, n_val, prev_val, rdm),
                            ties.method="random")[1:max_models]
  rdist <- SIMPle::select_vars(dist, selection=ord)

  ## optimize efp:
  opt <- optimize_efp(rdist,
                      n_eval,
                      prev_eval,
                      n_val,
                      prev_val,
                      rdm,
                      threshold,
                      batch_size,
                      max_iter,
                      target_tol,
                      return_simvals,
                      method_ext,
                      method_pred,
                      steady_plot,
                      save_plot,
                      info_plot,
                      ylim_plot)

  result <- ord[1:opt[[method_select]]$sel]
  return(result)
}

