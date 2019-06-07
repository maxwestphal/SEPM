#' Estimate model performances and covariance matrix.
#'
#' This function allows the estimation of model performances and their covariance by employing a
#' prespecified estimation method.
#'
#' @param comparison SEPM.comparison, see \code{\link{compare}}.
#' @param method cahracter, speciying known estimation method (default: "default"). Alternativly a
#' custom SEPM.control object may be spcified, see \code{\link{estimate_control}}.
#' @param ... further (named) arguments to control$mapping.
#' @param args named list of further arguments to control$mapping
#'
#' @details When no arguments are supplied other than comparison, maximum likelihood
#' (control = "default") is conducted. Call \code{\link{estimate_control}}() for more information regarding
#' estimation methods.
#'
#' @return An SEPM.comparison object which summarizes the estimates result.
#'
#' @examples
#' comp <- compare("accuracy", comparison=matrix(rbinom(30, 1, 0.8), 10))
#' estimate(comp)
#' @export
estimate <- function(comparison,
                     method = "default",
                     ...,
                     args = NULL)
{
  if(!inherits(comparison, "SEPM.comparison")){
    stop("comparison needs to be of class SEPM.comparison, see ?compare!")
  }
  control <- estimate_control(method)

  if(control$type == "probability" & comparison$hypothesis$task != "classification"){
    stop('type = "probability" only works for classificaton tasks!')
  }

  args <- c(args, list(...))
  control$args <- args

  a <- split_list(l=args, check.len = length(comparison$comparison),
                  convert = (!comparison$hypothesis$co.primary | length(args) == 0),
                  err.msg = "For co-primary endpoints, all additional arguments need be lists which
                             include the arguments for each subpopulation in the same order as given
                             in comparison (and have the same length as comparison)!")

  #result <- lapply(comparison$comparison, estimate_se, control=control, args=args)
  result <- lapply(1:length(comparison$comparison),
                   function(i){estimate_se(comparison$comparison[[i]],
                                           control = control, args = a[[i]])})
  names(result) <- names(comparison$comparison)

  out <- list(estimation = c(result, list(control = unclass(control))),
              hypothesis = comparison$hypothesis)
  class(out) <- append(class(out), "SEPM.estimation")
  return(out)
}




split_list <- function(l, check.len=length(l[[1]]), convert=F,
                       err.msg="All elements of list l should be lists with same length!"){
  if(convert){return(replicate(check.len, l, simplify = F))}
  if(any(!sapply(l, is.list))){stop(err.msg)}
  len <- sapply(l, length)
  if(any(len != check.len)){stop(err.msg)}
  return(lapply(1:len[1], function(i){lapply(l, '[[', i)}))
}






