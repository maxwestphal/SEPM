#' Statistical Evaluation of Prediction Models (SEPM)
#'
#' Control the selection process.
#'
#' @param method character, name of the selection method
#' @param mapping a function with formal input arguments \code{estimate} (vector of parameter
#' estimates), \code{cov} (covariance matrix of \code{estimate}) and \code{h}
#' (an \code{SEPM.hypothesis}. The output of mapping needs to be a logical with same length as
#' \code{estimate} where \code{TRUE} represents a selected and \code{FALSE} a non-selected model.
#'
#' @return A SEPM.select.control object which may be passed to \code{\link{select}}.
#'
#' @details mapping can have further input arguments which need to be specified via ... or args when
#' calling \code{\link{select}}. A call of \code{select_control()} prints all known selection methods.
#'
#' @examples
#' select_control()
#' select_control("rank")
#'
#' @export
select_control <- function(method,
                           mapping)
{
  if(is.missnullna(method)){
    cat("Known selection methods:\n")
    print(names(SEPM.SELECT.CONTROL))
    return(invisible())
  }
  if(is.SEPM.select.control(method)){
    return(method)
  }
  if(!is.character(method)){
    stop("method needs to be a character string!")
  }
  if(method %in% names(SEPM.SELECT.CONTROL)){
    message(paste0("SEPM: Using known selection method: ", method, "."))
    return(SEPM.SELECT.CONTROL[[method]])
  }
  out <- construct.select.control(method = method, mapping = mapping,
                                  custom=TRUE, check.args=TRUE)
  message(paste0("SEPM: Using known ", out$type, " selection method: ", method, "."))

  return(out)
}

