#' Statistical Evaluation of Prediction Models (SEPM)
#'
#' Control the estimation process.
#'
#' @param method character, name of the estimation method
#' @param type either \code{"generic"} (default) or \code{"probability"}, the latter should only be
#' used for classification tasks
#' @param mapping a function which specifies how the estimation is conducted, see details.
#'
#' @return A SEPM.estimate.control object which is passed to \code{\link{estimate}}.
#'
#' @details If \code{type="generic"}, mapping needs to be a function with input \code{comparison}
#' and output \code{list(theta.hat = ..., sigma.hat = ...)} which is computed based on the input
#' comparison matrix. If \code{type="probability"}, mapping needs to to a function with input
#' argument \code{p} (a probaility (matrix)) and output a modified (e.g. shrunk) probability
#' with the same dimensions as \code{p}.
#'
#' @export
estimate_control <- function(method,
                             type = c("generic", "probability"),
                             mapping){
  if(is.missnullna(method)){
    cat("Known estimation methods:\n")
    print(names(SEPM.ESTIMATE.CONTROL))
    return(invisible(names(SEPM.ESTIMATE.CONTROL)))
  }
  if(is.SEPM.estimate.control(method)){
    return(method)
  }
  if(!is.character(method)){
    stop("method needs to be a character string!")
  }
  if(method %in% names(SEPM.ESTIMATE.CONTROL)){
    message(paste0("SEPM: Using known estimation method: ", method, "."))
    return(SEPM.ESTIMATE.CONTROL[[method]])
  }
  out <- construct.estimate.control(method = method, type=type, mapping=mapping,
                                    custom=TRUE, check.args=TRUE)
  message(paste0("SEPM: Using known ", out$type, " estimation method: ", method, "."))
  return(out)
}
