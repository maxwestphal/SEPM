#' @importFrom utils str
#' @export
summary.SEPM.comparison <- function(object, ...){
  cat(paste0("##### SEPM.comparison regarding ", object$hypothesis$target,
             ifelse(object$hypothesis$custom, "(custom) ", ""), " for the following models:\n"))
  print(colnames(object$comparison$result))
  cat("Pass this object to estimate(...) for parameter estimation or directly to evaluate(...)
      for model evaluation.")
}

#' @export
summary.SEPM.estimation <- function(object, ...){
  cat(paste0("##### SEPM.estimation regarding ", object$hypothesis$target,
             ifelse(object$hypothesis$custom, "(custom) ", ""), " for the following models:\n"))
  print(colnames(estimation(object)$model.name))
  cat("Pass this object to evaluate(...) for model evaluation.")
}

#' @export
summary.SEPM.inference <- function(object, info=FALSE, control=FALSE, ...){
  cat(paste0("##### SEPM.inference regarding ", object$hypothesis$target,
             ifelse(object$hypothesis$custom, "(custom) ", ""), " with the following results:\n"))
  out <- result(object)
  print(out)
  if(info){
    out <- c(out, info(object))
    cat("### Further information:\n")
    utils::str(view(info(object)[[1]]), 1)
  }
  if(control){
    out <- c(out, control(object))
    cat("\n")
    cat("### Control information:\n")
    utils::str(view(control(object)[[1]]), 2)
  }
  return(invisible(out))
}



