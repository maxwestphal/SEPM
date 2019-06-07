#' @export
summary.SEPM.comparison <- function(x, ...){
  cat(paste0("##### SEPM.comparison regarding ", x$hypothesis$target, ifelse(x$hypothesis$custom, "(custom) ", ""), " for the following models:\n"))
  print(colnames(x$comparison$result))
  cat("Pass this object to estimate(...) for parameter estimation or directly to evaluate(...) for model evaluation.")
}

#' @export
summary.SEPM.estimation <- function(x, ...){
  cat(paste0("##### SEPM.estimation regarding ", x$hypothesis$target, ifelse(x$hypothesis$custom, "(custom) ", ""), " for the following models:\n"))
  print(colnames(estimation(x)$model.name))
  cat("Pass this object to evaluate(...) for model evaluation.")
}

#' @export
summary.SEPM.inference <- function(x, info=FALSE, control=FALSE, ...){
  cat(paste0("##### SEPM.inference regarding ", x$hypothesis$target, ifelse(x$hypothesis$custom, "(custom) ", ""), " with the following results:\n"))
  out <- result(x)
  print(out)
  if(info){
    out <- c(out, info(x))
    cat("### Further information:\n")
    str(view(info(x)[[1]]), 1)
  }
  if(control){
    out <- c(out, control(x))
    cat("\n")
    cat("### Control information:\n")
    str(view(control(x)[[1]]), 2)
  }
  return(invisible(out))
}

#' @export
summary.SEPM.selection <- function(x, control=FALSE, ...){
  cat(paste0("##### SEPM.selection regarding ", x$hypothesis$target, ifelse(x$hypothesis$custom, "(custom) ", ""), " with the following results:\n"))
  out <- result(x)
  str(out)
  if(control){
    out <- c(out, control(x))
    cat("\n")
    cat("### Control information:\n")
    str(view(control(x)[[1]]), 2)
  }
  return(invisible(out))
}

