hypothesis <- function(x){
  out <- x$hypothesis
  class(out) <- append(class(out), "SEPM.hypothesis")
  return(out)
}

estimation <- function(x, control=FALSE){
  out <- x$estimation
  return(out[1:(length(out)-ifelse(control, 0, 1))])
}

inference <- function(x, control=FALSE){
  out <- x$inference
  return(out[1:(length(out)-ifelse(control, 0, 1))])
}

model_names <- function(x){
  estimation(x)[[1]]$model.name
}

result <- function(x, lvl=1){
  x[[lvl]][grepl("result", names(x[[lvl]]))]
}

info <- function(x, lvl=1){
  x[[lvl]][grepl("info", names(x[[lvl]]))]
}

control <- function(x, lvl=1){
  x[[lvl]][grepl("control", names(x[[lvl]]))]
}

#' Extract features from a list
#'
#' @param x list or list-like object
#' @param what slot to extract from x
#'
#' @return
#' @export
get_feature <- function(x, what="model.name"){
  u <- unlist(unlist(x, F), F)
  u[grepl(what, names(u))][[1]]
}


