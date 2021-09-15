### FUNCTION: print.SEPM.estimate.control
#' @export
print.SEPM.estimate.control <- function(x){
  x <- rapply(x, function(x){attributes(x) <- NULL; x}, how="replace")
  cat("##### SEPM.estimate.control object with the following attributes: \n")
  str(x, 1)
}

### FUNCTION: print.SEPM.select.control
#' @export
print.SEPM.select.control <- function(x){
  x <- rapply(x, function(x){attributes(x) <- NULL; x}, how="replace")
  cat("##### SEPM.select.control object with the following attributes: \n")
  str(x, 1)
}

### FUNCTION: print.SEPM.transform
#' @export
print.SEPM.transform <- function(x){
  x <- rapply(x, function(x){attributes(x) <- NULL; x}, how="replace")
  cat("##### SEPM.transform object with the following attributes: \n")
  str(x, 1)
}

### FUNCTION: print.SEPM.target
#' @export
print.SEPM.target <- function(x){
  cat("##### SEPM.target with the following attributes: \n")
  str(x, 1)
}

### FUNCTION: print.SEPM.hypothesis
#' @export
print.SEPM.hypothesis <- function(x){
  cat("##### SEPM.hypothesis with the following attributes: \n")
  str(x, 1)
}

### FUNCTION: print.SEPM.comparison
#' @export
print.SEPM.comparison <- function(x){
  cat("##### SEPM.comparison with the following attributes: \n")
  str(x, 2)
}

### FUNCTION: print.SEPM.estimation
#' @export
print.SEPM.estimation <- function(x){
  cat("##### SEPM.estimation with the following attributes: \n")
  str(view(x))
}

### FUNCTION: print.SEPM.evaluation
#' @export
print.SEPM.inference <- function(x){
  cat("##### SEPM.evaluation with the following attributes: \n")
  str(view(x))
}

### FUNCTION: view
view <- function(x){
  rapply(x, function(x){attributes(x) <- NULL; x}, how="replace")
}
