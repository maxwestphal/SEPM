#' @export
dummy <- function(target="accuracy", n=c(100, 50), theta=seq(0.7, 0.9, 0.05), seed=c(1,2)){
  do.call(paste0("dummy_", target), (as.list(environment()))[-1])
}

### FUNCTION: dummy_accuarcy.cp
dummy_accuracy <- function(n=100, theta=seq(0.7, 0.9, 0.05), seed=1){
  set.seed(seed[1])
  sapply(theta, function(p) rbinom(n[1],1,p))
}

### FUNCTION: dummy_accuarcy.cp
dummy_accuracy.cp <- function(n=c(50, 100), theta=seq(0.7, 0.9, 0.05), seed=c(1,2)){
  list(Y1 = dummy_accuracy(n[1], theta, seed[1]),
       Y0 = dummy_accuracy(n[2], rev(theta), seed[2]))
}



