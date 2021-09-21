#' Generate (uncorrelated) dummy data
#'
#' @param target character, e.g. "accuarcy" or "sensspec"
#'
#' @param n integer, sample size(s)
#' @param theta numeric, scale parameter
#'
#' @return dummy data
#'
#' @details See \code{\link{dummy_accuracy}} or \code{\link{dummy_sensspec}}.
#'
#' @export
dummy <- function(target="accuracy",
                  n=c(100, 50),
                  theta=seq(0.7, 0.9, 0.05)){
  do.call(paste0("dummy_", target), (as.list(environment()))[-1])
}

#' Generate (uncorrelated) dummy data (target: accuracy)
#'
#' @param n integer, sample size (length 2: diseased healthy)
#' @param theta numeric, accuracy = theta
#'
#' @return dummy data (target: accuracy)
#' @export
dummy_accuracy <- function(n=100,
                           theta=seq(0.7, 0.9, 0.05)){
  sapply(theta, function(p) stats::rbinom(n[1],1,p))
}

#' Generate (uncorrelated) dummy data (target: sensspec)
#'
#' @param n integer, sample size (length 2: diseased healthy)
#' @param theta numeric, sens = theta; spec = rev(theta)
#'
#' @return dummy data (target: sensspec)
#' @export
dummy_sensspec <- function(n=c(50, 100),
                           theta=seq(0.7, 0.9, 0.05)){
  list(Y1 = dummy_accuracy(n[1], theta),
       Y0 = dummy_accuracy(n[2], rev(theta)))
}



