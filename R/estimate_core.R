### FUNCTION: estimate_se
estimate_se <- function(comparison,
                        control,
                        args){

  n <- nrow(comparison)
  est <- do.call(paste0("estimate_", control$type),
                 list(comparison=comparison, ec=control, args=args))
  out <- c(list(model.name = colnames(comparison)), est, list(n=n))
  return(out)
}

### FUNCTION: estimate_generic
estimate_generic <- function(comparison, ec, args){
  out <- list()
  result <- do.call(ec$mapping, c(list(comparison=comparison), args))
  out$theta.hat <- result$theta.hat
  out$theta.bar <- colMeans(comparison)
  out$sigma.hat <- result$sigma.hat
  return(out)
}

### FUNCTION: estimate_probability
estimate_probability <- function(comparison, ec, args){
  out <- list()
  n <- nrow(comparison); d <- n-1
  cp <- t(comparison) %*% comparison / n
  cp <- do.call(ec$mapping, c(list(p=cp, n=n), args) )
  mp <- diag(cp)
  out$theta.hat <- mp
  out$theta.bar <- colMeans(comparison)
  out$sigma.hat <- (cp - (mp %*% t(mp))) / d
  return(out)
}







