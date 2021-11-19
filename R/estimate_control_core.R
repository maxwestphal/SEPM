construct.estimate.control <- function(method,
                                       type = c("generic", "probability"),
                                       mapping,
                                       custom = FALSE,
                                       check.args = FALSE){
  if(check.args){
    type <- match.arg(type)
    req.args <- switch(type, generic="comparison", probability=c("p", "n"))
    if(!all(req.args %in% names(formals(mapping)))){
      stop(paste0("mapping must be a function with the following formal arguments: ",
                  paste(req.args, collapse = ", "), "!"))
    }
    message(paste0("SEPM: Using custom ", type, " estimation method: ", method, "."))
  }
  out <- list(method = method,
              type = type,
              mapping = mapping,
              custom = custom)
  class(out) <- append(class(out), "SEPM.estimate.control")
  return(out)
}

SEPM.ESTIMATE.CONTROL <- list()

SEPM.ESTIMATE.CONTROL[["default"]] <-
  construct.estimate.control("default", "generic",
                             function(comparison){
                               list(theta.hat = colMeans(comparison),
                                    sigma.hat = cov(comparison)/nrow(comparison))})

SEPM.ESTIMATE.CONTROL[["independence"]] <-
  construct.estimate.control("independence", "generic",
                             function(comparison){
                               v <- cov(comparison)/nrow(comparison);
                               list(theta.hat = colMeans(comparison),
                                    sigma.hat = diag(diag(v), nrow(v)))})

SEPM.ESTIMATE.CONTROL[["default.prob"]] <-
  construct.estimate.control("default.prob", "probability",
                             function(p, n){p})

SEPM.ESTIMATE.CONTROL[["bayes.shrinkage"]] <-
  construct.estimate.control("bayes.shrinkage", "probability",
                             function(p, n, lambda=1){
                               k <- as.matrix(p)*n; M <- ncol(k);
                               k <- k + 0.5*lambda; diag(k) <- diag(k) + 0.5*lambda;
                               return(k/(n + 2*lambda))
                             })

SEPM.ESTIMATE.CONTROL[["beta.approx"]] <-
  construct.estimate.control("beta.approx", "generic",
                             function(comparison, lambda=1){
                               out <- list()
                               n <- nrow(comparison); d <- n+2*lambda+1
                               cp <- t(comparison) %*% comparison / n

                               k <- as.matrix(cp)*n
                               k <- k + 0.5*lambda; diag(k) <- diag(k) + 0.5*lambda;

                               cp <- k/(n + 2*lambda); mp <- diag(cp)
                               out$theta.hat <- mp
                               out$sigma.hat <- (cp - (mp %*% t(mp))) / d
                               return(out)
                             })
