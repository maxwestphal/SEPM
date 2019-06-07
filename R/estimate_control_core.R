### FUNCTION: construct.estimate.control
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


### GLOBAL OBJECT: SEPM.ESTIMATE.CONTROL
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

SEPM.ESTIMATE.CONTROL[["known.cov"]] <-
  construct.estimate.control("known.cov", "generic",
                             function(comparison, cov){
                               list(theta.hat = colMeans(comparison),
                                    sigma.hat = cov)})

SEPM.ESTIMATE.CONTROL[["known.corr"]] <-
  construct.estimate.control("known.corr", "generic",
                             function(comparison, corr){
                               se <- sqrt(diag(cov(comparison)/nrow(comparison)));
                               list(theta.hat = colMeans(comparison),
                                    sigma.hat = diag(se, length(se)) %*% corr %*% diag(se, length(se)) )})

SEPM.ESTIMATE.CONTROL[["default.prob"]] <-
  construct.estimate.control("default.prob", "probability",
                             function(p, n){p})

SEPM.ESTIMATE.CONTROL[["logit.shrinkage"]] <-
  construct.estimate.control("logit.shrinkage", "probability", ### WORKING???
                             function(p, n, lambda=n^-2){
                               p <- as.matrix(p); M <- p; M[] <- (1-lambda)^2; diag(M) <- 1-lambda;
                               return(M*p)})

SEPM.ESTIMATE.CONTROL[["logit.shrinkage.rev"]] <-
  construct.estimate.control("logit.shrinkage.rev", "probability", ### TODO: TO BE REMOVED!!!
                             function(p, n){
                               p <- as.matrix(p); M <- p; M[] <- 1-(1/n^2); diag(M) <- diag(M)^2;
                               return(M*p)})

SEPM.ESTIMATE.CONTROL[["bayes.shrinkage"]] <-
  construct.estimate.control("bayes.shrinkage", "probability",
                             function(p, n, lambda=1){
                               k <- as.matrix(p)*n; M <- ncol(k);
                               k <- k + 0.5*lambda; diag(k) <- diag(k) + 0.5*lambda;
                               return(k/(n + 2*lambda))
                             })

SEPM.ESTIMATE.CONTROL[["beta.approx"]] <-   ###
  construct.estimate.control("beta.approx", "generic",
                             function(comparison, lambda=1, prior=NULL){
                               out <- list()
                               n <- nrow(comparison); d <- n+2*lambda+1 ### TODO: MODFIY D (TOTAL PRIOR OBS?!?); need prior and obs count
                               cp <- t(comparison) %*% comparison / n

                               k <- as.matrix(cp)*n
                               #if(is.null(prior)){
                                 # specify update to actual obs
                               #  prior <- NULL
                               #}
                               # stopifnot(dim(prior)==dim(cp))
                               k <- k + 0.5*lambda; diag(k) <- diag(k) + 0.5*lambda; # TODO: update by prior
                               cp <- k/(n + 2*lambda); mp <- diag(cp)

                               out$theta.hat <- mp
                               out$sigma.hat <- (cp - (mp %*% t(mp))) / d
                               return(out)
                             })





# SEPM.ESTIMATE.CONTROL[["bayes.shrinkage.2"]] <-                ### TODO: TO BE REMOVED!!!
#   construct.estimate.control("bayes.shrinkage.2", "probability",
#                              function(p, n){
#                                k <- as.matrix(p)*n; M <- ncol(k); s <- 2;
#                                k <- k + 0.25*s; diag(k) <- diag(k) + 0.25*s;
#                                return(k/(n+ s)) ###
#                              })
#
# SEPM.ESTIMATE.CONTROL[["bayes.shrinkage.2M"]] <-
#   construct.estimate.control("bayes.shrinkage.2M", "probability", ### TODO: TO BE REMOVED!!!
#                              function(p, n){
#                                k <- as.matrix(p)*n; M <- ncol(k); s <- 2*M;
#                                k <- k + 0.25*s; diag(k) <- diag(k) + 0.25*s;
#                                return(k/(n+ s)) ###
#                              })

# SEPM.ESTIMATE.CONTROL$train.shrinkage <- construct.estimate.control("train.shrinkage", "default",
#                                                           function(p, ...){},
#                                                           list())


