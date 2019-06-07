### FUNCTION: check.args.select
check.args.select <- function(weights, n.test, h){
  if(!is.numeric(weights)){
    stop("weights need to be numeric!")
  }
  if(h$co.primary){
    if(length(weights)>2){stop("For co-primary endpoints, weights should have length 2.")}
  }
  if(!is.na(n.test)){
    if(!is.int(n.test)){
      stop("n.test need to be an integer!")
    }
    if(h$co.primary){
      if(length(n.test)>2){stop("For co-primary endpoints, n.test should have length 2.")}
    }
  }
  return(invisible())
}

### FUNCTION: estimate_power
estimate_power <- function(s, e, h, n){
  n <- rep(n, length.out=length(e))
  h <- modify_hyp(h, e[[1]]$model.name)

  sel <- union(h$comparator, e[[1]]$model.name[s])
  s <- e[[1]]$model.name %in% sel
  if(!is.null(h$comparator) & sum(s)==1){return(0)}

  e <- modify_est(e, s, n.test=n)

  t <- SEPM.TRANSFORM[["none"]] ####### ALLOW OTHER TRANSFORMS??

  # Case (1): single endpoint
  if(!h$co.primary){
    pow <- test_se(e[[1]], h, t, power=T)
  }

  # Case (2): co-primary endpoints
  if(h$co.primary){
    pow <- test_cp(e, h, t, power=T)
  }

  return(pow)
}

### modify_hyp
modify_hyp <- function(h, model.names){
  if(is.numeric(h$comparator)){
    h$comparator <- model.names[h$comparator]
  }
  return(h)
}

### modify_est
modify_est <- function(e, s, n.test){
  lapply(1:length(e), function(i){
    if(is.null(e[[i]]$sigma.hat)){return(e[[i]])}
    y <- e[[i]]
    y$sigma.hat <- (y$sigma.hat * (y$n- 1) / (n.test[i] -1))[s, s]
    y$model.name <- y$model.name[s]
    y$theta.hat <- y$theta.hat[s]
    y$theta.bar <- y$theta.bar[s]
    y$n <- n.test[i]
    return(y)
  })
}


