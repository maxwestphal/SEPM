### FUNCTION: test_se
#' @importFrom mvtnorm qmvnorm
#' @importFrom mvtnorm pmvnorm
test_se <- function(e, h, t, m="maxT", cv=NULL, preview=FALSE, power=FALSE)
{
  S <- length(e$theta.hat)
  dd <- diag(rep(1,S)); colnames(dd) <- e$model.name

  if(!is.null(h$threshold)){
    K <- Kh <- dd
    vs <- rhs <- h$threshold
  }
  if(!is.null(h$comparator)){
    co <- check.comp(h$comparator, e$model.name)
    K <- dd; K[,co$i] <- -1; K <- K[-co$i, TRUE, drop=F]
    Kh <- dd[-co$i, -co$i]
    rhs <- 0
    vs <- co$n
  }

  rhs <- t$link(rhs + ifelse(!is.null(h$delta[1]), h$delta[1], 0))

  est <-  as.numeric(K %*% e$theta.hat)
  sigma <- K %*% e$sigma.hat %*% t(K)
  se <- sqrt(diag(sigma))

  est.t <- do.call(t$link, list(est))
  se.t <- do.call(t$se.t, list(se=se, n=e$n, estimate=est))

  tstat <- (est.t - rhs)/se.t
  R <- cov2cor(sigma)

  if(preview){return(list(t=tstat, sigma=sigma, R=R))}

  sigma.t <- diag(se.t, length(se.t)) %*% R %*% diag(se.t, length(se.t))

  tail <- alt2tail(h$alternative)
  if(is.null(cv)){
    if(m=="maxT"){
      cv <- mvtnorm::qmvnorm(1-h$alpha, tail=tail,
                             mean=rep(0, length(est)), sigma=R)$quantile
    }
    if(m=="Bonferroni"){
      cv <- qnorm(1-(h$alpha/length(est)))
    }
    if(m=="univariate"){
      cv <- qnorm(1-h$alpha)
    }
    if(m=="naive"){
      cv <- 0
    }
  }
  if(power){return(calc_power(tstat, R, cv, h))}

  # bias correction (ONLY WORKING FOR SINGLE ENDPOINTS)
  cv50 <- mvtnorm::qmvnorm(0.5, tail=switch(h$type, performance="lower.tail", error="upper.tail"),
                           mean=rep(0, length(est)), sigma=R)$quantile

  CI.t <- data.frame(estimate = est.t,
                     corrected = est.t + switch(h$type, performance=-1, error=1)*cv50*se.t,
                     lower = est.t - cv*se.t,
                     upper = est.t + cv*se.t)
  CI <- as.data.frame(do.call(t$inv, list(CI.t)))
  if(h$alternative == "greater"){CI$upper <- rep(Inf,nrow(CI))}
  if(h$alternative == "less"){CI$lower <- rep(-Inf, nrow(CI))}

  result <- data.frame(model.name = colnames(Kh),
                       null.hypothesis = hypchar(h$alternative, vs, h$delta),
                       CI,
                       teststat = tstat,
                       pvalue = pvals(tstat, R, h$alternative))
  result$reject <- result$pval < h$alpha
  result$rank <- rank(-abs(result$teststat)) ### TODO: correct ranks for all alternatives =, >, <???
  rownames(result) <- NULL

  info <- list(alternative = h$alternative,
               alpha = h$alpha,
               alpha.adj = ifelse(h$alternative == "two.sided",2,1)*pnorm(cv, lower.tail=FALSE),
               c.alpha = cv,
               dependence = mod(cv, nrow(K), alpha=h$alpha, tail),
               rho.summary = summary(R[upper.tri(R)]))

  out <- list(result=result, info=info)

  return(out)
}



### FUNCTION: test.cp
#' @importFrom mvtnorm qmvnorm
test_cp <- function(e, h, t, m="maxT", power=FALSE)
{
  u <- length(e)

  hh <- split_hyp(h, len=u)
  t1 <- lapply(1:2, function(i) test_se(e[[i]], h=hh[[i]], t=t, cv=NULL, preview=TRUE))

  b <- apply(matrix(sapply(t1, function(x) abs(x$t)), ncol=2), 1, which.min)
  l <- b2l(b, len=length(t1))
  R <- as.matrix(as.matrix(Matrix::bdiag(lapply(t1, function(x)x$R)))[l,l])

  if(m != "maxT"){stop("Only maxT supported for co-primary endpoints!")}
  cv <- mvtnorm::qmvnorm(1-h$alpha, tail=alt2tail(h$alternative),
                         mean=rep(0, nrow(R)), sigma=R)$quantile

  if(power){
    #tstat <- apply(matrix(sapply(t1, function(x) x$t), ncol=2), 1, min)
    tstat <- matrix(sapply(t1, function(x) x$t), ncol=2)[matrix(l, ncol=2)]
    return(calc_power(tstat, R, cv, h))
  }

  t2 <- lapply(1:u, function(i) test_se(e[[i]], h=hh[[i]], t=t, m=m, cv=cv, preview=FALSE, power=FALSE))

  out <- unlist(t2, recursive=FALSE)[-seq(2, (2-1)*u , 2)]
  names(out)[1:u] <- names(e)[1:u]

  #print(names(e))
  out$info$rho.summary <- summary(R[upper.tri(R)])

  # cols <- 6:9
  # rpl <- do.call(rbind, t3[1:u])[l, cols]
  # t3[1:u] <- lapply(t3[1:u], function(x){x[,cols] <- rpl; x})
  # t3$rank <- rev(rank(t3$pvalue))

  return(out)
}

### FUNCTION: calc_power
calc_power <- function(tstat, R, cv, h){
  u <- rep(cv, length(tstat)); l <- -u
  if(h$alternative == "greater"){l <- rep(-Inf, length(tstat))}
  if(h$alternative == "less"){u <- rep(Inf, length(tstat))}
  pow <- 1 - as.numeric(mvtnorm::pmvnorm(lower=l, upper=u, mean=tstat, sigma=R))
  return(pow)
}

### FUNCTION: b2l
b2l <- function(b, len){
  rep(1:len, each=length(b)) == rep(b, len)
}

### FUNCTION: split_hyp
split_hyp <- function(h, len){
  co <- rep(h$threshold, len); d <- rep(h$delta, len)
  out <- lapply(1:2, function(i) {hi = h; hi$threshold = co[i]; hi$delta = d[i]; hi})
  return(out)
}

### FUNCTION: hypchar
hypchar <- function(alternative, vs, delta){
  altchar <- switch(which(alternative == c("two.sided", "greater", "less")), "==", "<=", ">=")
  d <- paste0("(", ifelse(delta >= 0, "+", ""), delta, ")")
  paste(altchar, vs, d)
}

### FUNCTION: alt2tail
alt2tail <- function(alternative){
  switch(which(alternative[1] ==  c('greater', 'less', 'two.sided')),
         "lower.tail", "upper.tail", "both.tails")
}

### FUNCTION: mod (measure of dependence)
mod <- function(calpha, dim=5, alpha=0.05, tail="both.tails"){
  #if(tail=="both.tails"){alpha <- alpha/2}
  cd <- qnorm(1-alpha)
  # ci <- qnorm((1-alpha)^(1/dim)) # faster but gives slighty different result compared to qmvnorm
  ci <- qmvnorm(1-alpha, tail=tail, sigma=diag(dim))$quantile
  round(1 - (calpha-cd)/(ci-cd), 6)
}

### FUNCTION: pvals
#' @importFrom mvtnorm pmvnorm
pvals <- function(t, R, a){  # TODO: wrong pvals for "univariate" and "naive" evaluation
  M <- length(t)
  if(a == "two.sided"){l <- -abs(t); u <- abs(t)}
  if(a == "greater"){l <- rep(-Inf, M); u <- t}
  if(a == "less"){l <- t; u <- rep(Inf, M)}
  1 - sapply(1:M, function(m) mvtnorm::pmvnorm(lower=rep(l[m], M), upper=rep(u[m], M), mean=rep(0, M), sigma=R))
}







