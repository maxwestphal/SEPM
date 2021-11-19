#' @importFrom mvtnorm qmvnorm
#' @importFrom mvtnorm pmvnorm
#' @importFrom stats cov2cor pnorm predict var
test_se <- function(e, h, t, m="maxT", cv=NULL, cv50=NULL, preview=FALSE)
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
  R <- stats::cov2cor(sigma)

  if(preview){return(list(t=tstat, sigma=sigma, R=R))}

  sigma.t <- diag(se.t, length(se.t)) %*% R %*% diag(se.t, length(se.t))

  tail <- alt2tail(h$alternative)

  if(is.null(cv)){
    cv <- switch(m,
                 maxT = mvtnorm::qmvnorm(1-h$alpha, tail=alt2tail(h$alternative),
                                         mean=rep(0, nrow(R)), sigma=R)$quantile,
                 Bonferroni = stats::qnorm(1-(h$alpha/S)),
                 univariate = stats::qnorm(1-h$alpha),
                 naive = 0)
  }
  if(is.null(cv50)){
    cv50 <- switch(m,
                   maxT = mvtnorm::qmvnorm(0.5, tail=switch(h$type, performance="lower.tail", error="upper.tail"),
                                           mean=rep(0, S), sigma=R)$quantile,
                   Bonferroni = stats::qnorm(1-(0.5/S)),
                   univariate = stats::qnorm(1-0.5),
                   naive = 0)
  }

  CI.t <- data.frame(estimate = est.t,
                     corrected = est.t + switch(h$type, performance=-1, error=1)*cv50*se.t,
                     lower = est.t - cv*se.t,
                     upper = est.t + cv*se.t)
  CI <- as.data.frame(do.call(t$inv, list(CI.t)))

  if(h$alternative == "greater"){CI$upper <- rep(Inf, nrow(CI))}
  if(h$alternative == "less"){CI$lower <- rep(-Inf, nrow(CI))}

  result <- data.frame(model.name = colnames(Kh),
                       null.hypothesis = hypchar(h$alternative, vs, h$delta),
                       CI,
                       teststat = tstat,
                       pvalue = pvals(tstat, R, h$alternative, m=m))
  result$reject <- result$pval < h$alpha
  result$rank <- switch(h$alternative,
                        two.sided = rank(-abs(result$teststat)),
                        greater = rank(-result$teststat),
                        less = rank(result$teststat))
  rownames(result) <- NULL

  info <- list(alternative = h$alternative,
               alpha = h$alpha,
               alpha.adj = ifelse(h$alternative == "two.sided",2,1)*stats::pnorm(cv, lower.tail=FALSE),
               c.alpha = cv,
               dependence = mod(cv, nrow(K), alpha=h$alpha, tail),
               rho.summary = summary(R[upper.tri(R)]))

  out <- list(result=result, info=info)

  return(out)
}


#' @importFrom mvtnorm qmvnorm
test_cp <- function(e, h, t, m="maxT")
{
  u <- length(e)
  S <- length(e[[1]]$theta.hat)
  hh <- split_hyp(h, len=u)
  t1 <- lapply(1:u, function(i) test_se(e[[i]], h=hh[[i]], t=t, m=m,
                                        cv=NULL, cv50=NULL, preview=TRUE))

  b <- apply(matrix(sapply(t1, function(x) x$t), ncol=2), 1, which.min)
  l <- b2l(b, len=length(t1))
  R <- as.matrix(as.matrix(Matrix::bdiag(lapply(t1, function(x)x$R)))[l,l])

  cv <- switch(m,
               maxT = mvtnorm::qmvnorm(1-h$alpha, tail=alt2tail(h$alternative),
                                       mean=rep(0, nrow(R)), sigma=R)$quantile,
               Bonferroni = stats::qnorm(1-(h$alpha/S)),
               univariate = stats::qnorm(1-h$alpha),
               naive = 0)

  cv50 <- switch(m,
                 maxT = mvtnorm::qmvnorm(0.5, tail=switch(h$type, performance="lower.tail", error="upper.tail"),
                                         mean=rep(0, S), sigma=R)$quantile,
                 Bonferroni = stats::qnorm(1-(0.5/S)),
                 univariate = stats::qnorm(1-0.5),
                 naive = 0)

  t2 <- lapply(1:u, function(i) test_se(e[[i]], h=hh[[i]], t=t, m=m, cv=cv, cv50=cv50))

  out <- unlist(t2, recursive=FALSE)[-seq(2, (2-1)*u , 2)]
  names(out)[1:u] <- names(e)[1:u]
  out$info$rho.summary <- summary(R[upper.tri(R)])

  return(out)
}

b2l <- function(b, len){
  rep(1:len, each=length(b)) == rep(b, len)
}

split_hyp <- function(h, len){
  co <- rep(h$threshold, len); d <- rep(h$delta, len)
  out <- lapply(1:2, function(i) {hi = h; hi$threshold = co[i]; hi$delta = d[i]; hi})
  return(out)
}

hypchar <- function(alternative, vs, delta){
  altchar <- switch(which(alternative == c("two.sided", "greater", "less")), "==", "<=", ">=")
  d <- paste0("(", ifelse(delta >= 0, "+", ""), delta, ")")
  paste(altchar, vs, d)
}

alt2tail <- function(alternative){
  switch(which(alternative[1] ==  c('greater', 'less', 'two.sided')),
         "lower.tail", "upper.tail", "both.tails")
}

mod <- function(calpha, dim=5, alpha=0.05, tail="both.tails"){
  cd <- stats::qnorm(1-alpha)
  ci <- mvtnorm::qmvnorm(1-alpha, tail=tail, sigma=diag(dim))$quantile
  round(1 - (calpha-cd)/(ci-cd), 6)
}

#' @importFrom mvtnorm pmvnorm
pvals <- function(t, R, a, m="maxT"){
  M <- length(t)
  if(a == "two.sided"){l <- -abs(t); u <- abs(t)}
  if(a == "greater"){l <- rep(-Inf, M); u <- t}
  if(a == "less"){l <- t; u <- rep(Inf, M)}
  if(m == "maxT"){
    p <- 1 - sapply(1:M, function(m) mvtnorm::pmvnorm(lower=rep(l[m], M), upper=rep(u[m], M), mean=rep(0, M), sigma=R))
  }
  if(m == "Bonferroni"){
    p <- M*(stats::pnorm(u, lower.tail=FALSE) + stats::pnorm(l, lower.tail=TRUE))
  }
  if(m == "univariate"){
    p <- (stats::pnorm(u, lower.tail=FALSE) + stats::pnorm(l, lower.tail=TRUE))
  }
  if(m == "naive"){
    p <- rep(1- 1*(l<0 | u>0))
  }
  return(p)
}







