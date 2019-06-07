### FUNCTION: construct.select.control
construct.select.control <- function(method,
                                     mapping,
                                     custom = FALSE,
                                     check.args = FALSE){
  if(check.args){
    req.args <- c("estimate", "cov", "h")
    if(!all(req.args %in% names(formals(mapping)))){
      stop(paste0("mapping must be a function with the following formal arguments: ",
                  paste(req.args, collapse = ", "), "!"))
    }
    message(paste0("SEPM: Using custom ", type, " selection method: ", method, "."))
  }
  out <- list(method = method,
              mapping = mapping,
              custom = custom)

  class(out) <- append(class(out), "SEPM.select.control")
  return(out)
}

### GLOBAL OBJECT: SEPM.SELECT.CONTROL
SEPM.SELECT.CONTROL <- list()

SEPM.SELECT.CONTROL[["rank"]] <-
  construct.select.control("rank", function(estimate, cov, h, r=1, ties="random", delta=0, select.max=Inf){
    if(r > select.max){r <- select.max}
    stopifnot(length(delta) %in% c(1, length(estimate)))
    estimate <- estimate + delta
    rank(switch(h$type, error = estimate, performance = -estimate), ties.method=ties) <= r})

SEPM.SELECT.CONTROL[["close"]] <-
  construct.select.control("close", function(estimate, cov, h, dist=0.01, delta=0, select.max=Inf){
    stopifnot(length(delta) %in% c(1, length(estimate)))
    estimate <- estimate + delta
    opt <- switch(h$type, error=which.min(estimate), performance=which.max(estimate));
    abs(estimate - estimate[opt]) <= dist})

SEPM.SELECT.CONTROL[["se"]] <-
  construct.select.control("se", function(estimate, cov, h, c=1, delta=0, select.max=Inf){
    stopifnot(length(delta) %in% c(1, length(estimate)))
    estimate <- estimate + delta
    opt <- switch(h$type, error=which.min(estimate), performance=which.max(estimate));
    sel <- abs(estimate - estimate[opt]) <= c*sqrt(diag(cov))[opt];
    sel & rank(switch(h$type, error = estimate, performance = -estimate), ties.method="random") <= select.max})

SEPM.SELECT.CONTROL[["test"]] <- ### TODO: TAKES WAY TOO LONG!
  construct.select.control("test", function(estimate, cov, h, alpha=0.5, delta=0, select.max=Inf){
    stopifnot(length(delta) %in% c(1, length(estimate)))
    opt <- switch(h$type, error=which.min(estimate), performance=which.max(estimate));
    sh=h; sh$threshold=NULL; sh$comparator=paste0("model", opt); sh$delta=0;
    sh$alternative <- switch(h$type, error="less", performance="greater"); sh$alpha=alpha;
    est=list(estimation=list(result=list(model.name = paste0("model", 1:length(estimate)),
                                         theta.hat=estimate+delta, sigma.hat=cov),
                             control=list()), hypothesis=sh);
    class(est) <- append(class(est), "SEPM.estimation");
    out <- rep(TRUE, length(estimate)); out[-opt] <- infer(est)$inference$result$reject; return(out)
  })

SEPM.SELECT.CONTROL[["user"]] <-
  construct.select.control("user", function(estimate, cov, h, s=1, delta=0, select.max=Inf){
    1:length(estimate) %in% s})


