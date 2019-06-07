### FUNCTION: is.missnullna
is.missnullna <- function(x, cl=NULL){
  if(missing(x)){return(TRUE)}
  if(is.null(x)){return(TRUE)}
  if(any(is.na(x))){return(TRUE)}
  if(!is.null(cl)){
    return(ifelse(any(inherits(x, cl)), TRUE, FALSE))
  }
  return(FALSE)
}

### FUNCTION: sublist
sublist <- function(l, ..., check=NULL){
  d <- c(list(...), check); n <- names(d)
  if(length(d)==0){return(l)}
  l[sapply(l, function(e) all(sapply(1:length(d), function(i) identical(e[[n[i]]], d[[i]]) )) )]
}

### FUNCTION: print.known.targets
print.known.targets <- function(){
  cat("Known classification targets:\n ")
  print(names(sublist(SEPM.TARGET, task="classification")))
  cat("Known regression targets:\n ")
  print(names(sublist(SEPM.TARGET, task="regression")))
  return(invisible())
}

### FUNCTION: is.known.target
is.known.target <- function(target){
  is.character(target) & (target %in% names(SEPM.TARGET))[1]
}

### FUNCTION: construct_target
construct_target <- function(target = "accuracy",
                             measure = Metrics::accuracy,
                             task = "classification",
                             type = "performance",
                             co.primary = FALSE,
                             custom = FALSE,
                             check.args = FALSE){
  if(check.args){
    args <- as.list(environment())
    check.args.construct_target(args = args)
  }
  out <- list(target = target,
              measure = measure,
              task = task,
              type = type,
              co.primary = co.primary,
              custom = custom)
  class(out) <- append(class(out), "SEPM.target")
  return(out)
}

### FUNCTION: check.args.construct_target
check.args.construct_target <- function(args){
  if(!is.character(args$target)){
    stop("target needs to be a character!")
  }
  if(!is.function(args$measure) |
     !all(c("actual", "predicted") %in% names(formals(args$measure)))){
    stop("measure needs to be a function with formal arguments actual and predicted!")
  }
  if(is.na(args$task) | !args$task %in% c("classification", "regression")){
    stop("task needs to be either 'classification' or 'regression'!")
  }
  if(is.na(args$type) | !args$type %in% c("performance", "error")){
    stop("type needs to be either 'performance' or 'error'!")
  }
  if(!(is.logical(args$co.primary) & !is.na(args$co.primary))){
    stop("co.primary needs to be either TRUE or FALSE!")
  }
  if(args$co.primary & args$task == "regression"){
    stop("co.primary endpoints only implemented for (binary) classification tasks!")
  }
  return(invisible())
}

### GLOBAL OBJECT: SEPM.TARGET
SEPM.TARGET <- list()
SEPM.TARGET[["accuracy"]] <-
  construct_target()
SEPM.TARGET[["accuracy.cp"]] <-
  construct_target("accuracy.cp", Metrics::accuracy, "classification", "performance", TRUE)
SEPM.TARGET[["rmse"]] <-
  construct_target("rmse", Metrics::rmse, "regression", "error", FALSE)


