### FUNCTION: is.int
is.int <- function(x){
  if(!is.numeric(x)){
    return(FALSE)
  }
  x %% 1 == 0
}

### FUNCTION: check.args.define_hypothesis
construct_hypothesis <- function(threshold, comparator, delta, alternative, alpha, co.primary){
  if(missing(threshold)){
    threshold <- NULL
  }
  if(missing(comparator)){
    comparator <- NULL
  }
  n <- is.null(threshold) + is.null(comparator)
  if(n == 0){
    stop("Only threshold or comparator can be specified, not both!")
  }
  if(n == 2){
    stop("You need to specify either threshold as a performance/error level (numeric) or comparator as a model name (character) or model index (integer)!")
  }
  if(!is.null(threshold) & !is.numeric(threshold)){
    stop("threshold needs to be numeric!")
  }
  if(is.numeric(threshold)){
    if(co.primary & length(threshold)!=2){
      stop("threshold needs to have length 2 for co-primary endpoints!")
    }
  }
  if(!co.primary & length(delta) != 1){
    stop("delta needs to have length 1 for single endpoints!")
  }
  if(co.primary & is.numeric(threshold) & length(delta) != length(threshold)){
    if(length(delta) == 1){
      delta <- rep(delta, length(threshold))
    }else{
      stop("delta should have same length as threshold!")
    }
  }
  if(!is.null(comparator) & !is.int(comparator) & !is.character(comparator)){
    stop("comparator needs to be an integer (model index) or character (model name)!")
  }
  if(!is.numeric(delta)){
    stop("delta needs to be numeric")
  }
  if(!alternative %in% c("two.sided", "greater", "less")){
    stop("alternative needs to be either 'two.sided', 'greater' or 'less'!")
  }
  if(!is.numeric(alpha) | alpha <= 0 | alpha > 0.5){
    stop("alpha needs to be between 0 and 0.5!")
  }
  out <- list(threshold = threshold,
              comparator = comparator,
              delta = delta,
              alternative = alternative,
              alpha = alpha)
  return(out)
}
