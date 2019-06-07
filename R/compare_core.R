### FUNCTION: miss2def
miss2def <- function(x, default=NULL)
{
  if(missing(x)){return(default)}
  return(x)
}

### FUNCTION: get.dim
get.dim <- function(object, axis=1)
{
  if(is.array(object)){
    return(dim(object)[axis])
  }
  return(length(object))
}

### FUNCTION: tolist
tolist <- function(x)
{
  if(is.list(x)){
    return(x)
  }else{
    return(list(x))
  }
}

### FUNCTION: is.numeric.matrix
is.numeric.matrix <- function(x)
{
  all(!is.na(x)) & !is.null(x) & is.matrix(x) & is.numeric(x)
}

### FUNCTION: derive.comparison
derive.comparison <- function(hypothesis, comparison, predictions, labels,
                              features, models, model.names, pos.label=NULL, ...)
{
  comparison <- miss2def(comparison)
  predictions <- miss2def(predictions)
  labels <- miss2def(labels)
  features <- miss2def(features)
  models <- miss2def(models)
  model.names <- miss2def(model.names)
  pos.label <- miss2def(pos.label)
  args <- as.list(environment())
  e <- lapply(args, function(x)!is.null(x))
  m <- c(e$comparison, e$predictions & e$labels, e$labels & e$features & e$models)
  if(!any(m)){
    stop("Either (1) comparison, (2) predictions and labels or (3) labels and features and models
          need to be specified!")
  }
  mn <- get.model.names(args)
  args <- set.model.names(args, mn)
  if(!is.null(hypothesis$comparator)){
    check.comp(hypothesis$comparator, mn)
  }
  copr <- args$hypothesis$co.primary
  meas <- args$hypothesis$measure
  mode <- min(which(m))
  if(mode==1){
    message("SEPM: compare mode (1), deriving comparison based on input argument(s): comparison.")
    out <- derive.comparison.1(comparison = args$comparison,
                               co.primary = copr)
  }
  if(mode==2){
    message("SEPM: compare mode (2), deriving comparison based on input argument(s): predictions, labels.")
    out <- derive.comparison.2(predictions = args$predictions,
                               labels = args$labels,
                               measure = meas,
                               co.primary = copr,
                               pos.label)
  }
  if(mode==3){
    message("SEPM: compare mode (3), deriving comparison based on input argument(s): labels, features, models.")
    out <- derive.comparison.3(labels = args$labels,
                               features = args$features,
                               models = args$models,
                               measure = meas,
                               co.primary = copr,
                               pos.label = pos.label,
                               ...)
  }
  return(out)
}

### FUNCTION: derive.comparison.1
derive.comparison.1 <- function(comparison,
                                co.primary)
{
  if(!co.primary){
    if(!is.numeric.matrix(comparison)){
      stop("For single endpoints, comparison needs to be a numeric matrix!")
    }
    comparison <- list(result = comparison)
  }
  if(co.primary){
    if(!is.list(comparison) | length(comparison) != 2 |
       !is.numeric.matrix(comparison[[1]]) |
       !is.numeric.matrix(comparison[[2]]) |
       ncol(comparison[[1]]) != ncol(comparison[[2]])){
      stop("For co-primary endpoints, comparison needs to be a (named) list containing two numeric
            matrices with the same number of columns!")
    }
    lnames <- names(comparison)
    if(is.null(lnames)){lnames <- 1:0}
    names(comparison) = paste0("result.", lnames)
  }
  return(comparison)
}

### FUNCTION: derive.comparison.2
derive.comparison.2 <- function(predictions,
                                labels,
                                measure,
                                co.primary,
                                pos.label=NULL)
{
  labels <- as.matrix(labels)
  if(!is.matrix(predictions) | #typeof(predictions) != typeof(labels) |
     nrow(predictions) != get.dim(labels, axis=1) | ncol(labels) > 1){
    stop("predictions needs to be a matrix (n x S), labels needs to be a numeric of length n or a (n x 1) matrix!")
  }
  if(!co.primary){
    return(list(result=pred2comp(predictions, labels, measure)))
  }
  if(co.primary){
    u <- rev(sort(unique(labels)))
    if(length(u) != 2){
      stop("Co-primary endpoints analysis only compatible for binary classification, i.e. two unique labels.")
    }
    if(!is.null(pos.label)){
      if(! pos.label %in% u){
        stop("pos.label specified is not among the observed labels!")
      }
      if(! length(pos.label) == 1){
        stop("pos.label needs to have length 1!")
      }
      i <- pos.label == u
      u <- c(u[i], u[!i])
    }
    comp <- pred2comp(predictions, labels, measure)
    comparison <- list(comp[labels==u[1], ,drop=F], comp[labels==u[2], , drop=F])
    names(comparison) <- paste0("result.", u)
    return(comparison)
  }
}

### FUNCTION: derive.comparison.3
derive.comparison.3 <- function(labels,
                                features,
                                models,
                                measure,
                                co.primary,
                                pos.label,
                                ...)
{
  predictions <- mapply(model2pred, models, MoreArgs=append(list(newdata=features), list(...)), SIMPLIFY=TRUE)
  comparison <- derive.comparison.2(predictions, labels, measure, co.primary, pos.label)
  return(comparison)
}

### FUNCTION: pred2comp
pred2comp <- function(predictions,
                      labels,
                      measure){
  return(apply(predictions, 2, function(pred)
    apply(cbind(labels, pred), 1, function(lp) measure(actual=lp[1], predicted=lp[2]))))
}

### FUNCTION: model2pred
model2pred <- function(model, newdata, ...)
{
  pred <- predict(model, newdata=newdata, ...)
  return(pred)
}

### FUNCTION: get.model.names
get.model.names <- function(args)
{
  if(!is.null(args$models)){
    model.names <- names(args$models)
    S <- length(args$models)
  }
  if(!is.null(args$predictions)){
    model.names <- colnames(args$predictions)
    S <- ncol(args$predictions)
  }
  if(!is.null(args$comparison)){
    model.names <- colnames(tolist(args$comparison)[[1]])
    S <- ncol(tolist(args$comparison)[[1]])
  }
  if(!is.null(args$model.names)){
    if(S != args$model.names){
      stop("model.names has wrong lentgh!")
    }
    return(model.names)
  }
  if(is.null(model.names)){
    model.names <- paste0("model", 1:S)
  }
  return(model.names)
}

### FUNCTION: set.model.names
set.model.names <- function(args, model.names)
{
  if(!is.null(args$comparison)){
    if(is.list(args$comparison)){
      args$comparison <- lapply(args$comparison, function(x){
        colnames(x) <- model.names; return(x)})
    }
    if(is.matrix(args$comparison)){
      colnames(args$comparison) <- model.names
    }
  }
  if(!is.null(args$predictions)){
    colnames(args$predictions) <- model.names
  }
  if(!is.null(args$models)){
    names(args$models) <- model.names
  }
  return(args)
}

### FUNCTION: check.comp
check.comp <- function(c, mn){
  S <- length(mn)
  if(is.int(c)){
    if(c > S){
      stop(paste0("comparator specified as integer (",c, ") which exceeds the number of models (", S, ")!"))
    }
    name <- mn[c]
    return(list(i=c, n=name))
  }
  if(is.character(c)){
    index <- which(c == mn)
    if(length(index)==0){
      stop(paste0("comparator specified (", c , ") does not match any of the candidate model names: ", paste0(mn, collapse=", "), "!"))
    }
    return(list(i=index, n=c))
  }
  stop("comparator needs to be a character string or integer index specifying the reference model!")
}




