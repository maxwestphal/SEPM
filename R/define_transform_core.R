### FUNCTION: construct_transform
construct_transform <- function(transform, link, inv, se.t,
                                custom = FALSE, check.args = FALSE)
{
  if(check.args){
    if(!is.function(link) | !is.function(inv) | !is.function(se.t)){
      stop("link, inv and se.t need to be appropriatly defined functions, see ?define_transform!")
    }
  }
  out <- list(transform = transform,
              link = link,
              inv = inv,
              se.t = se.t,
              custom = custom)
  class(out) <- append(class(out), "SEPM.transform")
  return(out)
}

### FUNCTION: is.known.target
is.known.transform <- function(transform){
  is.character(transform) & (transform %in% names(SEPM.TRANSFORM))[1]
}


### GLOBAL OBJECT: SEPM.TRANSFORM
SEPM.TRANSFORM <- list()

SEPM.TRANSFORM[["none"]] <- construct_transform(transform = "none",
                                                link = function(x){x},
                                                inv = function(y){y},
                                                se.t = function(se, n, estimate){se})

SEPM.TRANSFORM[["logit"]] <- construct_transform(transform = "logit",
                                                 link = function(x){log(x/(1-x))},
                                                 inv = function(y){1/(1+exp(-y))},
                                                 se.t = function(se, n, estimate){
                                                   sqrt(abs(1/(n*estimate*(1-estimate))))})

SEPM.TRANSFORM[["asin.sqrt"]] <- construct_transform(transform = "asin.sqrt",
                                                     link = function(x){asin(sqrt(x))},
                                                     inv = function(y){sin(y)^2},
                                                     se.t = function(se, n, estimate){
                                                       rep(sqrt(1/(4*n)), length(se))})
