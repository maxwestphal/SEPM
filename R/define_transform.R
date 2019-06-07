#' Transform parameter estimates before conducting statistical inference
#'
#' Define a transform which shall be used for a statistical test, see \code{\link{infer}} or
#' \code{\link{evaluate}}.
#'
#' @param transform character, name of the transform.
#' @param link function, which maps the raw estimate to the transformed estimate.
#' @param inv function, inverse of link:  \code{inv = link^{-1}: y -> x}
#' @param se.t function, standard error of the transformed quantitiy (delta method). se.t needs
#' to have formal input arguments \code{se}, \code{n}, \code{estimate} (even if the result does
#' not depend on them).
#'
#' @return A SEPM.transform which may be passed to \code{\link{infer}} or \code{\link{evaluate}}.
#'
#' @details If transform corresponds to a known SEPM.transform object, this will object be returned
#' and all other input arguments are ignored. \code{define_transform()} will print a list of known
#' transforms.
#'
#' The use of a transform (other than "none") may help with making the test statistic(s) more
#' "gaussian". In addition, it can prevent confidence bounds outside the parameter space. This a
#' realistic issue in binomial models, e.g. when working with proportions like accuracy or
#' classification error (ce). In this case the implemented transforms "logit" and "asin.sqrt"
#' enforce confidence intervals in [0,1].
#'
#' The output of the function se.t can be computed analytically with help of the Delta method, see
#' \href{https://en.wikipedia.org/wiki/Delta_method}{Delta Method (Wikipedia)}.
#'
#' @export
define_transform <- function(transform,
                             link,
                             inv,
                             se.t)
{
  if(is.missnullna(transform)){
    message("SEPM: Specify a known transform or define a custom one, see ?define_transform.")
    cat("Known transformations:\n")
    print(names(SEPM.TRANSFORM))
    return(invisible(names(SEPM.TRANSFORM)))
  }
  if(is.SEPM.transform(transform)){
    return(transform)
  }
  if(!is.character(transform)){
    stop("transform needs to be a character!")
  }
  if(is.known.transform(transform)){
    message(paste0('SEPM: Recognized transform "', transform, '", using corresponding SEPM.transform.'))
    return(SEPM.TRANSFORM[[transform]])
  }
  message(paste0('SEPM: Unknown transform "', transform, '", constructing custom SEPM.transform.'))
  out <- construct_transform(transform=transform, link=link, inv=inv, se.t=se.t,
                             custom=TRUE, check.args=TRUE)
  return(out)
}



