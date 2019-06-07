#' Guidance for model evaluation study.
#'
#' Use guide(x) or x \%>\% guide() for any SEPM object x (SEPM.hypothesis,
#' SEPM.estimate, ...) to obtain hints which function to call next for your model evaluation study.
#' Get started by calling guide().
#'
#' @param x an arbitrary R object.
#'
#' @return No return value, only a message is printed to guide the user.
#'
#' @export
#'
#' @examples
#' guide()
guide <- function(x){
  if(is.missnullna(x)){
    message("SEPM: The first step of any model evaluation study is the definition of a hypothesis (system), see ?define_hypothesis.")

    return(invisible())
  }
  cl <- c("SEPM.target", "SEPM.hypothesis", "SEPM.comparison", "SEPM.estimation", "SEPM.evaluation",
          "SEPM.estimate.control", "SEPM.transform")
  funs <- c("define_target", "define_hypothesis", "compare", "estimate", "test",
            "estimate_control", "define_transform")
  nextstep <- c("define a hyptothesis", "compare predictions and labels", "estimate model parameters",
                "conduct statistical inference")
  i <- which(inherits(x, cl, which=TRUE) > 1)
  if(length(i)==0){
    message("SEPM: Input is not a SEPM object. Start a new evaluation study by defining a hypothesis (system), see?define_hypothesis.")
    return(invisible(help("define_hypothesis", "SEPM")))
  }
  xx <- as.character(substitute(x))
  if(i %in% c(1:4, 6:7)){
    cc <- ifelse(xx != ".",
                 paste0("SEPM: You created the ", cl[i], " object ", xx, " with ", funs[i], "." ),
                 paste0("SEPM: You created an ", cl[i], " object with ", funs[i], "." ))
    if(i %in% 1:4){
      cc <- paste0(cc, paste0(" The next step in the evaluation pipeline is to ", nextstep[i],
                             ", see ?", funs[i+1], "."))
    }
    if(i==2){
      cc <- paste0(cc, " Alternativly, you can also use evaluate as a wrapper for compare, estimate and test. See ?evaluate for details.")
    }
    if(i %in% 6:7){
      cc <- paste0(cc, " You may pass this object to ", ifelse(i==6, "estimate()", "test() or evaluate()"),
                  ", see ", ifelse(i==6, "?estimate", "?test or ?evaluate"), ".")
    }
  }
  if(i == 5){
    cc <- "SEPM: All steps of the evaluation pipeline have been completed. You can summarize the
    results via summary() or plot a basic overview with plot(). Also, report() might be useful
    to prepare a report of your results, see ?report."
  }
  message(cc)
  return(invisible())
}
