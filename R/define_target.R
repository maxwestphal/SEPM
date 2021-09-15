#' Define a target variable
#'
#' This function may be used to define a custom evaluation target. Many targets from the Metrics
#' package can be imported directly. Call define_target() for a list of known targets.
#'
#' @param target character, giving the name of the performance measure.
#' @param measure function with two arguments (actual, predicted) defining how (dis)similarity
#' between actual and predicted labels is measured. The return value of measure needs to be numeric,
#' even if the prediction is NA.
#' @param task character, either "classification" (default) or "regression"
#' @param type character, either "performance" (higher values indicate better performance, default)
#' or "error" (higher values indicate worse performance)
#' @param co.primary logical: should target be assessed in both subpopulations (only possible  for
#' binary  "classification" tasks)?
#'
#' @details If target is a recognized (e.g. "accuracy"), all other arguments are ignored.
#' If target is missing (default function call), a list of known targets is printed. The only
#' stored target with co.primary = TRUE, is called "sensspec" and corresponds to accessing
#' sensitivity and specificity of a classifier as co-primary endpoints.
#'
#' @return An object of class SEPM.objective which is used as the starting point of the further
#' evaluation pipeline. SEPM.objective contains elements target, measure, task and co.primary.
#' In addition, the logical custom indicates if the objective definition was user specific.
#'
#' @examples
#' define_target()
#' tar <- define_target("accuracy")
#' tar
#' @export
define_target <- function(target,
                          measure,
                          type = c("performance", "error"),
                          task = c("classification", "regression"),
                          co.primary = c(FALSE, TRUE)){
  ## Case (1): no target is specified:
  if(is.missnullna(target)){
    message("SEPM: Specify a known target or define a custom target, see ?define_target.")
    print.known.targets()
    return(invisible())
  }
  ## Case (2) input target is already SEPM.target
  if(is.SEPM.target(target)){
    return(target)
  }
  ## Case (3): known target is specified:
  if(is.known.target(target)){
    message(paste0('SEPM: Recognized target "', target, '", using corresponding SEPM.target.'))
    return(SEPM.TARGET[[target]])
  }
  ## Case (3): construct custom objective:
  message(paste0('SEPM: Unknown target "', target, '", constructing custom SEPM.target.'))
  task <- match.arg(task)
  type <- match.arg(type)
  co.primary <- co.primary[1]
  out <- construct_target(target = target,
                          measure = measure,
                          task = task,
                          type = type,
                          co.primary = co.primary,
                          custom = TRUE,
                          check.args = TRUE)
  return(out)
}
