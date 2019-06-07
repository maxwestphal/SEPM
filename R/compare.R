#' Compare predictions and labels
#'
#' This function may be used to compare predictions and labels of multiple prediction models.
#'
#' @param hypothesis object of class SEPM.hypothesis, see ?define_hypothesis.
#' @param comparison numeric matrix containing n observations of (dis)similarities between
#' labels and predictions (rows) for S candidate models (columns). If objective$co.primary is TRUE,
#' comparison needs to be a (named) list of two such matrices.
#' @param predictions numeric matrix with n observations (rows) with predictions from S models
#' (columns).
#' @param labels vector of length n containing the true labels.
#' @param features matrix or data.frame with n observations (rows) of P features (columns).
#' @param models named list of prediction models, i.e. list of R objects for which
#' predict(object, newdata=features, ...) produces the predictions for the given features.
#' @param model.names optional character of length S specifyng model names. By default column
#' names of comparison or predictions or names(models) are used.
#' @param pos.label specify the positive label, only used if hypothesis$co.primary is TRUE. If the
#' labels are 0,1 (FALSE, TRUE), 1 (TRUE) is assumed to represent the positive class by default.
#' @param ... further arguments (e.g. type) passed to predict for compare mode (3).
#'
#' @details One of the following has to be specified: (1) comparison, (2) predictions and labels or
#' (3) labels and features and models. A message indicates which of these compare modes is used to
#' obtain the result. If the requirements for multiple compare modes are met, the result will be
#' calculated based on the priority (1) > (2) > (3). Compare mode (2) should be the default. Mode (1)
#' basically assumes the output has been computed beforehand which might be useful for simulations.
#'
#' @return SEPM.comparison object containing a matrix including the compared (predicted, actual)
#' pairs and the input hypothesis.
#'
#' @examples
#' y <- rep(1:0, times=c(3,7))
#' yhat <- cbind(model1 = rep(1:0, 5),
#'               model2 = rep(0, 10),
#'               model3 = rep(1:0, times=c(2,8)))
#' define_hypothesis("accuracy", threshold = 0.75) %>%
#' compare(predictions = yhat, labels = y)
#' @export
compare <- function(hypothesis,
                    comparison,
                    predictions,
                    labels,
                    features,
                    models,
                    model.names,
                    pos.label,
                    ...)
{
  if(!is.SEPM.hypothesis(hypothesis)){
    stop("hypothesis needs to be an SEPM.hypothesis object, see ?define_hypothesis!")
  }

  comparison <- derive.comparison(hypothesis, comparison, predictions, labels,
                                  features, models, model.names, pos.label, ...)

  out <- list(comparison = comparison,
              hypothesis = unclass(hypothesis))
  class(out) <- append(class(out), "SEPM.comparison")
  return(out)
}















