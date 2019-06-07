# learn <- WRAPPER FOR compare %>% estimate %>% select
#' @export
learn <- function(hypothesis,
                  ...,
                  estimate = "default",
                  estimate.args = NULL,
                  select = "rank",
                  select.args = NULL,
                  weights = 1,
                  n.test = NA,
                  seed = 1,
                  messages = TRUE){
  eval_expr(
    hypothesis %>%
      SEPM::compare(...) %>%
      SEPM::estimate(method = estimate, args = estimate.args) %>%
      SEPM::select(method = select, args=select.args, weights=weights, n.test=n.test),
    messages = messages, warnings = TRUE)

}
