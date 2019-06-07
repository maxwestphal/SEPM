### FUNCTION: eval_expr
eval_expr <- function(expr, messages=T, warnings=T)
{
  if(messages & warnings)
    return(eval(expr))
  if(!messages & warnings)
    return(suppressMessages(eval(expr)))
  if(messages & !warnings)
    return(suppressWarnings(eval(expr)))
  if(!messages & !warnings)
    return(supressMessages(suppressWarnings(eval(expr))))
}
