is.SEPM.target <- function(x)
{
  inherits(x, "SEPM.target")
}

is.SEPM.hypothesis <- function(x)
{
  inherits(x, "SEPM.hypothesis")
}

is.SEPM.transform <- function(x)
{
  inherits(x, "SEPM.transform")
}

is.SEPM.estimate.control <- function(x)
{
  inherits(x, "SEPM.estimate.control")
}

is.SEPM.select.control <- function(x)
{
  inherits(x, "SEPM.select.control")
}

is.SEPM.compairson <- function(x)
{
  inherits(x, "SEPM.comparison")
}

is.SEPM.estimation <- function(x)
{
  inherits(x, "SEPM.estimation")
}

is.SEPM.selection <- function(x)
{
  inherits(x, "SEPM.selection")
}

is.SEPM.evaluation <- function(x)
{
  inherits(x, "SEPM.evaluation")
}
