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

is.SEPM.compairson <- function(x)
{
  inherits(x, "SEPM.comparison")
}

is.SEPM.estimation <- function(x)
{
  inherits(x, "SEPM.estimation")
}

is.SEPM.inference <- function(x)
{
  inherits(x, "SEPM.inference")
}



