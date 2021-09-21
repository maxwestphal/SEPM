### FUNCTION: plot.SEPM.evaluation
#' @export
plot.SEPM.inference <- function(x, ...)
{
  S <- x$inference[grepl("result", names(x$inference))]
  len <- length(S)
  graphics::par(mfrow=c(1,len))
  invisible(lapply(1:len, function(i){
    d <- S[[i]];
    u <- ifelse(all(is.finite(d$upper)), max(d$upper), 1.25*max(d$estimate))
    l <- ifelse(all(is.finite(d$lower)), min(d$lower), 0.80*min(d$estimate))
    plot(1, type="n", xlab="Model", xlim=c(1,nrow(d)), ylim=c(l, u), ylab="Estimates & CI")
    graphics::points(estimate ~ model.name, data=d, pch=20, cex=1);
    if(all(is.finite(d$upper))){
      graphics::points(upper ~ model.name, data=d, col="red", pch="-", cex=4)
    }
    if(all(is.finite(d$lower))){
      graphics::points(lower ~ model.name, data=d, col="red", pch="-", cex=4)
    }
    graphics::title(main=names(S)[i])
  }))
}



