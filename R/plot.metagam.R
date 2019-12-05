#' Title
#'
#' @param x object of class \code{metagam}
#' @param ...
#'
#' @return
#' @export
#'
plot.metagam <- function(x, ...){
  df <- x$posterior_quantiles

  plot(value ~ x2, data = df, subset = quantity == "mean" & f == 1 & x1 == 0)
}
