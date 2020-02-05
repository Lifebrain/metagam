#' Plot estimated smooth terms
#'
#' Plot the meta-analytic estimate of a smooth term along with the separate fits in each cohort.
#'
#' @param x Object returned by \code{\link{metagam}}.
#' @param ... Other arguments to plot.
#'
#' @return A ggplot object plotting a smooth term of interest along an axis. The meta-analytic
#' fit is shown as a solid black line, and the cohort fits are shown as dashed lines, separated by
#' color codes.
#'
#' @details This function currently works for meta-analytic estimates of a single smooth term, which can be either
#' univariate or bivariate. It alswo works for alternatively meta-analysis of response or link functions.
#'
#' @export
#'
#' @example /inst/examples/metagam_examples.R
#'
plot.metagam <- function(x, ...)
{
  if(length(x$terms) > 1){
    stop("plot.metagam currently only works for a single term.")
  }

  metadat <- if(x$type %in% c("iterms", "terms")){
    dplyr::filter(x$meta_estimates, .data$term == !!x$terms)
  } else {
    x$meta_estimates
  }

  dat <- if(x$type %in% c("iterms", "terms")){
    dplyr::filter(x$cohort_estimates, .data$term == !!x$terms)
  } else {
    x$cohort_estimates
  }


  if(length(x$xvars) == 1){
    gp <- plot_univariate_smooth(metadat, dat, x$xvars, x$type, x$terms)
  } else if(length(x$xvars) == 2){
    gp <- plot_bivariate_smooth(metadat, x$xvars, x$type, x$terms)
  } else {
    stop("plot.metagam currently only works for univariate or bivariate terms.")
  }


  return(gp)

}

plot_bivariate_smooth <- function(metadat, xvars, type, terms){

  var1 <- sym(xvars[[1]])
  var2 <- sym(xvars[[2]])
  ggplot2::ggplot(metadat, ggplot2::aes(x = !!var1, y = !!var2,
                                                 z = .data$estimate)) +
    ggplot2::geom_raster(ggplot2::aes(fill = .data$estimate)) +
    ggplot2::geom_contour() +
    ggplot2::labs(fill = if(type == "iterms") terms else type) +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_distiller(palette = "RdBu", type = "div")

}

plot_univariate_smooth <- function(metadat, dat, xvars, type, terms){

  var <- sym(xvars[[1]])
  gp <- ggplot2::ggplot(dat, ggplot2::aes(x = !!var, y = .data$estimate)) +
    ggplot2::geom_line(ggplot2::aes(group = .data$model, color = .data$model),
                       linetype = "dashed") +
    ggplot2::geom_line(data = metadat) +
    ggplot2::ylab(if(type == "iterms") terms else type) +
    ggplot2::theme_minimal() +
    ggplot2::labs(color = "Dataset")
}
