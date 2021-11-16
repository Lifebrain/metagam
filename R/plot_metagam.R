#' Plot estimated smooth terms
#'
#' Plot the meta-analytic estimate of a smooth term along with the separate fits in each cohort.
#'
#' @param x Object returned by \code{\link{metagam}}.
#' @param term The smooth term to plot. Defaults to \code{NULL}, which means that the first term is plotted.
#' @param ... Other arguments to plot.
#'
#' @return A ggplot object plotting a smooth term of interest along an axis. The meta-analytic
#' fit is shown as a solid black line, and the cohort fits are shown as dashed lines, separated by
#' color codes.
#'
#' @details This function currently works for meta-analytic estimates of a single smooth term, which can be either
#' univariate or bivariate. It also works for alternatively meta-analysis of response or link functions.
#'
#' @export
#'
#' @example /inst/examples/metagam_examples.R
#'
plot.metagam <- function(x, term = NULL, ...)
{
  if(!is.null(term) && length(term) > 1){
    stop("plot.metagam currently only works for a single term.")
  }
  if(is.null(term)){
    term <- names(x$term_list)[[1]]
  }

  metadat <- if(x$type %in% c("iterms", "terms")){
    x$meta_models[[term]]$predictions
  } else {
    x$meta_models$predictions
  }

  dat <- lapply(seq_along(x$cohort_estimates), function(ind) {
      if(x$type %in% c("iterms", "terms")){
        x$cohort_estimates[[ind]][[term]]
      } else {
        x$cohort_estimates[[ind]]
      }
    })

  xvars <- x$term_list[[term]]$xvars
  if(length(xvars) == 1){
    plot_univariate_smooth(metadat, dat, xvars, x$type, term)
  } else if(length(xvars) == 2){
    gp <- plot_bivariate_smooth(metadat, x$xvars, x$type, x$terms)
  } else {
    stop("plot.metagam currently only works for univariate or bivariate terms.")
  }

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

plot_univariate_smooth <- function(metadat, dat, xvar, type, term){
  plot(metadat[[xvar]], metadat[["estimate"]], type = "l",
       xlab = xvar,
       ylab = ifelse(type == "iterms", term, type),
       xlim = range(metadat[[xvar]]),
       ylim = range(c(metadat[["estimate"]], unlist(lapply(dat, function(x) x[["fit"]])))))
  iter <- seq_along(dat)
  for(i in iter){
    lines(dat[[i]][[xvar]], dat[[i]][["fit"]], lty = 2, col = i + 1L)
  }
  legend("topright", legend = seq_along(dat), col = iter + 1L, lty = 2,
         title = "Dataset")

}
