#' Dominance plot
#'
#' Plots the (relative) contribution of the individual GAMs to each data point
#' on a given axis. It shows whether and how parts of the axis are dominated
#' by certain individual GAMs.
#'
#' @param x Object returned by \code{\link{metagam}}.
#' @param axis Character specifying which variable to plot. Defaults to \code{NULL}; if \code{x} was
#' fitted with a single term, the explanatory variable corresponding to this term
#' is selected.
#' @param term Character specifying which smooth term to plot. Default to \code{NULL}; if \code{x}
#' was fitted with a single term, this one is taken.
#' @param relative Logical specifying whether to have relative or absolute scales.
#' Defaults to \code{TRUE}.
#' @param width Width of bars. Default to \code{NULL}, which means it is automatically
#' determined based on the minimum grid spacing in \code{x}.
#'
#' @return A ggplot object.
#'
#' @export
#'
#' @examples
#' # See the vignette, either at https://lifebrain.github.io/metagam/articles/articles/dominance.html
#' # or by typing the following in the console:
#' # vignette("Dominance")
#'
#'
plot_dominance <- function(x, axis = NULL, term = NULL, relative = TRUE,
                            width = NULL)
{

  if(is.null(axis)){
    axis <- x$xvars
  }

  if(is.null(term)){
    term <- x$terms
  }

  if(length(axis) > 1 || length(term) > 1){
    stop("plot_heterogeneity() currently only works for analyzing a single univariate term\n",
         "please run metagam() with type='iterms'.\n\n")
  }

  # position = fill gives percent stacked bar,
  # otherwise fo position = stacked
  if (isTRUE(relative)) {
    position = "fill"
  } else {
    position = "stacked"
  }



  dat <- dplyr::filter(x$cohort_estimates, .data$term == !!term)
  dat <- dplyr::rename_at(dat, dplyr::vars(axis), ~ "x")
  dat <- dplyr::mutate(dat, y = 1 / .data$se^2)

  if(is.null(width)){
    width <- min(abs(diff(dat[["x"]])))
  }

  gp <- ggplot2::ggplot(dat,
                        ggplot2::aes(x = .data$x, y = .data$y,
                                     fill = .data$model, width = width)) +
    ggplot2::geom_bar(position=position,stat="identity")+
    ggplot2::theme_minimal() +
    ggplot2::ylab("Relative Influence") +
    ggplot2::xlab(axis) +
    ggplot2::labs(fill = "Cohort")

  return(gp)

}
