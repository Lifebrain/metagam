#' Dominance plot
#'
#' Plots the (relative) contribution of the individual GAMs to each data point
#' on a given axis. It shows whether and how parts of the axis are dominated by
#' certain individual GAMs.
#'
#' @param x Object returned by \code{\link{metagam}}.
#' @param term Character specifying which smooth term to plot. Default to
#'   \code{NULL} which means that the first term (in alphabetic order) is taken.
#' @param relative Logical specifying whether to have relative or absolute
#'   scales. Defaults to \code{TRUE}.
#' @param width Width of bars. Default to \code{NULL}, which means it is
#'   automatically determined based on the minimum grid spacing in \code{x}.
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
plot_dominance <- function(x, term = NULL, relative = TRUE, width = NULL)
{
  term <- find_plot_term(x, term)
  xvar <- x$term_list[[term]]$xvars

  dat <- do.call(rbind, lapply(seq_along(x$cohort_estimates), function(ind) {
    dd <- x$cohort_estimates[[ind]][[term]]
    dd$influence <- dd$se^(-2)
    dd$cohort <- ind
    dd <- dd[, c(xvar, "influence", "cohort")]
    names(dd)[names(dd) == xvar] <- "x"
    dd
    }))

  dat$cohort <- factor(dat$cohort)
  if(is.null(width)){
    width <- min(abs(diff(dat[["x"]])))
  }
  if(relative){
    position <- "fill"
  } else {
    position <- "stack"
  }

  ggplot2::ggplot(dat, ggplot2::aes_(x =~ x, y =~ influence, fill =~ cohort)) +
    ggplot2::geom_bar(position = position, stat = "identity", width = width) +
    ggplot2::theme_minimal() +
    ggplot2::ylab("Relative Influence") +
    ggplot2::xlab(xvar) +
    ggplot2::labs(fill = "Cohort")


}
