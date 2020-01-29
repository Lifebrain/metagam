#' Domination plot
#'
#' Plots the (relative) contribution of the individual GAMs to each data point
#' on a given axis. It shows whether and how parts of the axis are dominated
#' by certain individual GAMs.
#'
#' @param x Object returned by \code{metagam}
#' @param axis Which variable to plot
#' @param term Which term to plot
#' @param relative Default to TRUE.
#' @param title Title of plot.
#' @param width Width of bars.
#'
#' @export
#'
plot_domination <- function(x, axis, term, relative=TRUE, title="Dominance Plot",
                            width = .01)
{

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

  # TODO: determine width parameter automatically
  gp <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$x, y = .data$y, fill = .data$model),
                        width = width)+
    ggplot2::geom_bar(position=position,stat="identity")+
    ggplot2::theme_minimal()+
    viridis::scale_fill_viridis(discrete = T) +
    ggplot2::ggtitle(title)+
    ggplot2::ylab("Relative Influence")

  return(gp)

}
