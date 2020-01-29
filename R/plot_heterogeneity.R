#' Title
#'
#' @param x metagam object
#' @param axis Which axis
#' @param term Which term
#' @param ... Other arguments to plot.
#'
#' @return plot
#' @export
#'
#'
#'
plot_heterogeneity <- function(x, axis, term, ...)
{

  dat <- x$meta_estimates
  dat <- dplyr::filter(dat, .data$term == !!term)
  dat <- dplyr::mutate(dat, QEp = purrr::map_dbl(.data$meta_model, ~ .$QEp))
  dat <- dplyr::rename_at(dat, dplyr::vars(axis), ~ "x")

  gp <- ggplot2::ggplot(
    data = dat,
    ggplot2::aes(x = .data$x, y = .data$QEp)
    ) +
    ggplot2::geom_line() +
    ggplot2::scale_y_continuous(trans = 'log2') +
    ggplot2::geom_hline(yintercept = 0.05, lty = 2) +
    ggplot2::theme_minimal() +
    ggplot2::ylab("Heterogeneity (p)") +
    ggplot2::xlab(axis)

  return(gp)
}
