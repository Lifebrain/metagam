#' Heterogeneity Plot
#'
#'
#' @param x Object returned by \code{\link{metagam}}.
#' @param axis Character specifying which variable to plot. Defaults to \code{NULL}; if \code{x} was
#' fitted with a single term, the explanatory variable corresponding to this term
#' is selected.
#' @param term Character specifying which smooth term to plot. Default to \code{NULL}; if \code{x}
#' was fitted with a single term, this one is taken.
#' @param type Character specifying which type of plot. Either \code{"Q"} for the test statistic
#' or \code{"p"} for the p-value. Defaults to \code{"Q"}.
#' @param alpha Significance level. Defaults to \code{.05}.
#' @param ... Other arguments to plot.
#'
#' @return A ggplot object.
#' @export
#'
#' @details This plot visualizes the heterogeneity along the given axis, using Cochrane's Q test.
#'
plot_heterogeneity <- function(x, axis = NULL, term = NULL, type = "Q", alpha = .05, ...)
{

  if(is.null(axis)){
    axis <- x$xvars
  }

  if(is.null(term)){
    term <- x$terms
  }

  dat <- x$meta_estimates
  dat <- dplyr::filter(dat, .data$term == !!term)
  dat <- dplyr::mutate(dat,
                       QE = purrr::map_dbl(.data$meta_model, ~ .$QE),
                       QEp = purrr::map_dbl(.data$meta_model, ~ .$QEp))
  dat <- dplyr::rename_at(dat, dplyr::vars(axis), ~ "x")


    if (type=="p") {

      gp <- ggplot2::ggplot(
        data = dat,
        ggplot2::aes(x = .data$x, y = .data$QEp)) +
        ggplot2::geom_line() +
        ggplot2::scale_y_continuous(trans = 'log2') +
        ggplot2::geom_hline(yintercept = !!alpha, lty = 2) +
        ggplot2::theme_minimal() +
        ggplot2::ylab("Heterogeneity (p)") +
        ggplot2::xlab(axis)

  } else if (type=="Q") {

    # TODO: øystein, please check whether this is the correct approximation

    dat <- dplyr::mutate(dat,
                  z = -0.862 + sqrt(0.743 - 2.404 * log(.data$QEp)),
                  Qse = .data$QE / .data$z)

  gp <- ggplot2::ggplot(
    data=dat,
    ggplot2::aes(x= .data$x, y=.data$QE)
    ) +
    ggplot2::geom_ribbon(
      mapping = ggplot2::aes(ymin = .data$QE + stats::qnorm(!!alpha / 2) * .data$Qse,
                             ymax = .data$QE + stats::qnorm(1 - !!alpha / 2) * .data$Qse),
      fill = viridis::viridis(4)[3],
      col = viridis::viridis(4)[1]
      ) +
    ggplot2::geom_line() +
    ggplot2::theme_minimal() +
    ggplot2::ylab("Heterogeneity (Q)") +
    ggplot2::xlab(axis)

  } else {
    stop("Unknown type. Try 'Q' or 'p'.")
  }

  return(gp)
}
