#' Heterogeneity Plot
#'
#'
#' @param x Object returned by \code{\link{metagam}}.
#' @param axis Character specifying which variable to plot. Defaults to \code{NULL}; if \code{x} was
#' fitted with a single term, the explanatory variable corresponding to this term
#' is selected.
#' @param term Character specifying which smooth term to plot. Defaults to \code{NULL}; if \code{x}
#' was fitted with a single term, this one is taken.
#' @param type Character specifying which type of plot. Either \code{"Q"} for the test statistic
#' or \code{"p"} for the p-value. Defaults to \code{"Q"}.
#' @param alpha_thresh Significance level. Defaults to \code{.05}.
#'
#' @return A ggplot object.
#' @export
#'
#' @details This plot visualizes the heterogeneity along the given axis, using Cochran's Q test.
#'
#' @examples
#' # See the vignette, either at https://lifebrain.github.io/metagam/articles/heterogeneity.html
#' # or by typing the following in the console:
#' # vignette("heterogeneity")
#'
#'
plot_heterogeneity <- function(x, axis = NULL, term = NULL,
                               type = "Q", alpha_thresh = .05)
{

  if(is.null(axis)){
    axis <- x$xvars
  }

  if(is.null(term)){
    if(x$type %in% c("iterms", "terms")){
      term <- x$terms
    } else {
      term <- x$type
    }
  }

  if(length(axis) > 1 || length(term) > 1){
    stop("plot_heterogeneity() currently only works for analyzing a single univariate term\n",
         "please run metagam() with type='iterms'.\n\n")
  }
  type <- match.arg(type, c("p", "Q"))

  dat <- make_heterogeneity_data(x, axis = axis, term = term)

  gp <- switch(type,
         "p" = plot_heterogeneity_p(dat, axis, alpha_thresh),
         "Q" = plot_heterogeneity_q(dat, axis, alpha_thresh)
         )

  return(gp)
}


# Internals ----
#' Prepare heterogeneity data
#'
#' Internal function to prepare
#' data for heterogeneity plots
#'
#' @inheritParams plot_heterogeneity
#'
#' @return tibble/data.frame
#'
#' @keywords internal
make_heterogeneity_data <- function(x, axis, term)
{
  dat <- x$meta_estimates
  mods <- x$meta_models
  dat <- dat[dat$term == term, ]
  dat$QE <- unlist(lapply(mods, function(x) x$QE))
  dat$QEp <- unlist(lapply(mods, function(x) x$QEp))

  names(dat)[names(dat) == axis] <- "x"

  return(dat)
}

#' Heterogeneity p-plot
#'
#' Internal function to plot
#' the heterogeneity p values
#'
#' @param data data made by \code{\link{make_heterogeneity_data}}.
#' @inheritParams plot_heterogeneity
#'
#' @return ggproto object
#'
#' @keywords internal
#'
plot_heterogeneity_p <- function(data, axis, alpha_thresh){
  ggplot2::ggplot(data = data,
                  ggplot2::aes(x = .data$x, y = .data$QEp)) +
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = alpha_thresh, lty = 2) +
    ggplot2::scale_y_continuous(trans = 'log2') +
    ggplot2::theme_minimal() +
    ggplot2::labs(y = "Heterogeneity (p)",
                  x = axis)
}

#' Heterogeneity Q-plot
#'
#' Internal function to plot
#' the heterogeneity Q values
#'
#' @param data data made by \code{\link{make_heterogeneity_data}}.
#' @inheritParams plot_heterogeneity
#'
#' @return ggproto object
#'
#' @keywords internal
#'
plot_heterogeneity_q <- function(data, axis, alpha_thresh){
  # TODO: øystein, please check whether this is the correct approximation

  data$z <- -0.862 + sqrt(0.743 - 2.404 * log(data$QEp))
  data$Qse <- data$QE / data$z

  ggplot2::ggplot(data = data,
                  ggplot2::aes(x = .data$x, y = .data$QE)) +
    ggplot2::geom_ribbon(
      mapping = ggplot2::aes(
        ymin = .data$QE + stats::qnorm(!!alpha_thresh / 2) * .data$Qse,
        ymax = .data$QE + stats::qnorm(1 - !!alpha_thresh / 2) * .data$Qse),
      alpha = .3
    ) +
    ggplot2::geom_line() +
    ggplot2::theme_minimal() +
    ggplot2::labs(y = "Heterogeneity (Q)",
                  x = axis)
}
