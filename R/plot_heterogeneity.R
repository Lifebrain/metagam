#' Heterogeneity Plot
#'
#'
#' @param x Object returned by \code{\link{metagam}}.
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
plot_heterogeneity <- function(x, term = NULL, type = "Q", alpha_thresh = .05)
{

  term <- find_plot_term(x, term)
  xvar <- x$term_list[[term]]$xvars

  type <- match.arg(type, c("p", "Q"))

  dat <- make_heterogeneity_data(x, term = term, xvar = xvar)

  gp <- switch(type,
         "p" = plot_heterogeneity_p(dat, xvar, alpha_thresh),
         "Q" = plot_heterogeneity_q(dat, xvar, alpha_thresh)
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
#' @return data.frame
#'
#' @keywords internal
make_heterogeneity_data <- function(x, term, xvar)
{
  if(x$type == "iterms"){
    dat <- x$meta_models[[term]]$predictions
    mods <- x$meta_models[[term]]$meta_models
  } else {
    dat <- x$meta_models$predictions
    mods <- x$meta_models$meta_models
  }


  dat$QE <- unlist(lapply(mods, function(x) x$QE))
  dat$QEp <- unlist(lapply(mods, function(x) x$QEp))

  names(dat)[names(dat) == xvar] <- "x"

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
plot_heterogeneity_p <- function(data, xvar, alpha_thresh){
  ggplot2::ggplot(data = data,
                  ggplot2::aes_(x =~ x, y =~ QEp)) +
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = alpha_thresh, lty = 2) +
    ggplot2::scale_y_continuous(trans = 'log2') +
    ggplot2::theme_minimal() +
    ggplot2::labs(y = "Heterogeneity (p)", x = xvar)
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
plot_heterogeneity_q <- function(data, xvar, alpha_thresh){


  data$z <- -0.862 + sqrt(0.743 - 2.404 * log(data$QEp))
  data$Qse <- data$QE / data$z
  data$ymin <- data$QE + stats::qnorm(alpha_thresh / 2) * data$Qse
  data$ymax <- data$QE + stats::qnorm(1 - alpha_thresh / 2) * data$Qse

  ggplot2::ggplot(data = data,
                  ggplot2::aes_(x =~ x, y =~ QE)) +
    ggplot2::geom_ribbon(mapping = ggplot2::aes_(ymin =~ ymin, ymax =~ ymax), alpha = .3) +
    ggplot2::geom_line() +
    ggplot2::theme_minimal() +
    ggplot2::labs(y = "Heterogeneity (Q)", x = xvar)
}



find_plot_term <- function(x, term){
  if(is.null(term)){
    term <- names(x$term_list)[[1]]
  }
  if(length(x$term_list[[term]]$xvars) > 1){
    stop("plot_heterogeneity() currently only works for analyzing a single univariate term.")
  }
  term
}
