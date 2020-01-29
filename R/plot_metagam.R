#' Plot estimated smooth terms
#'
#' Plot the meta-analytic estimate of a smooth term along with the separate fits in each cohort.
#'
#' @param x Object returned by \code{\link{metagam}}.
#' @param ... Other arguments to plot.
#'
#' @return A ggplot object.
#' @export
#'
plot.metagam <- function(x, ...)
{
  if(length(x$terms) > 1){
    stop("plot.metagam currently only works for a single term.")
  }

  if(length(x$xvars) > 1){
    stop("plot.metagam currently only works for univariate terms.")
  }

  prepare_df <- function(df){
    df <- dplyr::rename_at(df, dplyr::vars(!!x$xvars), ~ "x")
    dplyr::filter(df, .data$term == !!x$terms)
  }


  dat <- prepare_df(x$cohort_estimates)
  metadat <- prepare_df(x$meta_estimates)

  gp <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$x, y = .data$estimate)) +
    ggplot2::geom_line(ggplot2::aes(group = .data$model, color = .data$model),
                       linetype = "dashed") +
    ggplot2::geom_line(data = metadat) +
    ggplot2::xlab(x$xvars) +
    ggplot2::ylab(x$terms) +
    ggplot2::theme_minimal() +
    ggplot2::labs(color = "Dataset")

  return(gp)

}
