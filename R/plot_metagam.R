#' Plot estimated smooth terms
#'
#' Plot the meta-analytic estimate of a smooth term along with the separate fits
#' in each cohort.
#'
#' @param x Object returned by \code{\link{metagam}}.
#' @param term The smooth term to plot. Defaults to \code{NULL}, which means
#'   that the first term is plotted.
#' @param ci Type of confidence bands to plot around the meta-analytic fit.
#'   Defaults to "none", which means the no bands are plotted. Other options are
#'   "simultaneous", "pointwise", and "both". Simultaneous confidence bands
#'   require that \code{\link{metagam}} was run with \code{nsim} not equal to
#'   \code{NULL}.
#' @param legend Logical specifying whether or not to plot a legend. Defaults to
#'   \code{FALSE}.
#' @param only_meta Logical specifying whether to include the fits for each
#'   study, or to only plot the meta-analytic fit. Defaults to \code{FALSE}.
#' @param ... Other arguments to plot.
#'
#' @return The function is called for its side effect of producing a plot.
#'
#'
#' @export
#'
#' @example /inst/examples/metagam_examples.R
#'
plot.metagam <- function(x, term = NULL, ci = "none", legend = FALSE,
                         only_meta = FALSE, ...)
{
  if(!is.null(term) && length(term) > 1){
    stop("plot.metagam currently only works for a single term.")
  }
  if(is.null(term)){
    term <- names(x$term_list)[[1]]
  }

  stopifnot(x$type == "iterms" | length(x$term_list) == 1)

  if(x$type == "iterms"){
    metadat <- x$meta_models[[term]]$predictions
  } else {
    metadat <- x$meta_models$predictions
  }

  xvars <- x$term_list[[term]]$xvars
  metadat <- metadat[, c(xvars, "estimate", "se")]

  if(length(xvars) == 1){
    nms <- if(is.null(names(x$cohort_estimates))){
      seq_along(x$cohort_estimates)
    } else {
      names(x$cohort_estimates)
    }

    dat <- lapply(seq_along(x$cohort_estimates), function(ind) {
      if(x$type %in% c("iterms", "terms")){
        ret <- x$cohort_estimates[[ind]][[term]]
      } else {
        ret <- x$cohort_estimates[[ind]]
      }

      ret$model <- nms[[ind]]
      ret
    })

    common_cols <- Reduce(intersect, lapply(dat, colnames))
    dat <- lapply(dat, function(df) df[, common_cols, drop = FALSE])

    dat <- do.call(rbind, dat)
    dat <- dat[, c(xvars, "fit", "se.fit", "model")]


    if(ci %in% c("pointwise", "both")){
      alpha_quantiles = stats::qnorm(c(ci.lb = x$ci_alpha / 2, ci.ub = 1 - x$ci_alpha / 2))
      for(i in seq_along(alpha_quantiles)){
        eval(parse(text = paste0("metadat$", names(alpha_quantiles)[[i]], "<- metadat$estimate +",
              alpha_quantiles[[i]], "* metadat$se")))
      }
    }
    if(ci %in% c("simultaneous", "both")){
      if(is.null(x$simulation_results)){
        stop("Simultaneous confidence bands require that metagam was run with nsim not equal to NULL.")
      }

      metadat$ci.sim.lb <- x$simulation_results[[term]]$meta_sim_ci$estimate -
        x$simulation_results[[term]]$meta_sim_ci$se
      metadat$ci.sim.ub <- x$simulation_results[[term]]$meta_sim_ci$estimate +
        x$simulation_results[[term]]$meta_sim_ci$se
    }

    plot_univariate_smooth(metadat, dat, xvars, term, ci, legend, only_meta)
  } else if(length(xvars) == 2){
    metadat <- metadat[, c(xvars, "estimate", "se")]

    plot_bivariate_smooth(metadat, xvars)
  } else {
    stop("plot.metagam currently only works for univariate or bivariate terms.")
  }

}

plot_bivariate_smooth <- function(metadat, xvars){

  names(metadat)[names(metadat) %in% xvars] <- c("xxxaaa", "xxxbbb")

  ggplot2::ggplot(metadat, ggplot2::aes(x = .data$xxxaaa, y = .data$xxxbbb,
                                        z = .data$estimate)) +
    ggplot2::geom_raster(ggplot2::aes(fill = .data$estimate)) +
    ggplot2::geom_contour() +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_distiller(palette = "RdBu", type = "div") +
    ggplot2::xlab(xvars[[1]]) +
    ggplot2::ylab(xvars[[2]])
}

plot_univariate_smooth <- function(metadat, dat, xvars, term, ci, legend, only_meta){

  names(metadat)[names(metadat) == xvars] <- names(dat)[names(dat) == xvars] <- "xxxaaa"

  gp <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$xxxaaa, y = .data$fit)) +
    ggplot2::geom_line(data = metadat, ggplot2::aes(y = .data$estimate)) +
    ggplot2::ylab(term) +
    ggplot2::theme_minimal() +
    ggplot2::xlab(xvars)

  if(!only_meta){
    gp <- gp +
      ggplot2::geom_line(ggplot2::aes(group = factor(.data$model), color = factor(.data$model)),
                       linetype = "dashed") +
      ggplot2::labs(color = "Dataset")
  }

  if(ci %in% c("pointwise", "both")){
    gp <- gp +
      ggplot2::geom_ribbon(data = metadat, ggplot2::aes(y = .data$estimate, ymin = .data$ci.lb, ymax = .data$ci.ub), alpha = .3)
  }
  if(ci %in% c("simultaneous", "both")){
    gp <- gp +
      ggplot2::geom_ribbon(data = metadat, ggplot2::aes(y = .data$estimate, ymin = .data$ci.sim.lb, ymax = .data$ci.sim.ub), alpha = .3)
  }
  if(!legend){
    gp <- gp +
      ggplot2::theme(legend.position = "none")
  }
  gp
}
