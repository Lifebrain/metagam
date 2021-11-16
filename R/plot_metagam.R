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
#'   require that \code{\link{metagam}} was run with \code{nsim} not equal to \code{NULL}.
#' @param ... Other arguments to plot.
#'
#' @return The function is called for its side effect of producing a plot.
#'
#'
#' @export
#'
#' @example /inst/examples/metagam_examples.R
#'
plot.metagam <- function(x, term = NULL, ci = "none", legend = FALSE, ...)
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

  xvars <- x$term_list[[term]]$xvars
  if(length(xvars) == 1){
    dat <- lapply(seq_along(x$cohort_estimates), function(ind) {
      if(x$type %in% c("iterms", "terms")){
        x$cohort_estimates[[ind]][[term]]
      } else {
        x$cohort_estimates[[ind]]
      }
    })

    if(ci %in% c("pointwise", "both")){
      alpha_quantiles = qnorm(c(ci.lb = x$ci_alpha / 2, ci.ub = 1 - x$ci_alpha / 2))
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

    plot_univariate_smooth(metadat, dat, xvars, x$type, term, ci, legend)
  } else if(length(xvars) == 2){
    gp <- plot_bivariate_smooth(metadat, xvars, x$type, term)
  } else {
    stop("plot.metagam currently only works for univariate or bivariate terms.")
  }

}

plot_bivariate_smooth <- function(metadat, xvars, type, term, ci){

  xl <- lapply(xvars, function(x) sort(unique(metadat[[x]])))
  names(xl) <- c("x", "y")
  zl <- matrix(metadat$estimate, nrow = length(unique(metadat$x)))
  graphics::image(x = xl, z = zl,
                  xlab = xvars[[1]], ylab = xvars[[2]])
  graphics::contour(x = xl, z = zl, col = "blue", lwd = 2,
                    add = TRUE, method = "edge",
                    vfont = c("sans serif", "plain"))
  title(ifelse(type == "iterms", term, type))

}

plot_univariate_smooth <- function(metadat, dat, xvar, type, term, ci, legend){

  rd <- range(metadat[["estimate"]], unlist(lapply(dat, function(x) x[["fit"]])))
  if(ci != "none"){
    rd <- c(rd, range(metadat[, grep("^ci", names(metadat))]))
  }

  plot(metadat[[xvar]], metadat[["estimate"]], type = "l",
       xlab = xvar,
       ylab = ifelse(type == "iterms", term, type),
       xlim = range(metadat[[xvar]]),
       ylim = range(rd))
  if(ci %in% c("both", "simultaneous")){
    polygon(x = c(rev(metadat$x), metadat$x),
            y = c(rev(metadat[, "ci.sim.ub"]), metadat[, "ci.sim.lb"]),
            col = "gray80", border = NA)
  }
  if(ci %in% c("both", "pointwise")){
    polygon(x = c(rev(metadat$x), metadat$x),
            y = c(rev(metadat[, "ci.ub"]), metadat[, "ci.lb"]),
            col = "gray60", border = NA)
  }
  lines(metadat[[xvar]], metadat[["estimate"]])
  iter <- seq_along(dat)
  for(i in iter){
    lines(dat[[i]][[xvar]], dat[[i]][["fit"]], lty = 2, col = i + 1L)
  }

  if(legend){
    legend("topright", legend = seq_along(dat), col = iter + 1L, lty = 2,
           title = "Dataset")
  }



}
