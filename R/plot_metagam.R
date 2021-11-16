#' Plot estimated smooth terms
#'
#' Plot the meta-analytic estimate of a smooth term along with the separate fits in each cohort.
#'
#' @param x Object returned by \code{\link{metagam}}.
#' @param term The smooth term to plot. Defaults to \code{NULL}, which means that the first term is plotted.
#' @param ... Other arguments to plot.
#'
#' @return The function is called for its side effect of producing a plot.
#'
#'
#' @export
#'
#' @example /inst/examples/metagam_examples.R
#'
plot.metagam <- function(x, term = NULL, ...)
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

    plot_univariate_smooth(metadat, dat, xvars, x$type, term)
  } else if(length(xvars) == 2){
    gp <- plot_bivariate_smooth(metadat, xvars, x$type, term)
  } else {
    stop("plot.metagam currently only works for univariate or bivariate terms.")
  }

}

plot_bivariate_smooth <- function(metadat, xvars, type, term){

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

plot_univariate_smooth <- function(metadat, dat, xvar, type, term){
  plot(metadat[[xvar]], metadat[["estimate"]], type = "l",
       xlab = xvar,
       ylab = ifelse(type == "iterms", term, type),
       xlim = range(metadat[[xvar]]),
       ylim = range(c(metadat[["estimate"]], unlist(lapply(dat, function(x) x[["fit"]])))))
  iter <- seq_along(dat)
  for(i in iter){
    lines(dat[[i]][[xvar]], dat[[i]][["fit"]], lty = 2, col = i + 1L)
  }
  legend("topright", legend = seq_along(dat), col = iter + 1L, lty = 2,
         title = "Dataset")

}
