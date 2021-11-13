#' Meta-analysis of generalized additive models
#'
#' @param models List of generalized additive models, each of which has been returned
#' by \code{\link{strip_rawdata}}. If the list is named, the names will be used in the output.
#' @param grid Grid of values of the explanatory variables over which to compute the
#' estimated smooth terms. Defaults to \code{NULL}, which means that a grid is set up
#' for the smooth terms defined by the \code{terms} argument, with length given by
#' \code{grid_size} for numeric variables and a single value of each factor variable.
#' @param grid_size Numeric value giving the number of elements to use in the grid of explanatory
#' variables when \code{grid=NULL}. When multiple terms are supplied, each combination of
#' values of explanatory variables are generated, and the number of grid
#' points becomes \code{grid_size} to the power of the number of terms.
#' @param type Type of prediction to use. Defaults to \code{"iterms"}. Available options
#' are \code{"iterms"}, \code{"link"}, and \code{"response"}. See the documentation
#' of \code{mgcv::predict.gam} for details. Note that \code{type="terms"} is not supported,
#' since it may result in estimated zero standard deviation for smooth terms.
#' @param terms Character vector of terms, smooth or parametric, to be included in function estimate.
#' Only used if \code{type="iterms"}. Defaults to \code{NULL}, which means
#' that the first smooth term when listed in alphabetic order is taken.
#' @param method Method of meta analysis, passed on to \code{metafor::rma.uni}. Defaults to \code{"FE"}. See the documentation to
#' \code{metafor::rma} for all available options.
#' @param nsim Number of simulations to conduct in order to compute p-values and simultaneous
#' confidence bands for the meta-analytic fit. Defaults to \code{NULL}, which means that no simulations
#' are performed.
#' @param ci_alpha Significance level for simultaneous confidence bands. Ignored if \code{nsim} is \code{NULL}, and defaults to 0.05.
#' @param intercept logical defining whether or not to include the intercept in each smooth
#' term. Only applies when \code{type = "iterms"}.
#' @param restrict_range Character vector of explanatory variables to restrict such that only
#' values within the range for each cohort contribute to the meta-analysis. Default to \code{NULL},
#' which means that each model contributes across the whole range specified by \code{grid}. Currently
#' not implemented.
#'
#' @details It is currently assumed that all models have been fit with the same smooth terms,
#' although they do not need to have the same basis functions or knot placement. Future versions
#' will also include meta-analysis of parametric terms in the models.
#'
#' p-values are truncated below at 1e-16 before computing meta-analytic p-values
#' to ensure that no values are identically zero, which would imply that the
#' alternative hypothesis be true with no uncertainty.
#'
#' @return An object of type metagam.
#' @export
#' @example /inst/examples/metagam_examples.R
metagam <- function(models, grid = NULL, grid_size = 100, type = "iterms", terms = NULL,
                    method = "FE", nsim = NULL, ci_alpha = 0.05,
                    intercept = FALSE, restrict_range = NULL){

  if(!(type %in% c("iterms", "link", "response"))){
    stop('type must be one of "iterms", "link", and "response"\n')
  }

  # Find the terms from each model
  model_terms <- do.call(rbind, lapply(models, function(x) {
    data.frame(term = names(x$term_list), variables = unlist(x$term_list))
  }))

  # Check if the user-specified term exists
  if(!is.null(terms) && !all(ind <- terms %in% model_terms$term)){
    stop("Unknown term ", paste(terms[!ind], collapse = " and "), " requested.\n")
  }

  # If terms are not supplied and type is "iterms" or "terms", find the smooth terms
  # Otherwise use all terms
  if(is.null(terms) && type %in% c("iterms", "terms")){
    terms <- model_terms$term[[order(model_terms$term)[[1]]]]
  } else if(type %in% c("link", "response")){
    terms <- sort(unique(model_terms$term))
  }

  # Find the variables corresponding to terms
  xvars <- unique(model_terms[model_terms$term %in% terms, "variables"])

  # Create grid if not supplied by user
  if(is.null(grid)){
    # Find the minimum and maximum from each model
    grid <- lapply(models, function(x){
      res <- lapply(x$var.summary, function(vs){
        if(is.numeric(vs)){
          c(min(vs), max(vs))
        } else {
          rep(vs, 2)
        }
      })
      as.data.frame(res)
    })
    grid <- do.call(rbind, grid)
    # Combine to get overall minimum and maximum
    nms <- names(grid)
    grid <- lapply(nms, function(nm){
      if(nm %in% xvars){
        seq(from = min(grid[, nm]), to = max(grid[, nm]), length.out = grid_size)
      } else {
        sort(grid[, nm])[[1]]
      }
    })
    names(grid) <- nms

    # Expand
    grid <- expand.grid(grid)
  }

  # Find the estimates from each model over the grid
  cohort_estimates <- lapply(seq_along(models), function(ind) {
    pred <- stats::predict(models[[ind]], newdata = grid, type = type,
                           se.fit = TRUE, terms = terms)

    estimate <- if(type %in% c("iterms", "terms")){
      estimate <- pred$fit + if(intercept) attr(pred, "constant") else 0
      as.data.frame(estimate)
    } else if(type %in% c("link", "response")){
      eval(parse(text = paste("data.frame(", type, "= as.numeric(pred$fit))")))
    }
    names(estimate) <- paste0("estimate_", names(estimate))

    standard_error <- if(type %in% c("iterms", "terms")){
      as.data.frame(pred$se.fit)
    } else if(type %in% c("link", "response")){
      eval(parse(text = paste("data.frame(", type, "= as.numeric(pred$se.fit))")))
    }
    names(standard_error) <- paste0("se_", names(standard_error))

    res <- cbind(grid, estimate, standard_error)
    res$model <- ind
    res
  })
  cohort_estimates <- do.call(rbind, cohort_estimates)

  # Now do the meta-analysis. First reshape the dataframe.
  cohort_estimates <- stats::reshape(
    cohort_estimates,
    varying = c(
      grep("^estimate_", names(cohort_estimates), value = TRUE),
      grep("^se_", names(cohort_estimates), value = TRUE)
      ),
    timevar = "term",
    direction = "long",
    sep = "_",
    split = list(regexp = "_", include = TRUE, fixed = TRUE)
    )
  cohort_estimates$id <- NULL
  names(cohort_estimates)[names(cohort_estimates) %in% c("estimate_", "se_")] <- c("estimate", "se")
  cohort_estimates$model <- as.factor(cohort_estimates$model)

  # Now nest the estimates at each grid point
  vars <- setdiff(names(cohort_estimates), c("model", "estimate", "se"))
  grp <- factor(eval(parse(text = paste("paste(", paste0('cohort_estimates$', vars, collapse = ","), ")"))))
  levels(grp) <- order(levels(grp))

  splitdat <- split(cohort_estimates, f = grp)
  meta_models <- lapply(
    splitdat, function(x){
      metafor::rma(yi = x$estimate, sei = x$se, method = method)
    })

  predictions <- lapply(meta_models, function(x){
    pred <- stats::predict(x)
    data.frame(
      estimate = pred$pred,
      se = pred$se,
      ci.lb = pred$ci.lb,
      ci.ub = pred$ci.ub
    )
  })

  meta_estimates <- cbind(
    do.call(rbind, lapply(splitdat, function(x) unique(x[vars]))),
        do.call(rbind, predictions))

  if(!is.null(nsim)){
    if(length(terms) > 1) stop("P-value simulations currently only work for a single term.\n")

    masd_list <- lapply(seq_along(models), function(ind){
      getmasd(models[[ind]], grid, nsim, terms)
    })
    sim_ci <- get_meta_sim_ci(models, ci_alpha, masd_list,
                              cohort_estimates, xvars, method, grid)

    meta_pval <- if(testfun(1/nsim, models, masd_list,
                            cohort_estimates, xvars, method, grid) > 0){
      paste0("<", 1/nsim)
    } else {
      tryCatch({
        opt <- stats::uniroot(testfun, interval = c(1/nsim, .99), models = models,
                              masd_list = masd_list, cohort_estimates = cohort_estimates,
                              xvars = xvars, method = method, grid = grid)
        paste0(round(opt$root, floor(log10(nsim)) + 1))
        },
        error = function(e) "NA")
    }

  } else {
    meta_pval <- sim_ci <- NULL
  }

  # Extract p-values
  pvals <- do.call(rbind, lapply(models, function(x) {
    dat <- as.data.frame(x$s.table)
    dat$term <- rownames(dat)
    dat <- dat[dat$term %in% terms, ]
    dat[, c("term", setdiff(names(dat), "term"))]
  }))

  result <- list(
    cohort_estimates = cohort_estimates,
    meta_models = meta_models,
    meta_estimates = meta_estimates,
    pvals = pvals,
    meta_pval = meta_pval,
    sim_ci = sim_ci,
    terms = terms,
    method = method,
    xvars = xvars,
    intercept = intercept,
    cohorts = length(models),
    type = type
  )
  class(result) <- "metagam"

  return(result)

}
