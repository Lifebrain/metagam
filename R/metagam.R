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
                    method = "FE", intercept = FALSE, restrict_range = NULL){

  if(!(type %in% c("iterms", "link", "response"))){
    stop('type must be one of "iterms", "link", and "response"\n')
  }

  # Find the terms from each model
  model_terms <- do.call(rbind, lapply(models, function(x) x$term_df))

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
  xvars <- unique(unlist(lapply(model_terms[model_terms$term %in% terms, "variables"], function(x) x)))

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
      as.data.frame(do.call(cbind, res))
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
  cohort_estimates <- purrr::map_dfr(models, function(x) {
    pred <- stats::predict(x, newdata = grid, type = type,
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

    cbind(grid, estimate, standard_error)
  }, .id = "model")


  # Now do the meta-analysis. First reshape the dataframe.
  cohort_estimates <- tidyr::pivot_longer(
    cohort_estimates,
    cols = union(dplyr::starts_with("estimate_"), dplyr::starts_with("se_")),
    names_to = c(".value", "term"),
    names_pattern = "([[:alpha:]]+)\\_(.*)")

  # Now nest the estimates at each grid point
  meta_estimates <- dplyr::group_by_at(cohort_estimates,
                                       dplyr::vars(-"model", -"estimate", -"se"))
  meta_estimates <- tidyr::nest(meta_estimates)

  meta_estimates <- dplyr::mutate(
    meta_estimates,
    meta_model = lapply(.data$data, function(x){
      metafor::rma(yi = c(x$estimate), sei = c(x$se), method = method)
    })
    )

  meta_estimates <- dplyr::ungroup(meta_estimates)
  meta_estimates <- dplyr::bind_cols(
    meta_estimates,
    purrr::map_dfr(meta_estimates$meta_model, function(x) {
      pred <- stats::predict(x)

      dplyr::tibble(
        estimate = pred$pred,
        se = pred$se,
        ci.lb = pred$ci.lb,
        ci.ub = pred$ci.ub
      )
      }))

  # Extract p-values
  pvals <- purrr::map_dfr(models, function(x) {
    dat <- x$s.table
    tmp_terms <- rownames(dat)
    dat <- dplyr::as_tibble(dat)
    dat <- dplyr::mutate(dat, term = tmp_terms)
    dat <- dplyr::filter(dat, .data$term %in% terms)
    dat <- dplyr::select(dat, .data$term, dplyr::everything())
    dat
  }, .id = "model")

  # Split by term and meta-analyze p-values
  meta_pvals <- dplyr::group_by(pvals, .data$term)
  meta_pvals <- tidyr::nest(meta_pvals)

  result <- list(
    cohort_estimates = cohort_estimates,
    meta_estimates = meta_estimates,
    pvals = pvals,
    meta_pvals = meta_pvals,
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
