#' Meta-analysis of generalized additive models
#'
#' @param models List of generalized additive models, each of which has been
#'   returned by \code{\link{strip_rawdata}}. If the list is named, the names
#'   will be used in the output.
#' @param grid Grid of values of the explanatory variables over which to compute
#'   the estimated smooth terms. Defaults to \code{NULL}, which means that a
#'   grid is set up for the smooth terms defined by the \code{terms} argument,
#'   with length given by \code{grid_size} for numeric variables and a single
#'   value of each factor variable.
#' @param grid_size Numeric value giving the number of elements to use in the
#'   grid of explanatory variables when \code{grid=NULL}. When multiple terms
#'   are supplied, each combination of values of explanatory variables are
#'   generated, and the number of grid points becomes \code{grid_size} to the
#'   power of the number of terms.
#' @param type Type of prediction to use. Defaults to \code{"iterms"}. Available
#'   options are \code{"iterms"}, \code{"link"}, and \code{"response"}. See the
#'   documentation of \code{mgcv::predict.gam} for details. Note that
#'   \code{type="terms"} is not supported, since it may result in estimated zero
#'   standard deviation for smooth terms.
#' @param terms Character vector of terms, smooth or parametric, to be included
#'   in function estimate. Only used if \code{type="iterms"}. Defaults to
#'   \code{NULL}, which means that the first smooth term when listed in
#'   alphabetic order is taken.
#' @param method Method of meta analysis, passed on to \code{metafor::rma.uni}.
#'   Defaults to \code{"FE"}. See the documentation to \code{metafor::rma} for
#'   all available options.
#' @param nsim Number of simulations to conduct in order to compute p-values and
#'   simultaneous confidence bands for the meta-analytic fit. Defaults to
#'   \code{NULL}, which means that no simulations are performed. Only used if
#'   \code{type="iterms"}.
#' @param ci_alpha Significance level for simultaneous confidence bands. Ignored
#'   if \code{nsim} is \code{NULL}, and defaults to 0.05.
#' @param restrict_range Character vector of explanatory variables to restrict
#'   such that only values within the range for each cohort contribute to the
#'   meta-analysis. Default to \code{NULL}, which means that each model
#'   contributes across the whole range specified by \code{grid}. Currently not
#'   implemented.
#'
#' @details It is currently assumed that all models have been fit with the same
#'   smooth terms, although they do not need to have the same basis functions or
#'   knot placement. Future versions will also include meta-analysis of
#'   parametric terms in the models.
#'
#'   p-values are truncated below at 1e-16 before computing meta-analytic
#'   p-values to ensure that no values are identically zero, which would imply
#'   that the alternative hypothesis be true with no uncertainty.
#'
#' @return An object of type metagam.
#' @export
#' @example /inst/examples/metagam_examples.R
metagam <- function(models, grid = NULL, grid_size = 100, type = "iterms", terms = NULL,
                    method = "FE", nsim = NULL, ci_alpha = 0.05,
                    restrict_range = NULL){

  if(!(type %in% c("iterms", "link", "response"))){
    stop('type must be one of "iterms", "link", and "response"\n')
  }
  if(length(ci_alpha) > 1) stop("Only one confidence level for confidence bands allowed.")

  term_list <- find_terms(models, type, terms)
  if(is.null(grid)){
    grid <- create_grid(models, term_list, grid_size)
  }
  cohort_estimates <- lapply(models, function(model){
    extract_model_fits(model, term_list, grid, type)
  })

  if(type == "iterms"){
    meta_models <- Map(function(term, xvar){
      term_fits <- lapply(cohort_estimates, function(x) x[[term]][["fit"]])
      term_ses <- lapply(cohort_estimates, function(x) x[[term]][["se.fit"]])
      meta_models <- fit_meta_models(term_fits, term_ses, method)
      predictions <- cbind(
        create_newdat(xvar = xvar, grid = grid, type = type),
        get_predictions(meta_models))
      list(meta_models = meta_models, predictions = predictions)
    }, term = names(term_list), xvar = lapply(term_list, function(x) x$xvars))
  } else {
    fits <- lapply(cohort_estimates, function(x) x[["fit"]])
    ses <- lapply(cohort_estimates, function(x) x[["se.fit"]])
    meta_models <- fit_meta_models(fits, ses, method)
    meta_models <- list(
      meta_models = meta_models,
      predictions = cbind(
        create_newdat(term_list = term_list, grid = grid, type = type),
        get_predictions(meta_models))
    )
  }

  if(!is.null(nsim) && type == "iterms"){
    simulation_results <- simulate(term_list, grid, models, nsim, cohort_estimates, ci_alpha, method)
  } else {
    simulation_results <- NULL
  }

  result <- list(
    cohort_estimates = cohort_estimates,
    meta_models = meta_models,
    simulation_results = simulation_results,
    pvals = lapply(cohort_estimates, attr, "s.table"),
    term_list = term_list,
    method = method,
    ci_alpha = ci_alpha,
    type = type
  )
  class(result) <- "metagam"

  return(result)

}
