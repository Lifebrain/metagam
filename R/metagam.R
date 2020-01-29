#' Meta-analysis of generalized additive models
#'
#' @param models List of generalized additive models, each of which has been returned
#' by \code{\link{strip_rawdata}}. If the list is named, the names will be used in the output.
#' @param grid Grid of values of the explanatory variables over which to compute the
#' estimated smooth terms. Defaults to \code{NULL}, which means that a grid is set up
#' for the smooth terms defined by the \code{terms} argument, with length given by
#' \code{grid_size} for numeric variables and a single value of each factor variable.
#' @param grid_size Numeric value giving the number of elements to use in the grid of explanatory
#' variables when \code{grid=NULL}.
#' @param type Type of prediction to use. Defaults to \code{"iterms"}. Available options
#' are \code{"iterms"}, \code{"link"}, and \code{"response"}. See the documentation
#' of \code{mgcv::predict.gam} for details. Note that \code{type="terms"} is not supported,
#' since it may result in estimated zero standard deviation for smooth terms.
#' @param terms Character vector of terms, smooth or parametric, to be included in function estimate.
#' Only used if \code{type="iterms"}. Defaults to \code{NULL}, which means
#' that all smooth terms are computed.
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
#'
metagam <- function(models, grid = NULL, grid_size = 10, type = "iterms", terms = NULL,
                    method = "FE", intercept = FALSE, restrict_range = NULL){

  if(!(type %in% c("iterms", "link", "response"))){
    stop('type must be one of "iterms", "link", and "response"\n')
  }

  # If terms are not supplied, find the smooth terms
  if(type == "iterms" && is.null(terms)){
    terms <- unlist(unique(purrr::map(models, function(x) x$smooth_labels)))
  }

  # Create grid if not supplied by user
  if(is.null(grid)){
    # Find the minimum and maximum from each model
    grid <- purrr::map_dfr(models, function(x){
      purrr::map_dfr(x$var.summary, function(vs){
        if(is.numeric(vs)){
          c(min(vs), max(vs))
        } else {
          rep(vs, 2)
        }
      })
    })
    # Combine to get overall minimum and maximum
    grid <- purrr::map(grid, function(x) {
      if(is.numeric(x)) seq(from = min(x), to = max(x), length.out = grid_size)
      else sort(x)[[1]]
    })

    # Expand
    grid <- dplyr::as_tibble(expand.grid(grid))
  }

  # Find the estimates from each model over the grid
  cohort_estimates <- purrr::map_dfr(models, function(x) {
    pred <- stats::predict(x, newdata = grid, type = type,
                           se.fit = TRUE, terms = terms)

    estimate <- if(type %in% c("iterms", "terms")){
      estimate <- pred$fit + if(intercept) attr(pred, "constant") else 0
      dplyr::as_tibble(estimate)
    } else if(type %in% c("link", "response")){
      dplyr::tibble(!!type := pred$fit)
    }
    estimate <- dplyr::rename_all(estimate, function(x) paste0("est_", x))

    standard_error <- if(type %in% c("iterms", "terms")){
      dplyr::as_tibble(pred$se.fit)
    } else if(type %in% c("link", "response")){
      dplyr::tibble(!!type := pred$se.fit)
    }
    standard_error <- dplyr::rename_all(standard_error, function(x) paste0("se_", x))

    dplyr::bind_cols(grid, estimate, standard_error)
  }, .id = "model")


  # Now do the meta-analysis. First reshape the dataframe.
  cohort_estimates <- tidyr::pivot_longer(
    cohort_estimates,
    cols = union(dplyr::starts_with("est_"), dplyr::starts_with("se_")),
    names_to = c(".value", "term"),
    names_pattern = "([[:alpha:]]+)\\_(.*)")


  # Now nest the estimates at each grid point
  meta_estimates <- dplyr::group_by_at(cohort_estimates,
                                       dplyr::vars(-"model", -"est", -"se"))
  meta_estimates <- tidyr::nest(meta_estimates)

  meta_estimates <- dplyr::mutate(
    meta_estimates,
    meta_model = purrr::map(.data$data, function(x){
      metafor::rma(yi = x$est, sei = x$se, method = method)
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

  # Move this to strip_rawdata
  pvals <- purrr::map_dfr(models, function(x) {
    dat <- x$s.table
    terms <- rownames(dat)
    dat <- dplyr::as_tibble(dat)
    dat <- dplyr::mutate(dat, term = terms)
    dat <- dplyr::select(dat, .data$term, dplyr::everything())
    dat
  }, .id = "model")

  # Split by term and meta-analyze p-values
  meta_pvals <- dplyr::group_by(pvals, .data$term)
  meta_pvals <- tidyr::nest(meta_pvals)

  # Create a tibble which contains both the meta-analytic p-values
  # and the full objects returned by metap functions
  meta_pvals <- purrr::pmap_dfr(meta_pvals, function(term, data){
    df <- purrr::imap_dfc(
      list(
        sumz = metap::sumz,
        sump = metap::sump,
        maximump = metap::maximump,
        minimump = metap::minimump,
        logitp = metap::logitp,
        sumlog = metap::sumlog
        ),
      function(f, n){
        dplyr::tibble(!!n := list(f(pmax(!!data$`p-value`, 1e-16))))
      })
    df <- dplyr::mutate_all(df, list(pval = ~ as.numeric(.[[1]]$p)))
    df <- dplyr::mutate(df, term = term)
    df <- dplyr::select(df, .data$term, dplyr::ends_with("pval"), dplyr::everything())
  })


  result <- list(
    cohort_estimates = cohort_estimates,
    meta_estimates = meta_estimates,
    pvals = pvals,
    meta_pvals = meta_pvals
  )
  class(result) <- "metagam"

  return(result)

}
