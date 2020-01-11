#' Meta analyze nonlinear models
#'
#' @param fits List of GAM or GAMM fits of class metagam.
#' @param grid Grid over which to predict.
#' @param type defaults to "terms"
#' @param method Method of meta analysis. Defaults to "fixed".
#' @param terms Character vector of terms, smooth or parametric.
#' @param restrict_max char
#' @param restrict_min char
#' @param intercept logical defining whether or not to shift the output values
#'   by the intercept. Only applies when \code{type = "terms"} or \code{type = "iterms"}.
#'
#' @return An object of type metagam.
#' @export
#'
metagam <- function(fits, grid, type = "terms", terms = NULL, method = "fixed",
                    restrict_max = NULL, restrict_min = NULL, intercept = TRUE){
  fit_comb <- purrr::map_dfr(fits, function(fit){
    # Reduce grid to be within the range
    if(!is.null(restrict_min)){
      for(var in restrict_min){
        inds <- grid[, restrict_min] > fit$var_ranges[[restrict_min]]$min
        grid <- grid[inds, ]
      }
    }

    if(!is.null(restrict_max)){
      for(var in restrict_max){
        inds <- grid[, restrict_max] < fit$var_ranges[[restrict_max]]$max
        grid <- grid[inds, ]
      }
    }


    pred <- stats::predict(fit, newdata = grid, type = type, se.fit = TRUE,
                           terms = terms, newdata.guaranteed = TRUE)

    # Fix column names of fit
    if(type %in% c("iterms", "terms")){
      colnames(pred$fit) <- paste0("fit.", colnames(pred$fit))
      colnames(pred$se.fit) <- paste0("se.", colnames(pred$se.fit))
      dat <- dplyr::bind_cols(grid, dplyr::as_tibble(pred$fit), dplyr::as_tibble(pred$se.fit))
      dat <- tidyr::pivot_longer(dat,
                                 cols = union(dplyr::starts_with("fit."), dplyr::starts_with("se.")),
                                 names_to = c(".value", "term"),
                                 names_pattern = "([[:alnum:]]+)\\.([[:alnum:]\\(\\)0-9]+)"
      )

      if(intercept){
        dat <- dplyr::mutate(dat, fit = .data$fit + attr(pred, "constant"))
      }
      return(dat)

    } else {
      dplyr::mutate(grid, fit = as.numeric(pred$fit),
                    se = as.numeric(pred$se),
                    term = type)
    }

  }, .id = "cohort")

  fit_meta <- dplyr::group_by_at(fit_comb, dplyr::vars(-"fit", -"se", -"cohort"))

  if(method != "fixed"){
    fit_meta <- purrr::pmap_dfr(tidyr::nest(fit_meta), function(...){
      args <- list(...)
      m <- mvmeta::mvmeta(formula = args$data$fit, S = args$data$se^2, method = method)

      dplyr::bind_cols(
        dplyr::as_tibble(args[names(args) != "data"]),
        dplyr:: tibble(
          fit = as.numeric(m$coefficients),
          se = sqrt(as.numeric(m$vcov))
        )
      )
    })
  } else {
    fit_meta <- dplyr::summarise(
      fit_meta,
      fit = dplyr::coalesce(sum(.data$fit * .data$se^(-2)) /
                              sum(.data$se ^ (-2)), mean(.data$fit)),
      se = sum(.data$se ^ (-2)) ^ (-1/2)
    )
    fit_meta <- dplyr::ungroup(fit_meta)
  }

  result <- list(
    prediction = fit_meta,
    cohort_fits = fit_comb
  )
  class(result) <- "metagam"

  return(result)

}
