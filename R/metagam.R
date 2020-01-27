#' Meta analyze nonlinear models
#'
#' @param fits List of GAM or GAMM fits of class metagam.
#' @param grid Grid over which to predict.
#' @param type defaults to "iterms"
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
metagam <- function(fits, grid, type = "iterms", terms = NULL, method = "fixed",
                    restrict_max = NULL, restrict_min = NULL, intercept = FALSE){

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

    } else {
      dat <- dplyr::mutate(grid, fit = as.numeric(pred$fit),
                    se = as.numeric(pred$se),
                    term = type)
    }

    return(dat)

  }, .id = "cohort")

  fit_meta <- dplyr::group_by_at(fit_comb, dplyr::vars(-"fit", -"se", -"cohort"))

  fit_meta <- purrr::pmap_dfr(tidyr::nest(fit_meta), function(...){
    args <- list(...)
    m <- mvmeta::mvmeta(formula = args$data$fit, S = args$data$se^2, method = method)

    res <- dplyr::bind_cols(
      dplyr::as_tibble(args[names(args) != "data"]),
      dplyr:: tibble(
        fit = as.numeric(m$coefficients),
        se = sqrt(as.numeric(m$vcov)),
        q = as.numeric(mvmeta::qtest(m)$Q),
        qp = as.numeric(mvmeta::qtest(m)$p)
      )
    )


  })


  res <- purrr::pmap_dfr(tidyr::nest(fit_meta), function(...){
    args <- list(...)
    m <- mvmeta::mvmeta(formula = args$data$fit, S = args$data$se^2, method = method)
    dplyr::tibble( res=stats::residuals(m) )
  })



  pvals <- purrr::map_dfr(fits, function(fit){
    dat <- suppressWarnings(summary(fit))$s.table[terms, "p-value", drop = FALSE]
    term_names <- rownames(dat)
    dat <- tibble::as_tibble(dat)
    dat <- dplyr::mutate(dat, term = term_names)
    dat <- dplyr::rename(dat, pval = "p-value")
  })

  pvals <- purrr::map_dfr(split(pvals, pvals$term), function(x){
    purrr::map_dfr(list(sumz = metap::sumz, sump = metap::sump,
                        maximump = metap::maximump, minimump = metap::minimump,
                  logitp = metap::logitp, sumlog = metap::sumlog), function(f){
                    c(f(x$pval)$p)
                  })
  }, .id = "term")

  #browser()

  result <- list(
    prediction = fit_meta,
    cohort_fits = fit_comb,
    pvals = pvals,
    residuals = res
  )
  class(result) <- "metagam"

  return(result)

}
