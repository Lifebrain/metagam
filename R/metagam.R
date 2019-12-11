#' Meta analyze nonlinear models
#'
#' @param fits List of GAM or GAMM fits of class metagam.
#' @param grid Grid over which to predict.
#' @param type defaults to "terms"
#' @param method Method of meta analysis. Defaults to "fixed".
#' @param terms Character vector of terms, smooth or parametric.
#'
#' @return An object of type metagam.
#' @export
#'
metagam <- function(fits, grid, type = "terms", terms = NULL, method = "fixed"){
  fit_comb <- purrr::map_dfr(fits, function(fit){
    pred <- stats::predict(fit, newdata = grid, type = type, se.fit = TRUE,
                           terms = terms, newdata.guaranteed = TRUE)

    tidyr::pivot_longer(dplyr::as_tibble(pred$fit), tidyr::everything(),
                        names_to = "term", values_to = "fit")

    # Fix column names of fit
    colnames(pred$fit) <- paste0("fit.", colnames(pred$fit))
    colnames(pred$se.fit) <- paste0("se.", colnames(pred$se.fit))

    dat <- dplyr::bind_cols(grid, dplyr::as_tibble(pred$fit), dplyr::as_tibble(pred$se.fit))
    tidyr::pivot_longer(dat,
                        cols = union(dplyr::starts_with("fit."), dplyr::starts_with("se.")),
                        names_to = c(".value", "term"),
                        names_pattern = "([[:alnum:]]+)\\.([[:alnum:]\\(\\)0-9]+)"
                        )

    # Deal with intercept later
    #attr(pred, "constant")
  }, .id = "cohort")

  fit_comb <- dplyr::group_by_at(fit_comb, dplyr::vars(-"fit", -"se", -"cohort"))

  if(method != "fixed"){
    fit_comb <- purrr::pmap_dfr(tidyr::nest(fit_comb), function(...){
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
    fit_comb <- dplyr::summarise(
      fit_comb,
      fit = dplyr::coalesce(sum(rlang::.data$fit * rlang::.data$se^(-2)) /
                              sum(rlang::.data$se ^ (-2)), mean(rlang::.data$fit)),
      se = sum(rlang::.data$se ^ (-2)) ^ (-1/2)
    )
    dplyr::ungroup(fit_comb)
  }





  result <- list(
    prediction = fit_comb
  )
  class(result) <- "metagam"

  return(result)

}
