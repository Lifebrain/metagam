#' Prepare a single GAM/GAMM fit for meta analysis
#'
#' @param model Fitted gam object.
#' @param grid Grid over which to compute posterior.
#' @param nmc Number of Monte Carlo samples.
#'
#' @return An object of class \code{singlegam}.
#' @export
#'
singlegam <- function(model, grid = NULL, nmc = 100) {

  # Extract variables
  response <- as.character(model$formula)[[2]]
  predictors <- colnames(model$model)[colnames(model$model) != response]

  stopifnot(do.call(all, lapply(names(grid), function(x) x %in% predictors)))

  X <- model$model[, predictors, drop = FALSE]

  # Create a grid from the variables
  grid <- lapply(X, function(x) {
    if(is.numeric(x)){
      r <- range(x)
      seq(from = r[[1]], to = r[[2]], length.out = 30)
      } else {
        unique(x)
      }
    })

  # Expand combinatorially
  grid <- expand.grid(grid)

  # Get the linear predictor matrix at grid
  Xp <- stats::predict(model, newdata = grid, type = "lpmatrix")

  # Sample regression coefficients from posterior
  betas <- mgcv::rmvn(n = nmc, mu = stats::coef(model), V = stats::vcov(model))

  # Compute posterior fits
  posterior_sample <- t(betas %*% t(Xp))

  result <- list(
    formula = model$formula,
    predictors = predictors,
    response = response,
    nmc = nmc,
    grid = grid,
    posterior_sample = posterior_sample
  )

  class(result) <- "singlegam"

  return(result)
}
