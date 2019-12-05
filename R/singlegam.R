#' Prepare a single GAM/GAMM fit for meta analysis
#'
#' @param model Fitted gam object.
#' @param grid Grid over which to compute posterior.
#' @param nmc Number of Monte Carlo samples.
#' @param freq Logical
#' @param unconditional Logical
#' @param grid_length length
#'
#' @return An object of class \code{singlegam}.
#' @export
#'
singlegam <- function(model, grid = NULL, nmc = 100, grid_length = 30, freq = FALSE, unconditional = FALSE) {

  # Extract variables
  response <- as.character(model$formula)[[2]]
  predictors <- colnames(model$model)[colnames(model$model) != response]

  X <- model$model[, predictors, drop = FALSE]

  # Create a grid from the variables
  grid <- lapply(X, function(x) {
    if(is.numeric(x)){
      quantile(x, probs = seq(from = 0, to = 1, length.out = grid_length), names = FALSE)
      } else {
        unique(x)
      }
    })

  # Expand combinatorially
  grid <- expand.grid(grid)

  # Get the linear predictor matrix at grid
  Xp <- stats::predict(model, newdata = grid, type = "lpmatrix")

  # Sample regression coefficients from posterior
  betas <- mgcv::rmvn(n = nmc, mu = stats::coef(model), V = stats::vcov(model, freq = freq, unconditional = unconditional))

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
