#' Meta analyze GAMs
#'
#' @param models list of models
#' @param random_effects logical
#' @param probs posterior quantiles to return
#'
#' @return An object of class \code{metagam}
#' @export
#'
metagam <- function(models, random_effects = FALSE, probs = seq(0, 1, .25)){

  stopifnot(do.call(all, lapply(models, inherits, what = "singlegam")))
  stopifnot(do.call(all, lapply(models, function(m) nrow(m$grid) == nrow(m$posterior_sample))))
  stopifnot(length(unique(lapply(models, function(m) m$predictors))) == 1)

  predictors <- models[[1]]$predictors
  grid <- models[[1]]$grid
  predictor_classes <- do.call(c, lapply(models[[1]]$grid, class))

  # Create a joint dataframe
  posteriors <- Map(function(m, i){
    cbind(m$grid, fit = c(m$posterior_sample), sample = i)
  }, models, seq(from = 1, to = length(models), by = 1))

  posteriors <- do.call(rbind, posteriors)
  posteriors <- split(posteriors, f = posteriors[, predictors, drop = FALSE])
  posterior_quantiles <- Map(function(p, i) {
    qts <- stats::quantile(p$fit, probs = probs)
    data.frame(
      quantity = c("mean", "median", names(qts)),
      value = c(mean(p$fit), stats::median(p$fit), qts),
      row_number = i,
      stringsAsFactors = FALSE
    )
  }, posteriors, seq(from = 1, to = length(posteriors), by = 1))


  posterior_quantiles <- do.call(rbind, posterior_quantiles)
  rownames(posterior_quantiles) <- NULL

  grid$row_number = seq(from = 1, to = nrow(grid), by = 1)

  posterior_quantiles <- merge(posterior_quantiles, grid, by = "row_number")

  result <- list(
    predictors = predictors,
    posterior_quantiles = posterior_quantiles
  )
  class(result) <- "metagam"

  return(result)

}
