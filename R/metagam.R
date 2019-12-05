#' Meta analyze GAMs
#'
#' @param models list of models
#' @param probs posterior quantiles to return
#' @param grid_length lenght of grid
#'
#' @return An object of class \code{metagam}
#' @export
#'
metagam <- function(models, grid_length = 40, probs = c(.01, .025, .05, .95, .975, .99)){

  stopifnot(do.call(all, lapply(models, inherits, what = "singlegam")))
  stopifnot(do.call(all, lapply(models, function(m) nrow(m$grid) == nrow(m$posterior_sample))))
  stopifnot(length(unique(lapply(models, function(m) m$predictors))) == 1)

  predictors <- models[[1]]$predictors
  predictor_classes <- do.call(c, lapply(models[[1]]$grid, class))

  # Create a univariate grid per function
  # If numeric, interpolate to a common grid.
  all_posteriors <- lapply(predictors, function(x){
    # The other predictors, "comp" stands for "complement"
    x_comp <- setdiff(predictors, x)
    x_class <- predictor_classes[[x]]
    # Indices of rows to use for the other predictors
    # Simply take the first value of all other numeric predictors
    # TODO: Take midpoint of other variables and interpolate

    # Grids for each model
    grid_list <- create_common_grid(x, models, x_comp, x_class, grid_length)
    x_grids <- grid_list$x_grids
    common_x_grid <- grid_list$common_x_grid

    # Interpolate posterior over grid
    posterior_samples <- Map(function(m, g, i){
      # Extract the posterior samples at the grid
      posterior_sample <- m$posterior_sample[g$rows_comp, , drop = FALSE]
      # Interpolate each posterior curve
      if(x_class == "numeric"){
        posterior_sample <- lapply(as.data.frame(posterior_sample), function(y){
          stats::approx(x = as.numeric(g$x_grid[, x]), y = as.numeric(y), xout = common_x_grid)$y
        })
        posterior_sample <- unlist(posterior_sample, use.names = FALSE)
      } else {
        posterior_sample <- c(posterior_sample)
      }

      posterior_sample <- as.data.frame(cbind(common_x_grid, posterior_sample))
      colnames(posterior_sample) <- c(x, "value")
      posterior_sample$sample <- i

      return(posterior_sample)
    }, models, x_grids, seq(from = 1, to = length(models), by = 1))


    posterior_samples <- do.call(rbind, posterior_samples)

    # Compute the posterior quantities of interest
    posterior_samples <- split(posterior_samples, posterior_samples[, x])

    posterior_quantities <- lapply(posterior_samples, function(p){
      qts <- stats::quantile(p$value, probs = probs, na.rm = TRUE)
      data.frame(
        quantity = c("posterior_samples", "mean", "median", names(qts)),
        value = c(sum(!is.na(p$value)), mean(p$value, na.rm = TRUE), stats::median(p$value, na.rm = TRUE), qts),
        predictor = x,
        predictor_value = unique(p[, x]),
        stringsAsFactors = FALSE
      )
    })

    posterior_quantities <- do.call("rbind", posterior_quantities)
    rownames(posterior_quantities) <- NULL
    return(posterior_quantities)
  })
  names(all_posteriors) <- predictors

  result <- list(
    predictors = predictors,
    all_posteriors = all_posteriors
  )
  class(result) <- "metagam"

  return(result)

}
