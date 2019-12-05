#' Title
#'
#' @param models List of models
#' @param grid_list List of grids
#' @param x Character with predictor name
#' @param x_class Class of variable
#'
#' @return Dataframe with interpolated values
#' @export
#'
interpolate <- function(models, grid_list, x, x_class){
  posterior_samples <- Map(function(m, g, i){
    # Extract the posterior samples at the grid
    posterior_sample <- m$posterior_sample[g$rows_comp, , drop = FALSE]
    # Interpolate each posterior curve
    if(x_class == "numeric"){
      posterior_sample <- lapply(as.data.frame(posterior_sample), function(y){
        stats::approx(x = as.numeric(g$x_grid[, x]), y = as.numeric(y), xout = grid_list$common_x_grid)$y
      })
      posterior_sample <- unlist(posterior_sample, use.names = FALSE)
    } else {
      posterior_sample <- c(posterior_sample)
    }

    posterior_sample <- as.data.frame(cbind(grid_list$common_x_grid, posterior_sample))
    colnames(posterior_sample) <- c(x, "value")
    posterior_sample$sample <- i

    return(posterior_sample)
  }, models, grid_list$x_grids, seq(from = 1, to = length(models), by = 1))

  posterior_samples <- do.call(rbind, posterior_samples)
  return(posterior_samples)
}
