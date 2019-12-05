#' Title
#'
#' @param x Predictor name
#' @param models List of singlegam models
#' @param x_comp Complement of x, i.e., all other predictors
#' @param x_class Class of predictor
#' @param grid_length grid length
#'
#' @return List with a vector defining the common grid and the separate grids.
#' @export
#'
create_common_grid <- function(x, models, x_comp, x_class, grid_length){
  x_grids <- lapply(models, function(m){
    if(length(x_comp) > 0){
      rows_comp <- lapply(x_comp, function(z) m$grid[1, z] == m$grid[, z])
      rows_comp <- do.call(cbind, rows_comp)
      rows_comp <- apply(rows_comp, 1, all)
    } else {
      rows_comp <- TRUE
    }

    x_grid <- m$grid[rows_comp, , drop = FALSE]

    list(x_grid = x_grid, rows_comp = rows_comp)
  })

  all_grid_values <- unlist(lapply(x_grids, function(z) z$x_grid), use.names = FALSE)

  # Find the total range
  if(x_class == "numeric"){
    common_x_grid <- stats::quantile(all_grid_values, probs = seq(from = 0, to = 1, length.out = grid_length), names = FALSE)
  } else if(x_class %in% c("logical", "factor", "character")) {
    common_x_grid <- unique(all_grid_values)
  }

  result <- list(
    common_x_grid = common_x_grid,
    x_grids = x_grids
  )

  return(result)
}
