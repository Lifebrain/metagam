#' Summary method for GAMs stripped for rawdata
#'
#' @param object Object returned by \code{\link{strip_rawdata}}.
#' @param ... Other arguments.
#'
#' @return The function returns its input argument, which is printed to the console.
#' @export
#'
summary.striprawdata <- function(object, ...){
  object
}

#' Print method for striprawdata
#'
#' @param x Object of class \code{striprawdata}.
#' @param ... Other arguments.
#'
#' @return The function invisibly returns its argument.
#' @export
#'
print.striprawdata <- function(x, ...){
  cat("GAM stripped for individual participant data with strip_rawdata().\n")
  cat("For meta-analysis of smooth terms, use the following identifiers: ",
      paste(x$smooth_terms, collapse = ", "), ".\n\n", sep = "")
  cat("Original output for gam object:\n")
  writeLines(x$mgcv_output)

  invisible(x)
}
