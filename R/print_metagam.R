#' Print output from summary of metagam fit.
#'
#' @param x A summary.metagam object
#' @param digits Number of digits to print for meta-analytic p-values
#' @param ... Other arguments
#'
#' @return The function invisibly returns its input argument \code{x}.
#' @export
#'
print.summary.metagam <- function(x, digits = 8, ...){

  printfun(x)

  cat("Meta-analytic p-values of smooth terms:")
  print(
    knitr::kable(
      dplyr::mutate_at(x$meta_pvals, dplyr::vars(-.data$Test), ~ sprintf("%.3e", .))
      )
    )

  cat("\n\n")

  invisible(x)

}


#' Print method for metagam objects.
#'
#' @param x Object of class metagam.
#' @param ... Other arguments (not used).
#'
#' @return The function invisibly returns its input argument \code{x}.
#' @export
#'
print.metagam <- function(x, ...){
  printfun(x)
  invisible(x)
}



printfun <- function(x){
  cat("Meta-analysis of GAMs from ", x$cohorts, " cohorts, using method ", x$method, ".\n\n", sep = "")

  if(x$type %in% c("terms", "iterms")){
    cat("Smooth terms analyzed:", paste(x$terms, collapse = ", "), ".\n\n")
  } else if (x$type %in% c("link", "response")){
    cat(stringr::str_to_title(x$type), "function analyzed.\n\n")
  } else {
    stop("Unknown type", x$type, ".\n\n")
  }
}
