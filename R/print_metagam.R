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
    cat(paste0("Smooth terms analyzed: ", paste(names(x$term_list), collapse = ", "), ".\n\n"))
  } else if (x$type %in% c("link", "response")){
    cat(paste0(x$type, "function analyzed.\n\n"))
  } else {
    stop("Unknown type", x$type, ". ")
  }

  if(!is.null(x$simulation_results)){
    pval <- lapply(x$simulation_results, function(x) x$pval)

    cat("P-values for smooth terms:\n")
    for(term in names(pval)){
      cat(paste0(term, ": ", pval[[term]]))
    }
  }
}
