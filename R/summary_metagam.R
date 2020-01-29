#' Summary method for metagam objects
#'
#' @param object A metagam object as returned by \code{\link{metagam}}.
#' @param ... other arguments (not used).
#'
#' @return A list of summary information.
#' @export
#'
summary.metagam <- function(object, ...){


  metap_names <- c(
    sumz_pval = "Stouffer's sum of z",
    sump_pval = "Edgington's sum of p",
    maximump_pval = "Wilkinson's maximum p",
    minimump_pval = "Wilkinson's minimum p",
    logitp_pval = "logit p method",
    sumlog_pval = "Fisher's sum of logs"
  )

  meta_pvals <- dplyr::select_at(object$meta_pvals, dplyr::vars("term", dplyr::ends_with("pval")))
  meta_pvals <- tidyr::pivot_longer(meta_pvals, cols = dplyr::ends_with("pval"))
  meta_pvals <- tidyr::pivot_wider(meta_pvals, names_from = .data$term, values_from = .data$value)
  meta_pvals <- dplyr::mutate(meta_pvals, Test = metap_names[.data$name])
  meta_pvals <- dplyr::select(meta_pvals, -.data$name)
  meta_pvals <- dplyr::select(meta_pvals, .data$Test, dplyr::everything())

  ret <- list(
    meta_pvals = meta_pvals,
    terms = object$terms,
    method = object$method,
    intercept = object$intercept,
    cohorts = object$cohorts
  )

  class(ret) <- "summary.metagam"

  ret

}
