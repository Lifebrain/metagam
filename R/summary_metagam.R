#' Summary method for metagam objects
#'
#' @param object A metagam object as returned by \code{\link{metagam}}.
#' @param ... other arguments (not used).
#'
#' @return A list of class \code{summary.metagam} containing the following information:
#' \itemize{
#' \item \code{meta_pvals}: dataframe with meta-analytic p-values computed using the \code{metap} package.
#' \item \code{terms}: smooth terms that have been meta-analyzed.
#' \item \code{method}: method used for meta-analysis. See the \code{metafor} package for detailed description.
#' \item \code{intercept}: logical specifying whether or not the intercept has been included in the meta-analysis.
#' \item \code{cohorts}: Number of datasets ("cohorts") used in the meta-analysis.
#' }
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
    cohorts = object$cohorts,
    type = object$type
  )

  class(ret) <- "summary.metagam"

  ret

}



