#' Summary method for metagam objects
#'
#' @param object A metagam object as returned by \code{\link{metagam}}.
#' @param ... other arguments (not used).
#'
#' @return A list of class \code{summary.metagam} containing the following information:
#' \itemize{
#' \item \code{meta_pvals}: dataframe with p-values from each individual fit. These can be meta-analytically
#'   combined using the \code{metap} package.
#' \item \code{terms}: smooth terms that have been meta-analyzed.
#' \item \code{method}: method used for meta-analysis. See the \code{metafor} package for detailed description.
#' \item \code{intercept}: logical specifying whether or not the intercept has been included in the meta-analysis.
#' \item \code{cohorts}: Number of datasets ("cohorts") used in the meta-analysis.
#' }
#' @export
#'
summary.metagam <- function(object, ...){

  ret <- object

  class(ret) <- "summary.metagam"

  ret

}



