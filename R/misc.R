#' Get maximum of the absolute standard deviations
#'
#' This code is based on Dr. Gavin Simpson's blog post https://fromthebottomoftheheap.net/2016/12/15/simultaneous-interval-revisited/.
#'
#' @param mod Object of class "striprawdata", returned from \code{\link{strip_rawdata}}.
#' @param grid Grid of values over which to create a simultaneous confidence interval.
#' @param nmc Number of Monte Carlo samples.
#' @param terms Character vector of smooth terms.
#'
#' @return A vector of maxima of absolute standard deviations.
#' @export
#' @keywords internal
#'
getmasd <- function(mod, grid, nmc, terms){
  Vb <- stats::vcov(mod)
  inds <- grep(gsub(")", "\\)", gsub("(", "\\(", terms, fixed = TRUE), fixed = TRUE), colnames(Vb))
  Vb <- Vb[inds, inds]
  pred <- stats::predict(mod, grid, se.fit = TRUE, terms = terms, type = "iterms")
  se.fit <- pred$se.fit

  BUdiff <- mgcv::rmvn(nmc, mu = rep(0, nrow(Vb)), V = Vb)
  Cg <- stats::predict(mod, grid, type = "lpmatrix")[, inds]
  simDev <- Cg %*% t(BUdiff)
  absDev <- abs(sweep(simDev, 1, se.fit, FUN = "/"))
  ret <- apply(absDev, 2L, max)
  attr(ret, "pred") <- pred
  ret
}
