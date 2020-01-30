#' metagam: Flexible meta-analysis of generalized additive models.
#'
#' The main functions in the metagam package are described below.
#'
#' @section Stripping rawdata:
#' The function \code{\link{strip_rawdata}} takes a fit produced by the mgcv
#' package and removes all individual participants data.
#'
#' @section Meta-analysis:
#' The function \code{\link{metagam}} takes a list of fits produced by
#' \code{\link{strip_rawdata}} and computes meta-analytic fits.
#'
#' @section Plotting:
#' The functions \code{\link{plot_dominance}} and \code{\link{plot_heterogeneity}}
#' can be used to study the meta-analytic fit computed by \code{\link{strip_rawdata}}.
#'
#' @docType package
#' @name metagam-package
#'
NULL
