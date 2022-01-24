#' Plot between-study standard deviation
#'
#' When a random effects meta analysis has been used, this function
#' visualizes how the between-study standard deviation depends on the
#' explanatory variable.
#'
#' @param x Object returned from \code{\link{metagam}}.
#'
#' @return A \code{ggplot} object.
#' @export
#'
#' @examples
#' library("mgcv")
#' set.seed(1233)
#' shifts <- c(0, .5, 1, 0, -1)
#' datasets <- lapply(shifts, function(x) {
#'   ## Simulate data
#'   dat <- gamSim(scale = .1, verbose = FALSE)
#'   ## Add a shift
#'   dat$y <- dat$y + x * dat$x2^2
#'   ## Return data
#'   dat
#' })
#'
#' models <- lapply(datasets, function(dat){
#'   b <- gam(y ~ s(x2, bs = "cr"), data = dat)
#'   strip_rawdata(b)
#' })
#'
#' meta_analysis <- metagam(models, method = "REML")
#'
#' plot_between_study_sd(meta_analysis)
#'
#'
plot_between_study_sd <- function(x){

  terms <- names(x$meta_models)
  if(length(terms) > 1){
    stop("Currently only working for models with a single term being analyzed.")
  }

  term <- terms[[1]]
  ret <- data.frame(
    between_sd = vapply(x$meta_models[[term]]$meta_models,
                        function(x) x$tau2,
                        FUN.VALUE = numeric(1)))
  xvar <-  x$term_list[[term]][["xvars"]]
  eval(parse(text = paste0("ret$x <- x$meta_models[['", term, "']]$predictions$",
                           xvar)))

  ggplot2::ggplot(data = ret, ggplot2::aes_(x =~ x, y =~ between_sd)) +
    ggplot2::geom_line() +
    ggplot2::xlab(xvar) +
    ggplot2::ylab("Between-study standard deviation")



}
