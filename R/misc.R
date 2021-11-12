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
  apply(absDev, 2L, max)

}



get_sim_ci <- function(m, cohort_estimates, xvars, method, grid){
  dd <- merge(cohort_estimates, m, by = "model")
  dd$weight <- dd$se * dd$crit
  dd <- eval(parse(text = paste0("split(dd, f = list(dd$", xvars, ", dd$term))")))
  res <- do.call(rbind, lapply(dd, function(x){
    m <- metafor::rma(yi = x$estimate, sei = x$weight, method = method)
    dd3 <- as.data.frame(stats::predict(m))[, c("pred", "se")]
    dd3$ci_sim_lb <- dd3$pred - dd3$se
    dd3$ci_sim_ub <- dd3$pred + dd3$se
    dd3$se <- NULL
    dd3
  }))
  eval(parse(text = paste0("res$", xvars, "<- grid$", xvars)))

  res
}


get_meta_sim_ci <- function(models, alpha_seq, masd_list,
                            cohort_estimates, xvars, method, grid){
  crit <- do.call(rbind, lapply(seq_along(models), get_crit_models, alpha_seq, masd_list))
  crit <- split(crit, f = crit$alpha)
  lapply(crit, get_sim_ci, cohort_estimates, xvars, method, grid)
}

get_crit <- function(a, ind, masd_list){
  stats::quantile(masd_list[[ind]], probs = 1 - a, type = 8, names = FALSE)
  }

get_crit_models <- function(ind, alpha_seq, masd_list){
  r <- unlist(lapply(alpha_seq, get_crit, ind, masd_list))
  data.frame(crit = r, alpha = alpha_seq, model = ind)
}

testfun <- function(a, models, masd_list, cohort_estimates, xvars, method, grid){
  sim_ci <- get_meta_sim_ci(models, a, masd_list,
                            cohort_estimates, xvars, method, grid)[[1]]
  max(sim_ci$ci_sim_lb) - min(sim_ci$ci_sim_ub)
}
