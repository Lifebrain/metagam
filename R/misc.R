#' Get maximum of the absolute standard deviations
#'
#' This code is based on Dr. Gavin Simpson's blog post https://fromthebottomoftheheap.net/2016/12/15/simultaneous-interval-revisited/.
#'
#' @param mod Object of class "striprawdata", returned from \code{\link{strip_rawdata}}.
#' @param newdat Grid of values over which to create a simultaneous confidence interval.
#' @param nsim Number of Monte Carlo samples.
#' @param term Character vector of smooth terms.
#'
#' @return A vector of maxima of absolute standard deviations.
#' @export
#' @keywords internal
#'
getmasd <- function(mod, newdat, nsim, term){
  Vb <- stats::vcov(mod)
  inds <- grep(gsub(")", "\\)", gsub("(", "\\(", term, fixed = TRUE), fixed = TRUE), colnames(Vb))

  Vb <- Vb[inds, inds]
  pred <- stats::predict(mod, newdat, se.fit = TRUE, terms = term, type = "iterms")
  se.fit <- pred$se.fit

  BUdiff <- mgcv::rmvn(nsim, mu = rep(0, nrow(Vb)), V = Vb)
  Cg <- stats::predict(mod, newdat, type = "lpmatrix")[, inds]
  simDev <- Cg %*% t(BUdiff)
  absDev <- abs(sweep(simDev, 1, se.fit, FUN = "/"))
  apply(absDev, 2L, max)

}

# Find the terms from each model
find_terms <- function(models, type, terms){
  unique(lapply(models, function(model) {
    if(type == "iterms"){
      if(!is.null(terms)){
        model_terms <- model$term_list[names(model$term_list) %in% terms]
        if(!all(check <- terms %in% names(model_terms))){
          stop(paste("Unknown term", names(model_terms)[!check], "requested"))
        }
      } else {
        model_terms <- model$term_list[sort(names(model$term_list))[[1]]]
        if(length(model_terms) == 0){
          stop("No terms")
        }
      }
    } else {
      model_terms <- model$term_list
    }

    data.frame(
      xvars = unlist(model_terms), terms = names(model_terms)
    )
  }))[[1]]
}


create_grid <- function(models, term_list, grid_size){
  xvars <- unlist(unique(lapply(models, function(model) names(model$var.summary))))
  ranges <- lapply(models, function(model){
    ret <- lapply(xvars, function(xvar){
      v <- model$var.summary[[xvar]]
      if(is.numeric(v)){
        range(v)
      } else {
        v[[1]]
      }
    })
    names(ret) <- xvars
    ret
  })
  grid <- lapply(xvars, function(xvar){
    val <- unlist(lapply(ranges, function(range) range[[xvar]]))
    if(xvar %in% term_list$xvars){
      seq(from = min(val), to = max(val), length.out = grid_size)
    } else if(is.numeric(val)){
      min(val)
    } else {
      val[[1]]
    }
  })
  names(grid) <- xvars
  grid
}


extract_model_fits <- function(model, term_list, grid){
  res <- if(type == "iterms"){
    Map(function(term, xvar){
      newdat <- create_newdat(xvar = xvar, grid = grid, type = type)

      pred <- stats::predict(model, newdata = newdat, type = type,
                             se.fit = TRUE, terms = term)
      if(intercept) pred$fit <- pred$fit + attr(pred, "constant")
      newdat$fit <- as.numeric(pred$fit)
      newdat$se.fit <- as.numeric(pred$se.fit)
      newdat
    },
    term = term_list$terms,
    xvar = term_list$xvars)
  } else if(type %in% c("link", "response")){
    newdat <- create_newdat(term_list = term_list, grid = grid, type = type)
    pred <- stats::predict(model, newdata = newdat, type = type, se.fit = TRUE)
    newdat$fit <- pred$fit
    newdat$se.fit <- pred$se.fit
    newdat
  }
  attr(res, "s.table") <- model$s.table
  res
}

fit_meta_models <- function(fits, se.fits, method){
  fits <- do.call(cbind, fits)
  se.fits <- do.call(cbind, se.fits)
  fits <- split(fits, seq_len(nrow(fits)))
  se.fits <- split(se.fits, seq_len(nrow(se.fits)))

  Map(function(fit, se.fit){
    metafor::rma(yi = fit, sei = se.fit, method = method)
  }, fit = fits, se.fit = se.fits)
}

get_predictions <- function(meta_models){
  ret <- lapply(meta_models, function(x){
    pred <- stats::predict(x)
    data.frame(
      estimate = pred$pred,
      se = pred$se
    )
  })
  do.call(rbind, ret)
}

create_newdat <- function(xvar = NULL, term_list = NULL, grid, type){
  if(type == "iterms"){
    newdat <- lapply(grid[!names(grid) %in% xvar], function(x) x[[1]])
    eval(parse(text = paste0("newdat$", xvar, "<- grid$", xvar)))
    as.data.frame(newdat)
  } else {
    newdat <- lapply(grid[!names(grid) %in% term_list$xvars], function(x) x[[1]])
    for(xvar in term_list$xvars) eval(parse(text = paste0("newdat$", xvar, "<- grid$", xvar)))
    expand.grid(newdat)
  }
}

get_sim_ci <- function(models, cohort_estimates, masd_list,
                       term, xvar, ci_alpha){
  sim_cohort_estimates <- Map(function(model, cohort_estimate, masd){
    cohort_estimate$crit.width <- quantile(masd, probs = 1 - ci_alpha, type = 8) * cohort_estimate$se.fit
    cohort_estimate
  },
  model = models,
  cohort_estimate = lapply(cohort_estimates, function(x) x[[term]]),
  masd = masd_list
  )

  sim_ci_models <- fit_meta_models(lapply(sim_cohort_estimates, function(x) x[["fit"]]),
                                   lapply(sim_cohort_estimates, function(x) x[["crit.width"]]),
                                   method)
  cbind(
    create_newdat(xvar = xvar, grid = grid, type = type),
    get_predictions(sim_ci_models))
}


simulate <- function(term_list, grid, models, nsim, cohort_estimates, ci_alpha){
  Map(function(term, xvar){
    newdat <- create_newdat(xvar = xvar, grid = grid, type = "iterms")
    masd_list <- lapply(models, getmasd, newdat, nsim, term)
    meta_sim_ci <- get_sim_ci(models, cohort_estimates, masd_list, term, xvar, ci_alpha)

    optfun <- function(sig){
      dd <- get_sim_ci(models, cohort_estimates, masd_list, term, xvar, sig)
      max(dd$estimate - dd$se) - min(dd$estimate + dd$se)
    }

    pval <- if(sign(optfun(1/nsim)) == sign(optfun(.99))){
      paste("<", 1/nsim)
    } else {
      tryCatch(
        {as.character(stats::uniroot(optfun, interval = c(0, 1))$root)},
        error = function(e) NA_character_
      )
    }

    list(meta_sim_ci = meta_sim_ci, pval = pval)
  },
  term = term_list$terms,
  xvar = term_list$xvars)
}
