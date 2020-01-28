#' Strip rawdata from a generalized additive model
#'
#' This function removes all individual participant data from a generalized additive
#' model object, while keeping aggregated quantities. The resulting object can be
#' shared without exposing individual participant data.
#'
#' @param model A model fitted using \code{mgcv::gam} or \code{mgcv::gamm}.
#' @param path Optional path in which to save the object as a \code{.rds} file.
#' @param save_ranges Logical specifying whether to save the ranges of each
#' variable used by the model. For numeric variables this amounts to the minimum
#' and maximum, and for factors all levels are saved.
#'
#' @details If \code{model} is of class \code{"gamm"}, only the \code{"gamm"} part of the
#' model will be kept.
#'
#' Thin plate regression splines (\code{bs='tp'} and \code{bs='ts'}) and Duchon splines \code{bs='ds'}
#' are currently not supported, since for these splines \code{mgcv}
#' requires the unique values of the explanatory variables for each smooth term for the \code{predict}
#' method to work. Future updates to this package will fix this.
#'
#' @return Model object with individual participant data removed.
#' @export
#'
#'
#'
strip_rawdata <- function(model, path = NULL, save_ranges = FALSE){

  # Vector of mgcv smooth classes currently supported
  supported_smooths <- c("pspline.smooth", "tensor.smooth", "Bspline.smooth",
                         "cp.smooth", "cr.smooth", "cyclic.smooth", "t2.smooth")

  check_smooths <- function(x, supported_smooths){
    if(!inherits(x, supported_smooths))
      stop(paste0("metagam currently supports the following splines:\n",
                  paste(supported_smooths, collapse = "\n"),
                  "\n\nSee the Details section in the documentation for strip_rawdata()."))
  }

  # Validation
  if(!(inherits(model, "gam") | inherits(model, "gamm"))){
    stop('model must be of class "gam" or "gamm".\n')
  }

  if(inherits(model, "gamm")) model <- model$gam

  purrr::walk(model$smooth, function(x){
    check_smooths(x, supported_smooths)
    if(inherits(x, c("tensor.smooth", "t2.smooth"))){
      purrr::walk(x$margin, check_smooths, supported_smooths)
    }
  })

  # Find the smooth terms, what="term" gives the name of the explanatory
  # variable ("x1") while what="label" gives the smooth ("s(x1)")
  find_smooth <- function(model, what){
    unique(purrr::flatten_chr(
      purrr::map(model$smooth, function(x) x[[what]])
    ))
  }

  # Find p-values of smooth terms
  s.table <- summary(model)$s.table

  # Save only the parts of the model which contain aggregated data
  obj <- list(
    aic = model$aic,
    assign = model$assign,
    boundary = model$boundary,
    call = model$call,
    cmX = model$cmX,
    coefficients = model$coefficients,
    control = model$control,
    converged = model$converged,
    deviance = model$deviance,
    df.null = model$df.null,
    df.residual = model$df.residual,
    edf = model$edf,
    edf1 = model$edf1,
    edf2 = model$edf2,
    family = model$family,
    formula = model$formula,
    gcv.ubre = model$gcv.ubre,
    iter = model$iter,
    method = model$method,
    mgcv.conv = model$mgcv.conv,
    min.edf = model$min.edf,
    nsdf = model$nsdf,
    null.deviance = model$null.deviance,
    offset = model$offset,
    optimizer = model$optimizer,
    pred.formula = model$pred.formula,
    pterms = model$pterms,
    R = model$R,
    rank = model$rank,
    reml.scale = model$reml.scale,
    rV = model$rV,
    scale = model$scale,
    scale.estimated = model$scale.estimated,
    sig2 = model$sig2,
    smooth = model$smooth,
    sp = model$sp,
    terms = model$terms,
    var.summary = model$var.summary,
    Ve = model$Ve,
    Vp = model$Vp,
    Vc = model$Vc,
    n = nrow(model$model),
    smooth_terms = find_smooth(model, "term"),
    smooth_labels = find_smooth(model, "label"),
    s.table = s.table
  )

  class(obj) <- class(model)

  if(!is.null(path)) {
    print(paste("Saving object to", path))
    saveRDS(obj, file = path)
  }

  return(obj)
}
