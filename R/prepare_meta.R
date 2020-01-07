#' Prepare a model for meta analysis by removing all original data
#'
#' @param fit A model fitted using \code{mgcv::gam} or \code{mgcv::gamm}.
#' @param path Optional path in which to save the object.
#'
#' @return An object of class metagam, which contains only the aggregate data from the model fit.
#' @export
#'
prepare_meta <- function(fit, path = NULL){

  # Find the minimum and maximum of each variable
  var_ranges <- purrr::map(fit$model, function(x){
    if(is.numeric(x) | is.integer(x)){
      list(class = class(x), min = min(x), max = max(x))
    } else {
      list(class = class(x), unique = unique(x))
    }
    })


  # Save only the parts of the fit which contain aggregated data
  obj <- list(
    aic = fit$aic,
    assign = fit$assign,
    boundary = fit$boundary,
    call = fit$call,
    cmX = fit$cmX,
    coefficients = fit$coefficients,
    control = fit$control,
    converged = fit$converged,
    deviance = fit$deviance,
    df.null = fit$df.null,
    df.residual = fit$df.residual,
    edf = fit$edf,
    edf1 = fit$edf1,
    edf2 = fit$edf2,
    family = fit$family,
    formula = fit$formula,
    gcv.ubre = fit$gcv.ubre,
    iter = fit$iter,
    method = fit$method,
    mgcv.conv = fit$mgcv.conv,
    min.edf = fit$min.edf,
    nsdf = fit$nsdf,
    null.deviance = fit$null.deviance,
    offset = fit$offset,
    optimizer = fit$optimizer,
    pred.formula = fit$pred.formula,
    pterms = fit$pterms,
    R = fit$R,
    rank = fit$rank,
    reml.scale = fit$reml.scale,
    rV = fit$rV,
    scale = fit$scale,
    scale.estimated = fit$scale.estimated,
    sig2 = fit$sig2,
    smooth = fit$smooth,
    sp = fit$sp,
    terms = fit$terms,
    var.summary = fit$var.summary,
    Ve = fit$Ve,
    Vp = fit$Vp,
    Vc = fit$Vc,
    var_ranges = var_ranges
  )
  # Add a check to make sure Xu is gone
  # Something like obj$smooth <- map(obj$smooth, function(x) {x$Xu <- NULL; return(x)})
  class(obj) <- c("metagam", class(fit))

  if(!is.null(path)) saveRDS(obj, file = path)

  return(obj)
}
