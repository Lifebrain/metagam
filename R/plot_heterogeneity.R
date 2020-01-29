#' Title
#'
#' @param x metagam object
#' @param axis Which axis
#' @param term Which term
#' @param ... Other arguments to plot.
#'
#' @return plot
#' @export
#'
#'
#'
plot_heterogeneity <- function(x, axis, term, type = "Q", ...)
{

  dat <- x$meta_estimates
  dat <- dplyr::filter(dat, .data$term == !!term)
  dat <- dplyr::mutate(dat, 
                       QE = purrr::map_dbl(.data$meta_model, ~ .$QE),                       
                       QEp = purrr::map_dbl(.data$meta_model, ~ .$QEp))
  dat <- dplyr::rename_at(dat, dplyr::vars(axis), ~ "x")
  
  
    if (type=="p") {
  
  gp <- ggplot2::ggplot(data = dat,
                ggplot2::aes(x = .data$x, y= .data$QEp))+
   ggplot2::geom_line()+ ggplot2::scale_y_continuous(trans='log2')+
    ggplot2::geom_hline(yintercept=0.05,lty=2)+
    ggplot2::theme_minimal()+
    ggplot2::ylab("Heterogeneity (p)")
  
  } else if (type=="Q") {
    
    # TODO: øystein, please check whether this is the correct approximation
    dat$z = -0.862 + sqrt(0.743 - 2.404*log(.data$QEp))
    dat$se = dat$QE/dat$z
  
  gp <- ggplot2::ggplot(data=dat,
                        ggplot2::aes(x=.data$x,y=.data$QE))+
    ggplot2::geom_ribbon(mapping=aes(x=.data$x,ymin=.data$QE-1.96*.dat$se,ymax=.data$QE+1.96*.dat$se),
                         fill=viridis::viridis(4)[3],
                         col=viridis::viridis(4)[1])+
    ggplot2::geom_line()+
    ggplot2::theme_minimal()+
    ggplot2::ylab("Heterogeneity (Q)")
  
  } else {
    stop("Unknown type. Try 'Q' or 'p'.")
  }

  return(gp)
}
