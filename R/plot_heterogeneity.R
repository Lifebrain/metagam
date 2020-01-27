plot_heterogeneity <- function(x, axis, ...)
{
  
  gp <- ggplot2::ggplot(data=x$prediction,
                ggplot2::aes_string(x=axis,y="qp"))+
   ggplot2::geom_line()+ ggplot2::scale_y_continuous(trans='log2')+
    ggplot2::geom_hline(yintercept=0.05,lty=2)+
    ggplot2::theme_minimal()+
    ggplot2::ylab("Heterogeneity (p)")
  
  return(gp)
}