plot_heterogeneity <- function(x, axis, type="Q", ...)
{
  
  if (type=="p") {
  
  gp <- ggplot2::ggplot(data=x$prediction,
                ggplot2::aes_string(x=axis,y="qp"))+
   ggplot2::geom_line()+ ggplot2::scale_y_continuous(trans='log2')+
    ggplot2::geom_hline(yintercept=0.05,lty=2)+
    ggplot2::theme_minimal()+
    ggplot2::ylab("Heterogeneity (p)")
  
  } else if (type=="Q") {
    
    # TODO: øystein, please check whether this is the correct approximation
    x$prediction$z = -0.862 + sqrt(0.743 - 2.404*log(x$prediction$qp))
    x$prediction$se = x$prediction$q/x$prediction$z
  
  gp <- ggplot2::ggplot(data=x$prediction,
                        ggplot2::aes_string(x=axis,y="q"))+
    ggplot2::geom_ribbon(mapping=aes(x=x2,ymin=q-1.96*se,ymax=q+1.96*se),
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