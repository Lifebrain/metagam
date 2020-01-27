#' Domination plot
#' 
#' Plots the (relative) contribution of the individual GAMs to each data point
#' on a given axis. It shows whether and how parts of the axis are dominated
#' by certain individual GAMs.
#' 
#' @export
#' 
plot_domination <- function(x, axis, relative=TRUE, title="Domination Plot")
{  
  
  # position = fill gives percent stacked bar,
  # otherwise fo position = stacked
  if (isTRUE(relative)) {
    position = "fill"
  } else {
    position = "stacked"
  }
  
  
  # TODO: determine width parameter automatically
  gp <- ggplot2::ggplot(x$cohort_fits, 
                        ggplot2::aes_string(x=axis,y="1/se^2",fill="cohort",width=.01))+
    ggplot2::geom_bar(position=position,stat="identity")+
    ggplot2::theme_minimal()+
    viridis::scale_fill_viridis(discrete = T) +
    ggplot2::ggtitle(title)+
    ggplot2::ylab("Relative Influence")
  
  return(gp)
  
}
