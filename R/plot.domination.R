#' Domination plot
#' 
#' Plots the (relative) contribution of the individual GAMs to each data point
#' on a given axis. It shows whether and how parts of the axis are dominated
#' by certain individual GAMs.
#' 
#' @export
#' 
plot.domination <- function(x, axis, relative=TRUE, title="Domination Plot")
{  
  
  # position = fill gives percent stacked bar,
  # otherwise fo position = stacked
  if (isTRUE(relative)) {
    position = "fill"
  } else {
    position = "stacked"
  }
  
  
  # TODO: determine width parameter automatically
  gp <- ggplot(x$cohort_fits, aes_string(x=axis,y="1/se^2",fill="cohort",width=.01))+
    geom_bar(position=position,stat="identity")+
    theme_minimal()+scale_fill_viridis(discrete = T) +
    ggtitle(title)+
    ylab("Relative Influence")
  
  return(gp)
  
}
