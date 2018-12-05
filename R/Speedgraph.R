#' Graphing delays from NYC airpots
#'
#' @param data a data set.
#' @para color1,2,3 are different colors for each of the 3 variable which graph is grouped by.
#' @return A graph displaying departure delay (min) and speed for each of the three airports in NYC
#' @examples
#' Speedgraph()
#' @export

Speedgraph  <- function (data = MER(), color1= "forestgreen", color2 = "firebrick", color3 = "dodgerblue")  {

  library(dplyr)
  library(ggplot2)

  df2 <- data %>% group_by(origin) %>% mutate(speed =distance/air_time*60)
  data.frame(df2)

  out <- ggplot(df2, aes(dep_delay, speed , group = origin, color = origin)) +
    geom_point(size = 1, alpha = 0.5) +
    xlab("Departure Delay (minutes)")+
    ylab("Speed (MPH)")+
    facet_wrap(~origin, scales = "free_x")+
    scale_color_manual(values=c(color1, color2, color3))

  return(out)

}
