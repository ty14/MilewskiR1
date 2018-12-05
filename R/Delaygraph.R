#' Graphing delays from NYC airpots
#'
#' @param data a data set.
#' @para color1,2,3 are different colors for each of the 3 variable which graph is grouped by.
#' @return A graph displaying delay time and distance for each of the three airports in NYC
#' @examples
#' Delaygraph()
#' @export

Delaygraph <- function (data = MER(), color1= "forestgreen", color2 = "firebrick", color3 = "dodgerblue") {

  library(dplyr)
  library(ggplot2)

  df2 <- data %>% group_by(origin,distance) %>% summarise(Delay = mean(dep_delay, na.rm = TRUE))
  data.frame(df2)

  out <- ggplot(df2, aes(distance, Delay, group = origin, color = origin)) +
  geom_point(size=1) +
    ylab("Delay(min)")+
    xlab("Distance (miles)")+
  facet_wrap(~origin, scales = "free_x")+
    scale_color_manual(values=c(color1, color2, color3))



  return(out)

}




