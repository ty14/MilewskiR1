#' Graph which correlates 2 variable and displays correlation coefficient on graph.
#'
#' @param data a data set
#' @param x a data frame.
#' @param y another data frame.
#' @param color1,2,3 are different colors for each of the 3 variable which graph is grouped by.
#' @return A graph displaying the correlation between two variables with correlation coefficient on graph.
#' @examples
#' #correlation()
#' @export

correlation <- function (data = MER(),x = data$dep_delay, y = data$arr_delay, color1= "forestgreen", color2 = "firebrick", color3 = "dodgerblue") {
    library (ggplot2)

    CORR<- cor(x, y, use = "complete.obs",  method = "pearson")

 out<- ggplot(data, aes(x = dep_delay, y = arr_delay, color = origin)) +
      geom_point() +
      scale_x_continuous(breaks=c(0,1))+
      stat_smooth(method='lm',se=F)+
      facet_wrap(~origin, scales = "free_x") +
      geom_text(aes(label = CORR), parse = TRUE)
      scale_color_manual(values=c(color1, color2, color3))

  return(out)

}

correlation()
