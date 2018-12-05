#' Merging two dataframes
#'
#' @param x a data frame.
#' @param y another data frame.
#' @return A dataframe with \code{x} and \code{y} getting rid of duplicate columns.
#' @examples
#' #MER()
#' @export

MER <- function (x = nycflights13::weather, y = nycflights13::flights) {

  df <- merge(x,y, no.dups = TRUE)
  out <- df

  return(out)

}
