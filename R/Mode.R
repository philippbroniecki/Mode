#' Estimate the mode.
#'
#' This package provides a function to estimate the mode from a numeric or
#' character vector.
#'
#' @param x Vector to find the mode of. A numeric or character vector.
#'
#' @export
#' @importFrom magrittr %>%
#'
#' @return The mode. A numeric or character scalar.
#' @keywords Descriptive statistics, mode
#' @examples
#' Mode(x = c(1, 1, 2, 2, 3, 4, 4, 4))

Mode <- function(x) {

  # sort frequency table in descending order and extract its names
  Mode <- base::table(... = x) %>%
    base::sort(decreasing = TRUE) %>%
    base::names()

  # x is numeric
  if ( base::is.numeric(x) ) {
    # coerce character to to numeric
    Mode <- base::as.numeric(Mode)
  }

  # return mode
  return(Mode[1])

}
