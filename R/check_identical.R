#' Title: Check for identical rows in a data frame
#'
#' @param data An object of class data frame
#'
#' @return A numeric value indicating the number of identical rows in the data frame
#' @export
#'
#' @importFrom dplyr group_by_all tally filter
#'
#' @examples
check_identical <- function(data) {

  if (!is.data.frame(data)) {
    stop("data must be of class data.frame")
  }

  output <- data %>%
    group_by_all() %>%
    tally(name = "count") %>%
    filter(count > 1)

  print(nrow(output))

}
