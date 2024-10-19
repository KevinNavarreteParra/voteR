#' Title: Check for identical rows in a data frame
#'
#' @param data An object of class data frame
#' @param dup_num A numeric value indicating the number of identical rows to check for
#' @param logical A character value indicating the logical operator to use. Must be one of 'greater', 'equal', or 'less'
#'
#' @return A numeric value indicating the number of identical rows in the data frame
#' @export
#'
#' @importFrom dplyr group_by_all tally filter
#'
#' @examples
check_identical <- function(data, dup_num, logical) {

  if (!is.data.frame(data)) {
    stop("data must be of class data.frame")
  }

  if (!is.numeric(dup_num)) {
    stop("dup_num must be of class numeric")
  }

  if (!is.character(logical)) {
    stop("logical must be of class character")
  }
  if (logical == "greater") {
    output <- data %>%
      group_by_all() %>%
      tally(name = "count") %>%
      filter(count > dup_num)
  } else if (logical == "equal") {
    output <- data %>%
      group_by_all() %>%
      tally(name = "count") %>%
      filter(count == dup_num)
  } else if (logical == "less") {
    output <- data %>%
      group_by_all() %>%
      tally(name = "count") %>%
      filter(count < dup_num)
  } else {
    stop("logical must be one of 'greater', 'equal', or 'less'")
  }

  print(nrow(output))

}
