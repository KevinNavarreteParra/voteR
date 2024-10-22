#' Title: Check for identical rows in a data frame
#'
#' @description
#' This function takes as an input tabular data with variables corresponding to ballot-level voting patterns and returns the number of identical rows in the data frame. The purpose is to identify the number of permutations for which there are $n$ number of duplicates.
#'
#'
#' @param data An object of class data frame
#' @param dup_num A numeric value indicating the number of identical rows to check for
#' @param logical A character value indicating the logical operator to use. Must be one of 'greater', 'equal', or 'less'
#' @param name A character value indicating the name of the column to create in the output data frame. Default is 'count'
#'
#' @return A numeric value indicating the number of identical rows in the data frame
#' @export
#'
#' @importFrom dplyr group_by_all tally filter
#'
#' @examples
#' data <- data.frame(a = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5),
#'                   b = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5))
#'
#' check_identical(data, 2, "greater")
check_identical <- function(data, dup_num, logical, name = "count") {

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
      tally(name = name) %>%
      filter(name > dup_num)
  } else if (logical == "equal") {
    output <- data %>%
      group_by_all() %>%
      tally(name = name) %>%
      filter(name == dup_num)
  } else if (logical == "less") {
    output <- data %>%
      group_by_all() %>%
      tally(name = name) %>%
      filter(name < dup_num)
  } else {
    stop("logical must be one of 'greater', 'equal', or 'less'")
  }

  print(nrow(output))

}
