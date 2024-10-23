#' Check for identical rows in a data frame
#'
#'@description
#' This function takes as an input tabular data with variables corresponding to ballot-level voting patterns and returns the number of identical rows in the data frame. The purpose is to identify the number of permutations for which there are n number of duplicates.
#'
#'
#' @param data An object of class data frame
#' @param dup_num A numeric value indicating the number of identical rows to check for
#' @param logical A character value indicating the logical operator to use. Must be one of 'greater', 'equal', or 'less'
#' @param name A character value indicating the name of the column to create in the output data frame. Default is 'count'
#'
#' @details
#' There are several reasons why two people can have identical ballot patterns:
#'
#' - Random Chance: In elections where candidates are listed in a random order or where options are chosen randomly (like in some lotteries or local elections), individuals can have identical ballot patterns purely by chance.
#'
#' - Limited Choices: In elections with a limited number of candidates or options, especially in smaller or local elections, the ballot choices may be more constrained, increasing the likelihood of identical patterns.
#'
#' - Similar Preferences: Voters with similar political or social views may independently arrive at the same choices when presented with similar candidates or issues on the ballot.
#'
#' - Influence of Campaigns or Media: Voter decisions can be influenced by similar campaign messaging, media coverage, or public opinion, leading to identical voting patterns among groups of voters.
#'
#' - Voter Guides or Recommendations: Voters may consult the same voter guides, endorsements, or recommendations from trusted sources, resulting in similar ballot selections.
#'
#' - Strategic Voting: In some cases, voters may strategically choose candidates based on perceived electability or other strategic considerations, leading to similar ballot patterns.
#'
#' These factors can contribute to the occurrence of identical ballot patterns among different voters, even in diverse electoral contexts. The mere presence of identical ballot patterns does not necessarily indicate fraud or malfeasance.
#'
#' @return A numeric value indicating the number of identical rows in the data frame
#' @export
#'
#' @importFrom dplyr group_by_all tally filter
#'
#' @examples
#' data <- data.frame(a = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5),
#'                    b = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5))
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
