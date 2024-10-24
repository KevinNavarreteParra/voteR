% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_identical.R
\name{check_identical}
\alias{check_identical}
\title{Check for identical rows in a data frame}
\usage{
check_identical(data, dup_num, logical, name = "count")
}
\arguments{
\item{data}{An object of class data frame}

\item{dup_num}{A numeric value indicating the number of identical rows to check for}

\item{logical}{A character value indicating the logical operator to use. Must be one of 'greater', 'equal', or 'less'}

\item{name}{A character value indicating the name of the column to create in the output data frame. Default is 'count'}
}
\value{
A numeric value indicating the number of identical rows in the data frame
}
\description{
This function takes as an input tabular data with variables corresponding to ballot-level voting patterns and returns the number of identical rows in the data frame. The purpose is to identify the number of permutations for which there are n number of duplicates.
}
\details{
There are several reasons why two people can have identical ballot patterns:
\itemize{
\item Random Chance: In elections where candidates are listed in a random order or where options are chosen randomly (like in some lotteries or local elections), individuals can have identical ballot patterns purely by chance.
\item Limited Choices: In elections with a limited number of candidates or options, especially in smaller or local elections, the ballot choices may be more constrained, increasing the likelihood of identical patterns.
\item Similar Preferences: Voters with similar political or social views may independently arrive at the same choices when presented with similar candidates or issues on the ballot.
\item Influence of Campaigns or Media: Voter decisions can be influenced by similar campaign messaging, media coverage, or public opinion, leading to identical voting patterns among groups of voters.
\item Voter Guides or Recommendations: Voters may consult the same voter guides, endorsements, or recommendations from trusted sources, resulting in similar ballot selections.
\item Strategic Voting: In some cases, voters may strategically choose candidates based on perceived electability or other strategic considerations, leading to similar ballot patterns.
}

These factors can contribute to the occurrence of identical ballot patterns among different voters, even in diverse electoral contexts. The mere presence of identical ballot patterns does not necessarily indicate fraud or malfeasance.
}
\examples{
data <- data.frame(a = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5),
                   b = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5))

check_identical(data, 2, "greater")
}
