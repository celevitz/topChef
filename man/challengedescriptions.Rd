% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\docType{data}
\name{challengedescriptions}
\alias{challengedescriptions}
\title{challengedescriptions}
\format{
This data frame contains the following columns:
\describe{
  \item{\code{season}}{Name of season}
  \item{\code{seasonNumber}}{Season number}
  \item{\code{series}}{Top Chef US (listed as US); Top Chef US Masters
            (listed as US Masters); Top Chef Canada (listed as Canada)}
  \item{\code{episode}}{Episode number}
  \item{\code{challengeType}}{Challenge type: qualifying challenge,
             elimination, quickfire, sudden death quickfire, quickfire
             elimination, battle of the sous chefs}
  \item{\code{outcomeType}}{Is the challenge run as a team or as an
             individual?}
  \item{\code{challengeDescription}}{Description of the challenge}
  \item{\code{shopTime}}{If they go shopping, how long do they have?
             Unit is minutes}
  \item{\code{shopBudget}}{If they go shopping, what is their budget?
             Unit is dollars unless otherwise specified.}
  \item{\code{prepTime}}{If they have prep time, how long do they have?
             Unit is minutes}
  \item{\code{cookTime}}{How long they have to cook (in minutes)}
  \item{\code{productPlacement}}{List of products promoted in the
             challenge, other than the usual series-wide product placement.
             Will be blank if none were mentioned}
  \item{\code{advantage}}{If an advantage is offered to the winner of the
           challenge, it will be listed here: e.g., Immunity, choosing
           a protein in the elimination challenge, choosing your team in
           the elimination challenge. Will be blank if none were mentioned.}
  \item{\code{lastChanceKitchenWinnerEnters}}{If someone comes in from
      Last Chance Kitchen at this challenge, their name will be listed here.
      Will be blank for all other challenges.}
  \item{\code{restaurantWarWinner}}{Role played by the winner of
             restaurant wars: Executive Chef, Front of House, the full team,
             Line Cook, Roles Rotated, or No one won. Will only have values
             for Restaurant War episodes.}
  \item{\code{restaurantWarEliminated}}{Role played by the Chef eliminated
            after restaurant wars: Executive Chef, Front of House, the full
            team, Line Cook, Roles Rotated. Will only have values for
            Restaurant War episodes.}
  \item{\code{didJudgesVisitWinningTeamFirst}}{Categorical variable of
  which team was shown serving the judges first. Will only have values for
  Restaurant Wars episodes.}
}
}
\source{
\url{https://en.wikipedia.org/wiki/Top_Chef}
}
\usage{
data(challengedescriptions)
}
\description{
A dataset containing information about each challenge that the
Chefs compete in
}
\examples{
library(dplyr)
library(tidyr)
challengedescriptions \%>\%
   group_by(series,season,outcomeType) \%>\%
   summarise(n=n()) \%>\%
   pivot_wider(names_from=outcomeType,values_from=n)
}
\keyword{datasets}
