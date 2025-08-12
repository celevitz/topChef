#' @title Calculate One Season's Chefs' Weighted Scores Taking into Account
#' Number of Chefs Still in the Competition
#'
#' @description Calculates the Index score for each person within a season of
#'              Top Chef.
#'
#' @param seriesname Values can be: US, US Masters, Canada
#' @param seasonnumberofchoice Integer of the season number within the series
#' @param numberofchefsofinterest The number of chefs you want to still be in
#' thecompetition. The score will include all challenges up to and including
#' that number of chefs
#' @param scoringsystem Values can be original or modified. Modified gives
#' points for just being in the competition; original is only for how they do in
#' the challenges.
#'
#' @details The result of elimination challenges and quickfire challenges are
#'          weighted.
#'          Scoring: Elimination win = +7 points. Elimination high = +3 points.
#'                   Elimination low = -3 points. Eliminated = -7 points.
#'                   Quickfire win = +4 points. Quickfire high = +2 points.
#'                   Quickfire low = -2 points.
#'          Combines Sudden Death Quickfires with Eliminations
#'          Excludes Qualifying Challenges
#'          Holding constant the number of elimination challenges and quickfire
#'          challenges will allow comparison across seasons if you want
#'
#' @return Tibble of index score for each contestant in that season and their
#'          placement
#'
#' @importFrom stats filter
#' @importFrom stats aggregate
#' @importFrom stats reshape
#' @importFrom dplyr desc
#'
#' @export
#'
#'

ScoresThroughAGivenNumberOfChefs <- function(seriesname,seasonnumberofchoice
                                     ,numberofchefsofinterest, scoringsystem) {
  # 1. Set up the data
  challengewinsRaw <- topChef::challengewinsnoLCK
  challengewinsRaw <- challengewinsRaw[challengewinsRaw$series == seriesname &
                         challengewinsRaw$seasonNumber == seasonnumberofchoice,]
  chefdetails <- topChef::chefdetails
  chefdetails <- chefdetails[chefdetails$series == seriesname &
                             chefdetails$seasonNumber == seasonnumberofchoice,]
  chefdetails <- chefdetails[,c("series","season","seasonNumber","chef"
                                ,"placement")]

  # 2. How many chefs were in each episode?
  ## use the # of chefs with In Competition == TRUE

  numchefs <- unique(challengewinsRaw[,c("series","season","seasonNumber"
                                         ,"episode","inCompetition","chef")])
  numchefs$temp <- 0
  numchefs$temp[numchefs$inCompetition == "TRUE"] <- 1

  numchefs <- aggregate(numchefs$temp,by=list(numchefs$series,numchefs$season
                                              ,numchefs$seasonNumber
                                           ,numchefs$episode),FUN=sum,na.rm=T)
  names(numchefs) <- c("series","season","seasonNumber","episode"
                       ,"numberofchefs")

  # 3. Combine with challenge information
  ## Simplify the challenge & outcome types
  challengewins <- challengewinsRaw[,c("series","season","seasonNumber"
                                  ,"episode","chef"
                                  ,"challengeType","outcome")]
  challengewins <- merge(challengewins,numchefs)


    # remove challenge types we're not interested in
    challengewins <- challengewins[!(challengewins$challengeType %in%
                                     c("Battle of the Sous Chefs"
                                       ,"Qualifying Challenge")),]


    # simplify the challenge types & outcome types
    challengewins$challengeType[challengewins$challengeType %in%
                                c("Quickfire Elimination"
                                  ,"Sudden Death Quickfire")] <- "Elimination"
    challengewins$outcome[challengewins$outcome %in% c("WIN","WINNER")] <- "WIN"
    challengewins$outcome[challengewins$outcome %in% c("RUNNER-UP"
                                           ,"DISQUALIFIED","WITHDREW")] <- "OUT"
    challengewins$outcome[challengewins$outcome %in% c("DIDN'T COMPETE")]<-"IN"


  # 4. calculate the score
  ## how many of each outcome-challenge type combo did they get?
  ## score will be based on whether original or modified scoring system chosen
  ## only look at score up to and through the number of chefs of interest
  scoredetails <- challengewins[challengewins$numberofchefs >=
                                                      numberofchefsofinterest,]
    # no longer need the variable for number of chefs
    scoredetails$numberofchefs <- NULL

    # 4a. count the combos for each chef
    scoredetails$temp <- 1
    scoredetails <- aggregate(scoredetails$temp,FUN=sum
                              ,by=list(scoredetails$series,scoredetails$season
                                      ,scoredetails$seasonNumber
                                      ,scoredetails$chef,scoredetails$outcome
                                      ,scoredetails$challengeType))
    names(scoredetails) <- c("series","season","seasonNumber","chef","outcome"
                             ,"challengeType","n")

    # 4b. Multiply by the number of times they got each of those outcomes
    scoredetails$points[scoredetails$outcome == "WIN" &
                          scoredetails$challengeType == "Elimination"] <-
      7*scoredetails$n[scoredetails$outcome == "WIN" &
                         scoredetails$challengeType == "Elimination"]
    scoredetails$points[scoredetails$outcome == "OUT" &
                          scoredetails$challengeType == "Elimination"] <-
      -7*scoredetails$n[scoredetails$outcome == "OUT" &
                               scoredetails$challengeType == "Elimination"]
    scoredetails$points[scoredetails$outcome == "HIGH" &
                          scoredetails$challengeType == "Elimination"] <-
      3*scoredetails$n[scoredetails$outcome == "HIGH" &
                              scoredetails$challengeType == "Elimination"]
    scoredetails$points[scoredetails$outcome == "LOW" &
                          scoredetails$challengeType == "Elimination"] <-
      -3*scoredetails$n[scoredetails$outcome == "LOW" &
                               scoredetails$challengeType == "Elimination"]
    scoredetails$points[scoredetails$outcome == "WIN" &
                          scoredetails$challengeType == "Quickfire"] <-
      4*scoredetails$n[scoredetails$outcome == "WIN" &
                              scoredetails$challengeType == "Quickfire"]
    scoredetails$points[scoredetails$outcome == "HIGH" &
                          scoredetails$challengeType == "Quickfire"] <-
      4*scoredetails$points[scoredetails$outcome == "HIGH" &
                              scoredetails$challengeType == "Quickfire"]
    scoredetails$points[scoredetails$outcome == "LOW" &
                          scoredetails$challengeType == "Quickfire"] <-
      -2*scoredetails$n[scoredetails$outcome == "LOW" &
                               scoredetails$challengeType == "Quickfire"]

  # 4c. Add on points for how many episodes they were in
    if (scoringsystem == "modified") {
        additionalpoints <- unique(challengewinsRaw[,c("series","season"
                                           ,"seasonNumber","episode"
                                           ,"inCompetition","chef")])
        # keep just the episodes with the correct number of chefs
        # and just the data for when they're in the episode
        additionalpoints <- merge(additionalpoints,numchefs)
        additionalpoints <- additionalpoints[additionalpoints$numberofchefs >=
                                               numberofchefsofinterest &
                                     additionalpoints$inCompetition == "TRUE",]

        # count how many episode they are in
        additionalpoints$temp <- 1
        additionalpoints <- aggregate(additionalpoints$temp,FUN=sum
                                      ,by=list(additionalpoints$series
                                               ,additionalpoints$season
                                               ,additionalpoints$seasonNumber
                                               ,additionalpoints$chef))
        names(additionalpoints) <- c("series","season","seasonNumber","chef"
                                     ,"points")

        # Because I'm going to combine this with step 4b, add in placeholder
        # variables so that the two dataframes have the same dimensions
        additionalpoints$outcome <- additionalpoints$challengeType <-  "NA"
        additionalpoints$n <- 0

        scoredetails <- bind_rows(scoredetails,additionalpoints)

    }

  # 4d. Add up all the points
    finalscore <- aggregate(scoredetails$points,FUN = sum,na.rm=TRUE
                            ,by=list(scoredetails$series
                                     ,scoredetails$season
                                     ,scoredetails$seasonNumber
                                     ,scoredetails$chef))
    names(finalscore) <- c("series","season","seasonNumber","chef","points")

  # 5. add on placement info
    finalscore <- merge(finalscore,chefdetails)

    print(finalscore)

}



