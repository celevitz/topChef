#' @title Calculate One Season's Chef's Weighted Scores (Index)
#'
#' @description Calculates the Index score for each person within a season of
#'              Top Chef.
#'
#' @param seriesname Values can be: US, US Masters, Canada
#' @param seasonnumberofchoice Integer of the season number within the series
#' @param numberofelimchalls Number of elimination challenges you want to
#'                            include in the index
#' @param numberofquickfires Number of quickfire challenges you want to include
#'                          in the index
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
#'
#' @export
#'
#'

weightedindex <- function(seriesname,seasonnumberofchoice,numberofelimchalls
                          ,numberofquickfires) {
  # 1. Set up the data
    placementdata <- topChef::chefdetails[,c("chef","series","season"
                                             ,"seasonNumber","placement")]
    placementdata <- placementdata[placementdata$series == seriesname &
                           placementdata$seasonNumber == seasonnumberofchoice,]


    # 1a. Outcomes of the challenges
    challengewins <-
      topChef::challengewins[,names(topChef::challengewins)[!(names(topChef::
                                              challengewins) %in% "rating")] ]
    challengewins <- challengewins[challengewins$series == seriesname &
                           challengewins$seasonNumber == seasonnumberofchoice,]

    # 1ai. combine types of challenges
    challengewins$challengeType[challengewins$challengeType %in%
                                 c("Quickfire Elimination"
                                   ,"Sudden Death Quickfire")] <- "Elimination"

    # 1aii. Exclude the uncommon challenge types
    challengewins <- challengewins[!(challengewins$challengeType %in%
                                       c("Battle of the Sous Chefs"
                                         ,"Qualifying Challenge")),]

    # 1aiii. clean up outcomes: consolidate
    challengewins$outcome[challengewins$outcome %in% c("High","HiGH")] <-
      "HIGH"
    challengewins$outcome[grepl("LOW",challengewins$outcome)] <- "LOW"
    challengewins$outcome[challengewins$outcome %in%
                            c("DISQUALIFIED","RUNNER-UP","WITHDREW") |
                            grepl("OUT",challengewins$outcome) ] <- "OUT"
    challengewins$outcome[challengewins$outcome %in% c("DIDN'T COMPETE") |
                            grepl("N/A",challengewins$outcome) |
                            grepl("QUALIFIED",challengewins$outcome) ] <- "IN"
    challengewins$outcome[challengewins$outcome %in% c("WINNER")] <- "WIN"

    # 1b. need to consecutively number each challenge of each challenge type
    challnum <- unique(challengewins[,c("season","seasonNumber","challengeType"
                                        ,"episode")])
    challnum$count <- NA

    tempcount_elim <- 1
    for (e in unique(challnum$episode[challnum$challengeType ==
                                      "Elimination"])) {
      challnum$count[challnum$episode == e & challnum$challengeType ==
                       "Elimination"] <- tempcount_elim
      tempcount_elim <- tempcount_elim +1
    }

    tempcount_qf <- 1
    for (e in unique(challnum$episode[challnum$challengeType ==
                                      "Quickfire"])) {
      challnum$count[challnum$episode == e & challnum$challengeType ==
                       "Quickfire"] <- tempcount_qf
      tempcount_qf <- tempcount_qf +1
    }

    # 1bi. keep just the challenges that meet the criteria
    challkeep <- challnum[(challnum$count <= numberofelimchalls &
                             challnum$challengeType == "Elimination") |
                            (challnum$count <= numberofquickfires &
                               challnum$challengeType == "Quickfire") ,]

    ## 2. get the number of wins, losses, highs, etc. by chef
     statsbynumberofchalls <- merge(challengewins,placementdata,by=c("series"
                                               ,"season","seasonNumber","chef"))
     statsbynumberofchalls <- merge(statsbynumberofchalls,challkeep,by=
                                      c("season","seasonNumber","challengeType"
                                        ,"episode"))

     # 2a. keep just the episodes that are at or the same # of challenges
     # that have happened
     statsbynumberofchalls <- statsbynumberofchalls[!(is.na(
                                                statsbynumberofchalls$count)),]

     # 2b. drop when people were not in the competition
     statsbynumberofchalls <- statsbynumberofchalls[statsbynumberofchalls$
                                                      inCompetition == "TRUE",]

     # 2c. drop unnecessary variables
     statsbynumberofchalls <- statsbynumberofchalls[,c("season","seasonNumber"
                     ,"series","challengeType","chef","outcome","placement")]


     # 3. create summary stats by chef
     # get counts of wins, highs, and lows
     # don't combine outs & lows because we want to count those differently
     # in the index
     # for the currently airing season, they don't yet have a placement;
     # they'll get dropped in this aggregate function if they are empty;
     # fill them in for now
     statsbynumberofchalls$placement[is.na(statsbynumberofchalls$placement)] <-
       1.5
     statsbynumberofchalls$tempcount <- 1
     statsbynumberofchalls <- aggregate(statsbynumberofchalls$tempcount
                              ,by=list(statsbynumberofchalls$chef
                                       ,statsbynumberofchalls$season
                                       ,statsbynumberofchalls$seasonNumber
                                       ,statsbynumberofchalls$series
                                       ,statsbynumberofchalls$challengeType
                                       ,statsbynumberofchalls$outcome
                                       ,statsbynumberofchalls$placement)
                              ,FUN=sum)
     names(statsbynumberofchalls) <- c("chef","season","seasonNumber","series"
                                       ,"challengeType","outcome","placement"
                                       ,"count")

     # 3a. reshape the data
     # we don't need the "IN"/safe counts because we are holding constant
     # the number of challenges done
      statsbynumberofchalls <-
        statsbynumberofchalls[statsbynumberofchalls$outcome != "IN",]
      statsbynumberofchalls <- reshape(statsbynumberofchalls,
                                       timevar = "challengeType",
                                       idvar = c("chef","season","seasonNumber"
                                                 ,"series","outcome"
                                                 ,"placement"),
                                       direction = "wide")
      statsbynumberofchalls <- reshape(statsbynumberofchalls,
                                       timevar = "outcome",
                                       idvar = c("chef","season","seasonNumber"
                                                 ,"series","placement"),
                                       direction = "wide")
      names(statsbynumberofchalls) <- gsub("count.",""
                                           ,names(statsbynumberofchalls))
      statsbynumberofchalls$Quickfire.OUT <- NULL

    # 3b. if NA, replace with 0
      for (var in c(names(statsbynumberofchalls)[!(names(statsbynumberofchalls)
                               %in% c("chef","season","seasonNumber","series"
                                      ,"placement"))])) {
        statsbynumberofchalls[is.na(statsbynumberofchalls[,var]),var] <- 0
      }

    # 4. get the index
    statsbynumberofchalls$indexWeight <-
      statsbynumberofchalls$Elimination.WIN*7+
      statsbynumberofchalls$Elimination.HIGH*3 -
      statsbynumberofchalls$Elimination.LOW*3-
      statsbynumberofchalls$Elimination.OUT*7 +
      statsbynumberofchalls$Quickfire.WIN*4+
      statsbynumberofchalls$Quickfire.HIGH*2-
      statsbynumberofchalls$Quickfire.LOW*2

  statsbynumberofchalls
}



