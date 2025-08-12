#' @title Calculate One Season's Chef's Weighted Scores (Index)
#'
#' @description Calculates the Index score for each person within a season of
#'              Top Chef.
#'
#' @param seriesname Values can be: US, US Masters, Canada
#' @param seasonnumberofchoice Integer of the season number within the series
#' @param numberofelimchalls Number of elimination challenges you want to
#'                            include in the index. Must be greater than 0
#' @param numberofquickfires Number of quickfire challenges you want to include
#'                          in the index. Must be greater than 0.
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

weightedindex <- function(seriesname,seasonnumberofchoice,numberofelimchalls
                          ,numberofquickfires) {
  # 1. Set up the data
    placementdata <- topChef::chefdetails[,c("chef","series","season"
                                             ,"seasonNumber","placement")]
    placementdata <- placementdata[placementdata$series == seriesname &
                           placementdata$seasonNumber == seasonnumberofchoice,]


    # 1a. Outcomes of the challenges
    challengewinsnoLCK <-
      topChef::challengewinsnoLCK[,names(topChef::challengewinsnoLCK)[!(names(topChef::
                                              challengewinsnoLCK) %in% "rating")] ]
    challengewinsnoLCK <- challengewinsnoLCK[challengewinsnoLCK$series == seriesname &
                           challengewinsnoLCK$seasonNumber == seasonnumberofchoice,]

    # 1ai. combine types of challenges
    # because there could be both a SDQ & an elimination in an episode, and I
    # use episode as a row ID, I need to make sure that we keep those challenge
    # results separate from the other elimination challenges in that episode
    challengewinsnoLCK$episodeascharacter <- as.character(challengewinsnoLCK$episode)
    challengewinsnoLCK$episodeascharacter[nchar(challengewinsnoLCK$episodeascharacter)==1] <-
      paste0("0",challengewinsnoLCK$episodeascharacter[nchar(challengewinsnoLCK$episodeascharacter)==1])

    challengewinsnoLCK$challID <- paste0(challengewinsnoLCK$episodeascharacter,"a_",challengewinsnoLCK$challengeType)
    challengewinsnoLCK$challID[challengewinsnoLCK$challengeType == "Quickfire Elimination"] <-
      paste0(challengewinsnoLCK$episodeascharacter[challengewinsnoLCK$challengeType == "Quickfire Elimination"] ,"b_qe")
    challengewinsnoLCK$challID[challengewinsnoLCK$challengeType == "Sudden Death Quickfire"] <-
      paste0(challengewinsnoLCK$episodeascharacter[challengewinsnoLCK$challengeType == "Sudden Death Quickfire"] ,"b_sdq")
    challengewinsnoLCK$challID[challengewinsnoLCK$challengeType == "Quickfire"] <-
      paste0(challengewinsnoLCK$episodeascharacter[challengewinsnoLCK$challengeType == "Quickfire"] ,"a_qf")
    challengewinsnoLCK$challID[challengewinsnoLCK$challengeType == "Elimination"] <-
      paste0(challengewinsnoLCK$episodeascharacter[challengewinsnoLCK$challengeType == "Elimination"] ,"c_elim")

    challengewinsnoLCK$challengeType[challengewinsnoLCK$challengeType %in%
                                 c("Quickfire Elimination"
                                   ,"Sudden Death Quickfire")] <- "Elimination"

    # 1aii. Exclude the uncommon challenge types
    challengewinsnoLCK <- challengewinsnoLCK[!(challengewinsnoLCK$challengeType %in%
                                       c("Battle of the Sous Chefs"
                                         ,"Qualifying Challenge")),]

    # 1aiii. clean up outcomes: consolidate
    challengewinsnoLCK$outcome[challengewinsnoLCK$outcome %in% c("High","HiGH")] <-
      "HIGH"
    challengewinsnoLCK$outcome[grepl("LOW",challengewinsnoLCK$outcome)] <- "LOW"
    challengewinsnoLCK$outcome[challengewinsnoLCK$outcome %in%
                            c("DISQUALIFIED","RUNNER-UP","WITHDREW") |
                            grepl("OUT",challengewinsnoLCK$outcome) ] <- "OUT"
    challengewinsnoLCK$outcome[challengewinsnoLCK$outcome %in% c("DIDN'T COMPETE") |
                            grepl("N/A",challengewinsnoLCK$outcome) |
                            grepl("QUALIFIED",challengewinsnoLCK$outcome) ] <- "IN"
    challengewinsnoLCK$outcome[challengewinsnoLCK$outcome %in% c("WINNER")] <- "WIN"

    # 1b. need to consecutively number each challenge of each challenge type
    challnum_temp <- unique(challengewinsnoLCK[, c("season","seasonNumber"
                                              ,"challengeType","episode"
                                              ,"episodeascharacter","challID")])
    challnum_temp <- challnum_temp[order(challnum_temp$seasonNumber
                                         ,challnum_temp$episode
                                         ,challnum_temp$challID
                                         ,desc(challnum_temp$challengeType)),]

      # not all challenges that we're looking at start in episode 1
      # and, some episode 1s don't have a quickfire
        # order of eliminations
        elimorder <- unique(challnum_temp[challnum_temp$challengeType == "Elimination",c("challengeType","episode","challID")])
        row.names(elimorder) <- NULL
        elimorder$count <- row.names(elimorder)
        elimorder$challengeType <- "Elimination"
        elimorder$seasonNumber <- seasonnumberofchoice

        # order of quickfires
        qforder <- unique(challnum_temp[challnum_temp$challengeType == "Quickfire",c("challengeType","episode","challID")])
        row.names(qforder) <- NULL
        qforder$count <- row.names(qforder)
        qforder$challengeType <- "Quickfire"
        qforder$seasonNumber <- seasonnumberofchoice

        challnum <- rbind(elimorder,qforder)

        # if an elimination and quickfire don't happen in ep 1
        # need to add a row of quickfire
        # if the first episode of a quickfire is after the first episode of an elim,
        # then make the count 0 until it is the episode of the first quickfire
        firstelim_ep <- min(challnum$episode[challnum$challengeType == "Elimination"])
        firstqf_ep <- min(challnum$episode[challnum$challengeType == "Quickfire"])

        for (tempcount in seq(firstelim_ep, firstqf_ep,1)) {
          if (tempcount < firstqf_ep) {
            newrow <- data.frame(cbind("Quickfire",tempcount
                            ,paste0("0",as.character(tempcount),"a_qf")
                            ,0,seasonnumberofchoice
                          ) )
            names(newrow) <- names(challnum)
            challnum <- rbind(challnum,newrow )
          }
        }

    # 1bi. keep just the challenges that meet the criteria
    # update: in case the numbers are vastly different (e.g., 10 elim challs & 0 Qfs)
            # need to instead of having: an OR statement about keeping things if the
            # count is less than or = to the number of elim challs select OR
            # count is less than or = to the # of qf challs
            # instead going to append the datasets together

    challnum$count <- as.numeric(challnum$count)

    challkeep <- rbind(challnum[challnum$count <= numberofelimchalls &
                             challnum$challengeType == "Elimination" &
                               !(is.na(challnum$count)),]
                  ,challnum[(challnum$count <= numberofquickfires &
                               challnum$challengeType == "Quickfire" &
                               !(is.na(challnum$count))) ,] )

    ## 2. get the number of wins, losses, highs, etc. by chef
     statsbynumberofchalls <- merge(challengewinsnoLCK,placementdata,by=c("series"
                                               ,"season","seasonNumber","chef"))
     statsbynumberofchalls <- merge(statsbynumberofchalls,challkeep,by=
                                      c("seasonNumber","challengeType"
                                        ,"episode","challID"))

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



