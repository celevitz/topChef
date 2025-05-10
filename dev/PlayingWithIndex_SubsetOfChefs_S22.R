rm(list=ls())
library(tidyverse)
library(openxlsx)
library(topChef)
library(ggplot2)
library(devtools)
#devtools::install_github("celevitz/topChef")

directory <- "/Users/carlylevitz/Documents/Data/topChef/"

challengewins <- read.csv(paste0(directory,"Top Chef - Challenge wins.csv"))%>%
  filter(series == "US")

chefdetails <- read.csv(paste0(directory,"Top Chef - Chef details.csv"))  %>%
  filter(series == "US" )

# Episode 8 of S 22
    seriesname <- "US"
    numberofelimchalls <- 8
    numberofquickfirechalls <- numberofquickfires <- 5
    seasonnumberofchoice <- 22

## Index
## Write it out here, because it calls on the Top Chef package and that's
    # not working right now
    weightedindex <- function(seriesname,seasonnumberofchoice
                              ,numberofelimchalls
                              ,numberofquickfires) {
      # 1. Set up the data
      placementdata <- topChef::chefdetails[,c("chef","series","season"
                                               ,"seasonNumber","placement")]
      placementdata <- placementdata[placementdata$series == seriesname &
                                       placementdata$seasonNumber == seasonnumberofchoice,]


      # 1a. Outcomes of the challenges
      challengewins <-
        challengewins[,names(challengewins)[!(names(challengewins) %in% "rating")] ]
      challengewins <- challengewins[challengewins$series == seriesname &
                                       challengewins$seasonNumber == seasonnumberofchoice,]

      # 1ai. combine types of challenges
      # because there could be both a SDQ & an elimination in an episode, and I
      # use episode as a row ID, I need to make sure that we keep those challenge
      # results separate from the other elimination challenges in that episode
      challengewins$episodeascharacter <- as.character(challengewins$episode)
      challengewins$episodeascharacter[nchar(challengewins$episodeascharacter)==1] <-
        paste0("0",challengewins$episodeascharacter[nchar(challengewins$episodeascharacter)==1])

      challengewins$challID <- paste0(challengewins$episodeascharacter,"a_",challengewins$challengeType)
      challengewins$challID[challengewins$challengeType == "Quickfire Elimination"] <-
        paste0(challengewins$episodeascharacter[challengewins$challengeType == "Quickfire Elimination"] ,"b_qe")
      challengewins$challID[challengewins$challengeType == "Sudden Death Quickfire"] <-
        paste0(challengewins$episodeascharacter[challengewins$challengeType == "Sudden Death Quickfire"] ,"b_sdq")
      challengewins$challID[challengewins$challengeType == "Quickfire"] <-
        paste0(challengewins$episodeascharacter[challengewins$challengeType == "Quickfire"] ,"a_qf")
      challengewins$challID[challengewins$challengeType == "Elimination"] <-
        paste0(challengewins$episodeascharacter[challengewins$challengeType == "Elimination"] ,"c_elim")

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
      challnum_temp <- unique(challengewins[, c("season","seasonNumber"
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
      statsbynumberofchalls <- merge(challengewins,placementdata,by=c("series"
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

      # 2c. NEW PART OF CODE: flag just the chefs who were in the most recent
      # challenge you're looking at (whether it's elimination or QF)
      tempDataToFlagEliminatedPlayers <- challkeep %>%
        arrange(challID)
      challengeflag <- tempDataToFlagEliminatedPlayers[max(nrow(
                                            tempDataToFlagEliminatedPlayers))
                                      ,"challID"]
        statsbynumberofchalls$stillincomp <- 0
        statsbynumberofchalls$stillincomp[statsbynumberofchalls$challID ==
                                            challengeflag] <- 1
        statsbynumberofchalls <- statsbynumberofchalls %>%
          group_by(seasonNumber,series,season,chef) %>%
          mutate(stillincomp = max(stillincomp))

      # 2d. drop unnecessary variables
      statsbynumberofchalls <- statsbynumberofchalls[,c("season","seasonNumber"
                                                      ,"series","challengeType"
                                                      ,"chef","outcome"
                                                    ,"placement"
                    # NEW CODE
                                                    ,"stillincomp")]


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
                                            ,statsbynumberofchalls$placement
                                            # NEW CODE
                                            ,statsbynumberofchalls$stillincomp)
                                         ,FUN=sum)
      names(statsbynumberofchalls) <- c("chef","season","seasonNumber","series"
                                        ,"challengeType","outcome","placement"
                                        # NEW CODE
                                        ,"stillincomp"
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
                                                 ,"placement"
                                                 # NEW CODE
                                                 ,"stillincomp"),
                                       direction = "wide")
      statsbynumberofchalls <- reshape(statsbynumberofchalls,
                                       timevar = "outcome",
                                       idvar = c("chef","season","seasonNumber"
                                                 ,"series","placement"
                                                 # NEW CODE
                                                 ,"stillincomp"
                                                 ),
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

      # 3c. How many times are they at jduges table?
      statsbynumberofchalls$judgestableQF <-statsbynumberofchalls$Quickfire.WIN+
        statsbynumberofchalls$Quickfire.HIGH+statsbynumberofchalls$Quickfire.LOW
      statsbynumberofchalls$judgestableElim <-
        statsbynumberofchalls$Elimination.WIN+
        statsbynumberofchalls$Elimination.HIGH+
        statsbynumberofchalls$Elimination.LOW +
        statsbynumberofchalls$Elimination.OUT
      statsbynumberofchalls$judgestableElimPercent <-
        statsbynumberofchalls$judgestableElim/numberofelimchalls

      # 4. get the index
      statsbynumberofchalls$indexWeight <-
        statsbynumberofchalls$Elimination.WIN*7+
        statsbynumberofchalls$Elimination.HIGH*3 -
        statsbynumberofchalls$Elimination.LOW*3-
        statsbynumberofchalls$Elimination.OUT*7 +
        statsbynumberofchalls$Quickfire.WIN*4+
        statsbynumberofchalls$Quickfire.HIGH*2-
        statsbynumberofchalls$Quickfire.LOW*2

      # NEW CODE
      # Rank of all chefs
      # Rank of chefs in the competition at the time

        statsbynumberofchalls <- statsbynumberofchalls[order(
                        statsbynumberofchalls$indexWeight,decreasing=TRUE),]
        row.names(statsbynumberofchalls) <- NULL
        statsbynumberofchalls$OverallRank <- as.numeric(row.names(statsbynumberofchalls))

        statsbynumberofchalls <- statsbynumberofchalls[order(
          statsbynumberofchalls$stillincomp
          ,statsbynumberofchalls$indexWeight,decreasing=TRUE),]
        row.names(statsbynumberofchalls) <- NULL
        statsbynumberofchalls$RankOfThoseStillIn <- as.numeric(row.names(statsbynumberofchalls))
        statsbynumberofchalls$RankOfThoseStillIn[statsbynumberofchalls$stillincomp == 0] <- NA


      statsbynumberofchalls
    }



#############################################################################
## Now play around: Restaurant Wars -- what were the standings heading into it?
    rightbeforeRW <- weightedindex("US",1,6,7) %>%
      bind_rows(weightedindex("US",2,9,10)) %>%
      bind_rows(weightedindex("US",3,7,8)) %>%
      bind_rows(weightedindex("US",4,10,11)) %>%
      bind_rows(weightedindex("US",5,9,8)) %>%
      bind_rows(weightedindex("US",6,8,9)) %>%
      bind_rows(weightedindex("US",7,8,9)) %>%
      bind_rows(weightedindex("US",8,6,6)) %>%
      bind_rows(weightedindex("US",9,7,7)) %>%
      bind_rows(weightedindex("US",10,9,9)) %>%
      bind_rows(weightedindex("US",11,9,5)) %>%
      bind_rows(weightedindex("US",12,8,4)) %>%
      bind_rows(weightedindex("US",13,10,5)) %>%
      bind_rows(weightedindex("US",14,8,4)) %>%
      bind_rows(weightedindex("US",15,8,5)) %>%
      bind_rows(weightedindex("US",16,3,4)) %>%
      bind_rows(weightedindex("US",17,7,6)) %>%
      bind_rows(weightedindex("US",18,7,7)) %>%
      bind_rows(weightedindex("US",19,7,6)) %>%
      bind_rows(weightedindex("US",20,9,6)) %>%
      bind_rows(weightedindex("US",21,7,5)) %>%
      bind_rows(weightedindex("US",22,7,6)) %>%
    select(season,seasonNumber,chef,placement,stillincomp,indexWeight
           ,OverallRank,RankOfThoseStillIn) %>%
    mutate(eliminatedInRW = case_when(
      (seasonNumber == 1 & chef %in% c("Miguel M.")) |
        (seasonNumber == 2 & chef %in% c("Michael M.")) |
        (seasonNumber == 3 & chef %in% c("Tre W.")) |
        (seasonNumber == 4 & chef %in% c("Dale T.")) |
        (seasonNumber == 5 & chef %in% c("Radhika D.")) |
        (seasonNumber == 6 & chef %in% c("Laurine W.")) |
        (seasonNumber == 7 & chef %in% c("Kenny G.")) |
        (seasonNumber == 8 & chef %in% c("Marcel V.")) |
        (seasonNumber == 9 & chef %in% c("Ty-Lor B.")) |
        (seasonNumber == 10 & chef %in% c("Kristen K.")) |
        (seasonNumber == 11 & chef %in% c("Sara J.")) |
        (seasonNumber == 12 & chef %in% c("Keriann vR.")) |
        (seasonNumber == 13 & chef %in% c("Phillip F.L.")) |
        (seasonNumber == 14 & chef %in% c("Katsuji T.")) |
        (seasonNumber == 15 & chef %in% c("Claudette Z.-W.")) |
        (seasonNumber == 16 & chef %in% c("Nini N.","Pablo L")) |
        (seasonNumber == 17 & chef %in% c("Kevin G.")) |
        (seasonNumber == 18 & chef %in% c("Sara H.")) |
        (seasonNumber == 19 & chef %in% c("Jackson K.")) |
        (seasonNumber == 20 & chef %in% c("Nicole Gomes")) |
        (seasonNumber == 21 & chef %in% c("Kaleena Bliss")) ~ "yes"
      ,TRUE ~ "no"
    ))

  chefseliminatedinRW <- rightbeforeRW %>%
    filter(eliminatedInRW == "yes") %>%
    mutate(placement = as.numeric(placement)) %>%
    select(!c(stillincomp,eliminatedInRW,indexWeight))

  table(chefseliminatedinRW$placement)
  table(chefseliminatedinRW$OverallRank)
  table(chefseliminatedinRW$RankOfThoseStillIn)

  chefseliminatedinRW %>%
    filter(RankOfThoseStillIn <=2)

  rightbeforeRW %>%
    filter(seasonNumber == 22) %>%
    mutate(placement = ifelse(placement == 1.5,NA,placement)
           ,stillincomp = ifelse(stillincomp == 1,"yes","no")
           ,stillincomp = ifelse(chef == "Henry Lu","no",stillincomp)
           ,RankOfThoseStillIn = ifelse(chef == "Henry Lu",NA,RankOfThoseStillIn)) %>%
    arrange(RankOfThoseStillIn,OverallRank) %>%
    select(chef,stillincomp,indexWeight,RankOfThoseStillIn,OverallRank)


########################################################################
## Season 22 episode 8


  allseasons <- weightedindex("US",1,numberofelimchalls,numberofquickfires)
  for (season in seq(2,22,1)) {
    allseasons <- rbind(allseasons
                        ,weightedindex("US",season,numberofelimchalls
                                       ,numberofquickfires))
  }

  allseasons %>%
    filter(Elimination.WIN >=3) %>%
    select(chef,season,seasonNumber,placement,Elimination.WIN
           ,Elimination.HIGH,Quickfire.WIN,Quickfire.HIGH
           ,indexWeight) %>%
    mutate(placement = ifelse(chef == "Tristen Epps",NA,placement)) %>%
    arrange(desc(Elimination.WIN),desc(indexWeight))




