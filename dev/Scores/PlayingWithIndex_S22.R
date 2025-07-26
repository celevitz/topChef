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

# Prior to the Double Elimination at 12 chefs: 4 elims and 4 QFs
    episodenumber <- 8
    numberofelimchalls <- 8
    numberofquickfirechalls <- numberofquickfires <- 6
    eliminatedchefs <- c("Anya El-Wattar","Zubair Mohajir"
                         ,"Mimi Weissenborn","Sam Olayinka"
                         ,"Ying Gao","Katianna Hong","Corwin Hemming"
                         ,"Kat Turner","Henry Lu")

## Index
## Write it out here, because it calls on the Top Chef package and that's
    # not working right now


    weightedindex <- function(seriesname,seasonnumberofchoice,numberofelimchalls
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
## Now play around -- current ranks
    allseasons <- weightedindex("US",22
                                ,numberofelimchalls,numberofquickfirechalls)
    for (sn in 1:21) {
      allseasons <- allseasons %>%
        bind_rows(weightedindex("US",sn
                                ,numberofelimchalls,numberofquickfirechalls))
    }

    allseasons$topofelims <- allseasons$Elimination.HIGH+
                                            allseasons$Elimination.WIN
    allseasons$topofqf <- allseasons$Quickfire.HIGH+allseasons$Quickfire.WIN

  # Distribution at this # of challenges
    summary(allseasons$indexWeight)
    summary(allseasons$indexWeight[allseasons$placement == 1])

    allseasons <- allseasons %>%
    mutate(quartile = case_when(indexWeight < summary(allseasons$indexWeight)[2] ~ "1. Under 25th percentile"
                          ,indexWeight >= summary(allseasons$indexWeight)[2] &
                            indexWeight < summary(allseasons$indexWeight)[3] ~
                                            "2. 25th to under 50th percentile"
                          ,indexWeight >= summary(allseasons$indexWeight)[3] &
                            indexWeight < summary(allseasons$indexWeight)[5] ~
                                              "3. 50th to under 75th percentile"
                          ,indexWeight >= summary(allseasons$indexWeight)[5] &
                            indexWeight <= summary(allseasons$indexWeight)[6] ~
                                              "4. 75th percentile or better"
                                ))

    # How many S22 chefs are in the upper quartile?
    # Four chefs: Katianna, Massimo, Tristen, Vinny
      allseasons %>%
        filter(seasonNumber == 22 & quartile == "4. 75th percentile or better") %>%
        select(chef,indexWeight)

    # How many chefs are in the top quartile? Not taking the "unique" ones
    # because there are folks on the list more than once
      table(allseasons$quartile)

    ## Placement by quartile
      ## need to first remove the placements for S22, cuz I have placeholders
      ##  for the chefs still in the comp
      allseasons <- allseasons %>%
        mutate(placement = as.numeric(placement)
               ,placement = ifelse(placement == 1.5,NA,placement))

      allseasons %>%
        group_by(quartile) %>%
        summarise(mean_placement = mean(placement,na.rm = T)
                  ,median_placement = median(placement,na.rm=T)
                  # lower is better, higher is worse
                  #,worst_placement = max(placement,na.rm=T)
                  ,best_placement = min(placement,na.rm=T))

    ## Top quartile
    allseasons %>%
      filter(quartile == "4. 75th percentile or better") %>%
      select(season,seasonNumber,chef,placement,indexWeight) %>%
      arrange(desc(indexWeight))

    ## winners at this stage and season 22 folks
    allseasons %>%
      filter(placement == 1 | seasonNumber == 22) %>%
      select(season,seasonNumber,chef,indexWeight,quartile,RankOfThoseStillIn) %>%
      arrange(desc(indexWeight))

    ## season 22 not in top half -- there hasn't been a winner from there
    allseasons %>%
      filter(seasonNumber == 22 &
               quartile %in% c("3. 50th to under 75th percentile"
                               ,"4. 75th percentile or better")) %>%
      select(chef)

    ## is index weight related to placement?
    summary(lm(allseasons$placement ~ allseasons$indexWeight))

    write.csv(allseasons %>%
                filter(seasonNumber == 22 | placement == 1 # |
                         #quartile == "4. 75th percentile or better"
                         )
              ,paste0(directory,"Scoring",numberofelimchalls,"elims"
                      ,numberofquickfires,"QFs_Winners-S22-TopQuartile.csv")
              ,row.names=FALSE)

    write.csv(allseasons %>%
                filter(quartile == "4. 75th percentile or better" &
                         indexWeight > 19)
              ,paste0(directory,"Scoring",numberofelimchalls,"elims"
                      ,numberofquickfires,"QFs_TopChefs.csv")
              ,row.names=FALSE)



#############################################################################
## Now play around --

  ## All seasons: what % of the time are people at the Judges table?
    allseasons <- weightedindex("US",22
                                ,numberofelimchalls,numberofquickfirechalls)
    for (sn in 1:21) {
      allseasons <- allseasons %>%
        bind_rows(weightedindex("US",sn
                                ,numberofelimchalls,numberofquickfirechalls))
    }
    allseasons$percentofjudgestableHigh <- (allseasons$Elimination.HIGH +
                                allseasons$Elimination.WIN)/(allseasons$Elimination.HIGH +
                               allseasons$Elimination.WIN + allseasons$Elimination.LOW +
                                   allseasons$Elimination.OUT)
    summary(allseasons$judgestableElimPercent)

    # Average ranges from 32.8% to 58.3%
    # Six seasons have at least 1 person who was at judges table 100%
    # Only 1 season had someone at 0%
    allseasons %>%
      group_by(season,seasonNumber) %>%
      summarise(mean=mean(judgestableElimPercent)
                ,median=median(judgestableElimPercent)
                ,min=min(judgestableElimPercent)
                ,max=max(judgestableElimPercent)) %>%
      arrange(desc(mean),desc(median)) %>%
      print(n=22)

    # four seasons have outliers (3, 7, 11, 22)
    allseasons %>%
    ggplot(aes(x=seasonNumber,y=judgestableElimPercent)) +
      geom_boxplot(aes(group=seasonNumber)) #+
      #geom_jitter(color="blue")

    limited <- allseasons %>%
      group_by(season,seasonNumber) %>%
      mutate(seasonaverage = mean(judgestableElimPercent,na.rm=T)) %>%
      filter((seasonNumber == 3 & judgestableElimPercent == 1 ) |
               (seasonNumber == 7 & judgestableElimPercent >=.7) |
               (seasonNumber == 11 & judgestableElimPercent >=.75) |
               (seasonNumber == 22 & judgestableElimPercent >=.7) |
               as.numeric(placement) == 1) %>%
      mutate(outlier = case_when((seasonNumber == 3 & judgestableElimPercent == 1 ) |
                                   (seasonNumber == 7 & judgestableElimPercent >=.7) |
                                   (seasonNumber == 11 & judgestableElimPercent >=.75) |
                                   (seasonNumber == 22 & judgestableElimPercent >=.7) ~ "Outlier"
                                 ,TRUE ~ "Not outlier")
             ,winner = ifelse(as.numeric(placement) == 1, "Winner","Non-winner")) %>%
      select(chef,season,seasonNumber,placement,judgestableElimPercent
             ,percentofjudgestableHigh,seasonaverage,outlier,winner)

      limited %>%
        ggplot(aes(x=seasonNumber,y=judgestableElimPercent) )+
        geom_line(aes(x=seasonNumber,y=seasonaverage)) +
        geom_text(aes(x=seasonNumber,y=judgestableElimPercent
                      ,label=chef,color=winner))

      ## Winners and S22
      winnersand22 <- allseasons %>%
        filter(as.numeric(placement) == 1 | seasonNumber == 22) %>%
        mutate(winner = ifelse(as.numeric(placement) == 1, "Winner","S22"))

      winnersand22summ <- winnersand22 %>%
        group_by(winner,judgestableElim,percentofjudgestableHigh) %>%
        summarise(n=n())

      winnersand22 %>%
        ggplot(aes(x=judgestableElim,y=percentofjudgestableHigh
               ))  +
        geom_text(aes(label=chef,alpha=.7,color=winner),cex=3) +
        ylab("Percent of time they were there for being at the top") +
        xlab("Times at judges' table out of 7 elimination challenges") +
        ggtitle("Times at judges table and reason for being there")

      winnersand22 %>%
        ggplot(aes(x=judgestableElim,y=percentofjudgestableHigh
        ))  +
        geom_jitter(aes(color=winner,fill=winner,shape=winner)) +
        ylab("Percent of time they were there for being at the top") +
        xlab("Times at judges' table out of 7 elimination challenges") +
        ggtitle("Times at judges table and reason for being there (jittered)")

      winnersand22summ %>%
        ggplot(aes(x=judgestableElim,y=percentofjudgestableHigh
                   ,color=winner,fill=winner,shape=winner,size=n))  +
        geom_point(alpha=.5) +
        ylab("Percent of time they were there for being at the top") +
        xlab("Times at judges' table out of 7 elimination challenges") +
        ggtitle("Times at judges table and reason for being there")

  ## Comapring S22 to past winners
    ## Stats about S22
    s22challstats <- weightedindex("US",22,numberofelimchalls,numberofquickfirechalls) %>%
      mutate(eliminated = ifelse(chef %in% eliminatedchefs,"Out","In the competition")) %>%
      select(!c(season,seasonNumber,series,placement)) %>%
      arrange(eliminated,desc(indexWeight))

    s22challstats <- s22challstats[,c("chef","eliminated","Quickfire.WIN"
                                      ,"Quickfire.HIGH","Quickfire.LOW"
                                      ,"Elimination.WIN","Elimination.HIGH"
                                      ,"Elimination.LOW"
                                      ,"indexWeight")]

    ## compare ranks
    currentrank <- s22challstats %>%
      arrange(eliminated,desc(indexWeight)) %>%
      select(chef,eliminated,indexWeight) %>%
      mutate(rank = row.names(s22challstats)
             ,season = 22)
      # compare to Danny and Buddha

    s21 <- weightedindex("US",21,numberofelimchalls,numberofquickfirechalls)
    row.names(s21) <- NULL
    s21 <- s21 %>%
      arrange(desc(indexWeight)) %>%
      mutate(rank = row.names(s21)
             ,season = 21) %>%
      filter(chef == "Danny Garcia" | rank == 1) %>%
      select(chef,rank,season,indexWeight,placement)

    s20 <-  weightedindex("US",20,numberofelimchalls,numberofquickfirechalls)
    row.names(s20) <- NULL
    s20 <- s20 %>%
      arrange(desc(indexWeight)) %>%
      mutate(rank = row.names(s20)
             ,season = 20) %>%
      filter(chef == "Buddha" | rank == 1) %>%
      select(chef,rank,season,indexWeight,placement)

    s19 <- weightedindex("US",19,numberofelimchalls,numberofquickfirechalls)
    row.names(s19) <- NULL
    s19 <- s19 %>%
      arrange(desc(indexWeight)) %>%
      mutate(rank = row.names(s19)
             ,season = 19) %>%
      filter(chef == "Buddha" | rank == 1) %>%
      select(chef,rank,season,indexWeight,placement)

    s18 <- weightedindex("US",18,numberofelimchalls,numberofquickfirechalls)
    row.names(s18) <- NULL
    s18 <- s18 %>%
      arrange(desc(indexWeight)) %>%
      mutate(rank = row.names(s18)
             ,season = 18) %>%
      filter(chef == "Gabe E." | rank == 1) %>%
      select(chef,rank,season,indexWeight,placement)

    currentrank %>%
      bind_rows(s21 %>% mutate(eliminated = "In the competition")) %>%
      bind_rows(s20 %>% mutate(eliminated = "In the competition")) %>%
      bind_rows(s19 %>% mutate(eliminated = "In the competition")) %>%
      bind_rows(s18 %>% mutate(eliminated = "In the competition"))

## What's the spread of scores at this # of QFs and Elims?
    allseasons <- weightedindex("US",1
                                ,numberofelimchalls,numberofquickfirechalls)
    for (season in seq(2,22,1)) {
      allseasons <- rbind(allseasons
                          ,weightedindex("US",season,numberofelimchalls
                                         ,numberofquickfirechalls))

    }

    allseasons %>%
      group_by(season,seasonNumber) %>%
      summarise(standarddeviation = sd(indexWeight,na.rm=T)
                ,differencebtwMaxAndMin = max(indexWeight,na.rm=T) -
                                          min(indexWeight,na.rm=T)
                  ) %>%
      arrange(standarddeviation) %>%
      print(n=22)


