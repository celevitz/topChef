## Carly Levitz
## June 9, 2024
## Purpose: stats about the F4 in each season
## stats PRIOR to the F4 challenge
## remove qualifying challenges
## outcome consolidation:
##  winner = win (this shouldn't be a thing, given we're not including
##          anything F4 onward)
##  OUT == Low.
## diving by the # of challenges, not # of challenges each individual
##  participated in. I.e., people from LCK will be downgraded...

rm(list=ls())

library(tidyverse)
library(topChef)
library(ggplot2)

finalfours <- topChef::chefdetails %>%
  filter(series=="US" &
           (placement <=4 ) | (seasonNumber == 21 & is.na(placement))) %>%
  select(series,season,seasonNumber,chef,placement)

numberofchallseachseasonbeforeF4 <- topChef::challengewins %>%
  right_join(topChef::episodeinfo %>%
               filter(series == "US" & nCompetitors >= 5 &
                        !(is.na(nCompetitors)))) %>%
  select(series,season,seasonNumber,challengeType,episode) %>%
  # remove the reunions and qualifying challenges
  filter(!(is.na(challengeType)) & challengeType != "Qualifying Challenge") %>%
  distinct() %>%
  # going to consolidate all challenge types to get overall rates
  group_by(series,season,seasonNumber) %>%
  summarise(nchall=n())

challoutcomes <- topChef::challengewins %>%
  # keep just the episodes prior to final four
  right_join(topChef::episodeinfo %>%
               filter(series == "US" & nCompetitors >= 5 &
                        !(is.na(nCompetitors)))) %>%
  # remove the reunions and qualifying challenges
  filter(!(is.na(challengeType)) & challengeType != "Qualifying Challenge") %>%
  # consolidate outcomes
  mutate(outcome = case_when(outcome == "WINNER"~"WIN"
                             ,outcome == "OUT" ~ "LOW"
                             ,TRUE ~ outcome)) %>%
  # keep just the ones we want stats on: high, low, win
  filter(inCompetition == TRUE & outcome %in% c("HIGH","LOW","WIN")) %>%
  ## going to consolidate all challenge types to get overall rates
  group_by(series,season,seasonNumber,chef,outcome) %>%
  summarise(n=n())

## Bring data together to get the %s in each category

  f4stats <- challoutcomes %>%
    full_join(numberofchallseachseasonbeforeF4) %>%
    right_join(finalfours) %>%
    group_by(series,season,seasonNumber,chef,outcome) %>%
    mutate(percent = n/nchall) %>%
    select(!n) %>%
    pivot_wider(names_from=outcome,values_from=percent)

  for (var in c("HIGH","LOW","WIN")) {
    f4stats[is.na(f4stats[,var]),var] <- 0
  }

  f4stats$top <- f4stats$WIN+f4stats$HIGH


## detailed tables
  f4stats %>%
    filter(seasonNumber == 21)  %>%
    arrange(desc(WIN),desc(HIGH),LOW)

  f4stats %>%
    arrange(desc(top),desc(WIN),desc(HIGH),LOW) %>%
    filter(top > .5) %>%
    mutate(atjudgestable=top+LOW)

## # of people better than S21 people
  nrow(f4stats[f4stats$top > .429,])
  nrow(f4stats[f4stats$WIN > .238,])
  nrow(f4stats[f4stats$LOW < .0952,])

## summary by season
  summarybyseason <- data.frame(f4stats %>%
    group_by(series,season,seasonNumber) %>%
    summarise(HIGH =mean(HIGH)
              ,LOW=mean(LOW)
              ,WIN=mean(WIN)
              ,top=mean(top)) %>%
    arrange(desc(top),desc(WIN),desc(HIGH),LOW) %>%
    relocate(top,.before=HIGH) %>%
      relocate(WIN,.after=top))

  summarybyseason %>%
    arrange(WIN)


##############################################################
## Measures of variance of final fours
  # Instead of using the index, do it manually
  # This is because it won't be the same # of challenges for each season

  variances <- topChef::challengewins %>%
    right_join(topChef::episodeinfo %>%
                 filter(series == "US" & nCompetitors >= 5 &
                          !(is.na(nCompetitors)))) %>%
    select(series,season,seasonNumber,challengeType,episode,challengeType
           ,chef,outcome) %>%
    # keep just the final fours
    right_join(finalfours %>% select(!placement)) %>%
    # remove the reunions and qualifying challenges
    filter(!(is.na(challengeType)) )

  # A. Consolidate challenge types
  variances$challengeType[variances$challengeType %in%
                                c("Quickfire Elimination"
                                  ,"Sudden Death Quickfire")] <- "Elimination"

  # B. Exclude the uncommon challenge types
  variances <- variances[!(variances$challengeType %in%
                                     c("Battle of the Sous Chefs"
                                       ,"Qualifying Challenge")),]

  # C.  clean up outcomes: consolidate
  variances$outcome[variances$outcome %in% c("High","HiGH")] <-
    "HIGH"
  variances$outcome[grepl("LOW",variances$outcome)] <- "LOW"
  variances$outcome[variances$outcome %in%
                          c("DISQUALIFIED","RUNNER-UP","WITHDREW") |
                          grepl("OUT",variances$outcome) ] <- "OUT"
  variances$outcome[variances$outcome %in% c("DIDN'T COMPETE") |
                          grepl("N/A",variances$outcome) |
                          grepl("QUALIFIED",variances$outcome) ] <- "IN"
  variances$outcome[variances$outcome %in% c("WINNER")] <- "WIN"

  # D. Calculate points
  variances$points <- 0
  variances$points[variances$challengeType == "Quickfire" & variances$outcome == "WIN"] <- 4
  variances$points[variances$challengeType == "Quickfire" & variances$outcome == "HIGH"] <- 2
  variances$points[variances$challengeType == "Quickfire" & variances$outcome == "LOW"] <- -2

  variances$points[variances$challengeType == "Elimination" & variances$outcome == "WIN"] <- 7
  variances$points[variances$challengeType == "Elimination" & variances$outcome == "HIGH"] <- 3
  variances$points[variances$challengeType == "Elimination" & variances$outcome == "LOW"] <- -3
  variances$points[variances$challengeType == "Elimination" & variances$outcome == "OUT"] <- -7

  variances <- variances %>%
    group_by(series,season,seasonNumber,chef) %>%
    summarise(points=sum(points)) %>%
    ungroup() %>%
    group_by(series,season,seasonNumber) %>%
    summarise(minimum=min(points),mean=mean(points)
              ,maximum=max(points),stdev=sd(points))

  # E. Look at the data
    variances %>%
      arrange(stdev,mean) %>%
      print(n=21)

    summary(variances$minimum)
    summary(variances$mean)
    summary(variances$maximum)
    summary(variances$stdev)

  ## Check for if the scores are related to the length of the season prior to F4
    numberofepisodespriortoF4 <- topChef::challengewins %>%
      # keep just the episodes prior to final four
      right_join(topChef::episodeinfo %>%
                   filter(series == "US" & nCompetitors >= 5 &
                            !(is.na(nCompetitors)))) %>%
      # remove the reunions and qualifying challenges
      filter(!(is.na(challengeType)) & challengeType != "Qualifying Challenge") %>%
      # how many episodes does this count as?
      select(season,seasonNumber,series,episode) %>%
      group_by(season,seasonNumber,series) %>%
      distinct() %>%
      summarise(numberofepisodesbeforeF4= n())

    variances %>%
      left_join(numberofepisodespriortoF4) %>%
      ungroup() %>%
      mutate(season=paste0("S",seasonNumber,": ",season)) %>%
      select(!c(series,seasonNumber)) %>%
      arrange(stdev,numberofepisodesbeforeF4) %>%
      print(n=21)


################################################################################
## How do winners compare to the rest of the final fours, at the end of the season?
  # Chose 17 challenges as the number for the index function, cuz that's more than the most in a season

    final4stats <- weightedindex("US",1,17,17)
    for (season in seq(2,21,1)) {
      final4stats <- rbind(final4stats,weightedindex("US",season,17,17))

    }

    final4stats <- final4stats %>%
      # keep just the final 4s
      filter(placement <= 4) %>%
      # get win/high percents & low/out percents
      mutate(winhigh = (Elimination.HIGH+Quickfire.HIGH+Elimination.WIN+
                          Quickfire.WIN)/
                       (Elimination.HIGH+Quickfire.HIGH+Elimination.WIN+
                          Quickfire.WIN + Elimination.LOW + Quickfire.LOW +
                          Elimination.OUT)
             ,lowout =  (Elimination.LOW+Quickfire.LOW+Elimination.OUT)/
               (Elimination.HIGH+Quickfire.HIGH+Elimination.WIN+
                  Quickfire.WIN + Elimination.LOW + Quickfire.LOW +
                  Elimination.OUT)
             ,win = (Elimination.WIN+ Quickfire.WIN)/
               (Elimination.HIGH+Quickfire.HIGH+Elimination.WIN+
                  Quickfire.WIN + Elimination.LOW + Quickfire.LOW +
                  Elimination.OUT)
             ,high = (Elimination.HIGH+Quickfire.HIGH)/
               (Elimination.HIGH+Quickfire.HIGH+Elimination.WIN+
                  Quickfire.WIN + Elimination.LOW + Quickfire.LOW +
                  Elimination.OUT)
             ,placement=as.character(placement))



  final4stats %>%
    ggplot(aes(x=win,y=high,color=placement,shape=placement)) +
    geom_point()

  final4stats %>%
    ggplot(aes(x=win,y=winhigh,color=placement,shape=placement)) +
    geom_point()

  cor(final4stats$win,final4stats$high)
  cor.test(final4stats$win,final4stats$high)
  cor.test(final4stats$winhigh,final4stats$win)






