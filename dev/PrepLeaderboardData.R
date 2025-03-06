## Prepping leaderboard data for Datawrapper
## Bring all the data sources together (rewards, challenge stats, scores)
## Each row will be one chef-season (i.e., chefs who are in multiple seasons
##    will have multiple rows)

rm(list=ls())

library(tidyverse)
directory <- "/Users/carlylevitz/Documents/Data/topChef/"

# Bring in data
chefdetails_raw <- read.csv(paste0(directory,"Top Chef - Chef details.csv"))
challengewins_raw <- read.csv(paste0(directory,"Top Chef - Challenge wins.csv"))
challengedescriptions_raw<- read.csv(paste0(directory
                                    ,"Top Chef - Challenge descriptions.csv"))
rewards_raw<- read.csv(paste0(directory,"Top Chef - Rewards.csv"))
rewards_raw <- rewards_raw %>% filter(series == "US")
judges_raw<- read.csv(paste0(directory,"Top Chef - Guest judges.csv"))
episodeinfo_raw<-read.csv(paste0(directory,"Top Chef - Episode information.csv"))

## Create unique identifiers for:
##    Each chef-season
##    Each challenge (add on info about team or not)
chefdetails <- chefdetails_raw %>%
  mutate(seasonnumberasstring = ifelse(seasonNumber<10
                                       ,paste0("0",as.character(seasonNumber))
                                       ,as.character(seasonNumber))
    ,chefkey = paste(series,seasonnumberasstring,season,name,sep="-")) %>%
  select(name,series,season,seasonnumberasstring,chefkey,chef,placement) %>%
  filter(series == "US")

challengewins <- challengewins_raw %>%
  full_join(challengedescriptions_raw %>%
      select(series,season,seasonNumber,episode,challengeType,outcomeType)) %>%
  mutate(episodeasstring = ifelse(episode<10
                                  ,paste0("0",as.character(episode))
                                  ,as.character(episode))
         ,seasonnumberasstring = ifelse(seasonNumber<10
                                        ,paste0("0",as.character(seasonNumber))
                                        ,as.character(seasonNumber))
        ,challengekey = paste(series,season,seasonnumberasstring
                              ,as.character(episodeasstring),challengeType,outcomeType,
                              sep="-")
    # condense categories of challenge types & outcomes
   ,challengeTypeCondensed = ifelse(challengeType %in% c("Quickfire Elimination"
                                                     ,"Sudden Death Quickfire"
                                                     ,"Quickfire elimination")
                                    ,"Elimination",challengeType)
   ,outcomeTopBottom = case_when(
       outcome %in% c("DISQUALIFIED","OUT","RUNNER-UP","LOW") ~ "BOTTOM"
       ,outcome %in% c("WIN","WINNER","HIGH") ~ "TOP"
       ,TRUE ~ NA)) %>%
  full_join(chefdetails %>%
             select(series,season,seasonnumberasstring,name,chef,chefkey)) %>%
  filter(inCompetition == TRUE &
           challengeTypeCondensed %in% c("Elimination","Quickfire") &
          series == "US") %>%
  select(!c(series,season,seasonNumber,inCompetition,challengeType,rating))

## count how many rewards total and then Keep just the $ and that
  rewards <- rewards_raw %>%
    # drop the no winner ones
    filter(chef != "No winner" ) %>%
    left_join(chefdetails %>% select(series,season,seasonnumberasstring,chef,chefkey)) %>%
    # how many rewards they won
    group_by(series,season,seasonnumberasstring,chef,chefkey) %>%
    mutate(rewardswon = n()) %>%
    group_by(series,season,seasonnumberasstring,chef,chefkey,rewardswon) %>%
    # how much money they won
    filter(rewardType %in% c( "Money", "Money & prize")) %>%
    separate_wider_delim(reward," ",names= c("money", "b")
                         ,too_few = "align_start",too_many="drop") %>%
    separate_wider_delim(money,".",names= c("money", "c")
                         ,too_few = "align_start",too_many="drop") %>%
    select(!c(b,c)) %>%
    mutate(money = as.numeric(gsub(",","",gsub("\\$","",money)))) %>%
    summarise(moneywon=sum(money))

## Number of individual elimination challenges

## Challenge statistics
  challengestats <- chefdetails %>% left_join(
    # number competed in:
    challengewins %>%
      group_by(chefkey,challengeTypeCondensed) %>%
      summarise(competedin=n()) %>%
      pivot_wider(names_from=challengeTypeCondensed,values_from=competedin) %>%
      rename(EliminationCompetedIn=Elimination,QuickfireCompetedIn=Quickfire)
    ) %>% left_join(

    # number WON
    challengewins %>%
      filter(outcome %in% c("WIN","WINNER")) %>%
      group_by(chefkey,challengeTypeCondensed) %>%
      summarise(won=n()) %>%
      pivot_wider(names_from=challengeTypeCondensed,values_from=won) %>%
      rename(EliminationWon=Elimination,QuickfireWon=Quickfire)
    ) %>% left_join(

    # number individual wins
    challengewins %>%
      filter(outcome %in% c("WIN","WINNER") & outcomeType == "Individual") %>%
      group_by(chefkey,challengeTypeCondensed) %>%
      summarise(won=n()) %>%
      pivot_wider(names_from=challengeTypeCondensed,values_from=won) %>%
      rename(EliminationWonIndiv=Elimination,QuickfireWonIndiv=Quickfire)
    ) %>% left_join(
    # number individual elimination challenges competed in
      challengewins %>%
        filter(outcomeType == "Individual" &
                 challengeTypeCondensed == "Elimination" &
                 !(outcome %in% c("DIDN'T COMPETE"))) %>%
        group_by(chefkey) %>%
        summarise(numberindividualelimchalls=n())
    ) %>% left_join(
    # number top
    challengewins %>%
      filter(outcomeTopBottom %in% "TOP") %>%
      group_by(chefkey,challengeTypeCondensed) %>%
      summarise(top=n()) %>%
      pivot_wider(names_from=challengeTypeCondensed,values_from=top) %>%
      rename(EliminationTop=Elimination,QuickfireTop=Quickfire)
    ) %>% left_join(
    # number bottom
    challengewins %>%
      filter(outcomeTopBottom %in% "BOTTOM") %>%
      group_by(chefkey,challengeTypeCondensed) %>%
      summarise(top=n()) %>%
      pivot_wider(names_from=challengeTypeCondensed,values_from=top) %>%
      rename(EliminationBottom=Elimination,QuickfireBottom=Quickfire)
    ) %>% left_join(
    # number judges table
    challengewins %>%
      filter(outcomeTopBottom %in% c("TOP","BOTTOM") ) %>%
      group_by(chefkey,challengeTypeCondensed) %>%
      summarise(top=n()) %>%
      pivot_wider(names_from=challengeTypeCondensed,values_from=top) %>%
      rename(EliminationBottom=Elimination,QuickfireBottom=Quickfire)
    ) %>% left_join(
    # number episodes won
    challengewins %>%
      filter(outcome %in% c("WIN","WINNER")) %>%
      group_by(episode,chefkey) %>%
      summarise(n=n()) %>%
      filter(n>1) %>%
      group_by(chefkey) %>%
      summarise(episodeswon=n())
    ) %>% left_join(
    # number of times won immunity
    challengewins %>%
      filter(immune == TRUE) %>%
      group_by(chefkey) %>%
      summarise(immunities=n())
    ) %>%
  # win rates
    mutate(EliminationWinRate=EliminationWon/EliminationCompetedIn
           ,QuickfireWinRate = QuickfireWon/QuickfireCompetedIn)

## advantages??
  # not yet coded
## Bring all data together
  alldata <- chefdetails %>%
    left_join(challengestats) %>%
    left_join(rewards) %>%
    filter(series %in% "US")

  # overall win rate
  for (var in names(alldata)[8:length(alldata)]) {
    alldata[is.na(alldata[,var]),var] <- 0
  }

  alldata <- alldata %>%
    mutate(OverallWinRate = (EliminationWon + QuickfireWon)/(EliminationCompetedIn+QuickfireCompetedIn))

write.csv(alldata
          ,paste0(directory,"Top Chef - Full Individual Stats.csv")
          ,row.names=FALSE)



### GOATs
  goats <- alldata %>%
    filter((OverallWinRate>=.26 |
             EliminationTop >= 7 |
             episodeswon >=2) & EliminationCompetedIn > 3
             ) %>%
    arrange(desc(OverallWinRate),desc(EliminationTop)) %>%
    select(!c(series,chefkey,chef,EliminationWonIndiv,QuickfireWonIndiv
              ,QuickfireTop,EliminationBottom,QuickfireBottom
              ,immunities,rewardswon,moneywon
              ,EliminationWon,QuickfireWon))

  goats <- goats[,c("name","season","seasonnumberasstring","placement"
                    ,"OverallWinRate","EliminationWinRate","EliminationCompetedIn"
                    ,"EliminationTop","QuickfireWinRate","QuickfireCompetedIn"
                    ,"episodeswon")]

  goats <- goats %>%
    group_by(name,season,seasonnumberasstring) %>%
    mutate(temp1 = ifelse(OverallWinRate >=.26,1,0)
           ,temp2 = ifelse(EliminationTop >= 7,1,0)
           ,temp3 = ifelse(episodeswon >=2,1,0)
           ,numbercriteriamet = sum(temp1,temp2,temp3)) %>%
    select(!c(temp1,temp2,temp3))

  goats %>% filter(numbercriteriamet == 2) %>% select(name,season,OverallWinRate,EliminationTop,episodeswon,numbercriteriamet) %>% view()
  goats %>% filter(numbercriteriamet == 3) %>% select(name,season,OverallWinRate,EliminationTop,episodeswon,numbercriteriamet) %>% view()


  write.csv(goats
            ,paste0(directory,"Top Chef - GOATs.csv")
            ,row.names=FALSE)

### Winners
  winners <- alldata %>%
    filter(placement==1) %>%
    arrange(desc(EliminationWon),desc(QuickfireWon)) %>%
    mutate(individualwinrate = EliminationWonIndiv/EliminationWon
           ,indivwinrateoutofindivchalls = EliminationWonIndiv/numberindividualelimchalls
           ,elimTopRate=EliminationTop/EliminationCompetedIn
           ,elimBottomRate=EliminationBottom/EliminationCompetedIn
           ,quickfireTopRate = QuickfireTop/QuickfireCompetedIn
           ,quickfireBottomRate = QuickfireBottom/QuickfireCompetedIn) %>%
    select(!c(series,season,chefkey,chef,placement,QuickfireWonIndiv
              ,rewardswon,immunities,EliminationWonIndiv))

  winners <- winners[,c("name","seasonnumberasstring"
                        ,"EliminationWon","individualwinrate","EliminationTop"
                        ,"EliminationBottom","EliminationCompetedIn"
                        ,"QuickfireWon","QuickfireTop","QuickfireBottom"
                        ,"QuickfireCompetedIn","OverallWinRate"
                        ,"EliminationWinRate","indivwinrateoutofindivchalls"
                        ,"QuickfireWinRate"
                        ,"elimTopRate","elimBottomRate"
                        ,"quickfireTopRate","quickfireBottomRate"
                        ,"episodeswon","moneywon")]


  write.csv(winners
            ,paste0(directory,"Top Chef - Winners.csv")
            ,row.names=FALSE)


## How many people were in more than 1 season?
  chefdetails_raw %>%
    group_by(name) %>%
    filter(series == "US") %>%
    summarise(firstseason=min(seasonNumber)
              ,n=n()) %>%
    filter(n>1) %>%
    arrange(firstseason,name) %>%
    print(n=50)
  # which season produced the most multi-season chefs?
  chefdetails_raw %>%
    group_by(name) %>%
    filter(series == "US") %>%
    summarise(firstseason=min(seasonNumber)
              ,n=n()) %>%
    filter(n>1) %>%
    ungroup() %>%
    group_by(firstseason) %>%
    summarise(n=n())





