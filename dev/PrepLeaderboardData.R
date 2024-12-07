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
  select(name,series,season,seasonnumberasstring,chefkey,chef,placement)

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
           challengeTypeCondensed %in% c("Elimination","Quickfire")) %>%
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

## advantages???

## keep only the US ones



  alldata <- chefdetails %>%
    left_join(challengestats) %>%
    left_join(rewards)

write.csv(alldata
          ,paste0(directory,"Top Chef - Data for Data Wrapper.csv")
          ,row.names=FALSE)
