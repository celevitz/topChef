## Purpose: did we lose front runners at RW?
## Date: 2026-04-26
## Author: Carly Levitz
## methods note: not going to use final NPT+. Instead, am going to look at only
##    the challenges that occurred prior to RW

rm(list=ls())
library(tidyverse)

directory <- "/Users/carlylevitz/Documents/Data/topChef/"
rwchalls <- read.csv(paste0(directory,"Top Chef - Challenge descriptions.csv"))
challwins <- read.csv(paste0(directory,"Top Chef - Challenge wins.csv"))

# get the challenge data prior to RW
beforerw <- rwchalls %>%
  # Keep just the US RW episodes
  filter(series == "US" & !is.na(restaurantWarEliminated)) %>%
  select(season,seasonNumber,series,episode) %>%
  rename(rwep=episode) %>%
  # merge on the challenge wins, then just keep those that are for RW episode
  #   or before. Don't include the elim chall in the challenge count
  full_join(challwins %>%
              filter(series == "US")) %>%
  mutate(dropelimchall = ifelse(grepl("Elimination",challengeType) &
                                  episode == rwep ,"drop","keep")) %>%
  filter(episode <= rwep & dropelimchall == "keep") %>%
  # drop unneeded variables
  select(!c(rwep,immune,advantage,rating,dropelimchall))

# Calculate stats on their performance
stats <- beforerw %>%
 filter(challengeType %in% c("Elimination","Quickfire","Quickfire Elimination"
                              ,"Sudden Death Quickfire") &
           inCompetition == "TRUE" &
           # remove the didn't compete rows
           outcome != "DIDN'T COMPETE"  ) %>%
  mutate(outcome2 = case_when(
    outcome %in% c("HIGH","WIN","WINNER") ~ "Top"
    ,outcome %in% c("DISQUALIFIED","LOW","OUT","RUNNER-UP","WITHDREW")~"Bottom"
    ,TRUE ~ "in")
    ,In = ifelse(outcome2 %in% c("in","Top"),1,0)
    ,Top = ifelse(outcome2 %in% c("Top"),1,0)
    ## E = eliminations in
    ,E = ifelse(challengeType %in% c("Elimination","Quickfire Elimination"
                                     ,"Sudden Death Quickfire"),1,0)
    ## Q = quickfires in
    ,Q = ifelse(challengeType %in% c("Quickfire","Optional Quickfire"),1,0)
    ## ET = eliminations top or win
    ,ET = ifelse(challengeType %in% c("Elimination","Quickfire Elimination"
                                      ,"Sudden Death Quickfire") &
                   Top == 1,1,0)
    ## QT = quickfires top or win
    ,QT = ifelse(challengeType %in% c("Quickfire","Optional Quickfire") &
                   Top == 1,1,0)
    ## EW = elimination win
    ,EW = ifelse(challengeType %in% c("Elimination","Quickfire Elimination"
                                      ,"Sudden Death Quickfire") &
                   outcome %in% c("WIN","WINNER"),1,0)
    ## QW = quickfire win
    ,QW = ifelse(challengeType %in% c("Quickfire","Optional Quickfire") &
                   outcome %in% c("WIN","WINNER"),1,0)
    ## Win percent
    ,W = ifelse(outcome %in% c("WIN","WINNER"),1,0)
  ) %>%
  group_by(series,season,seasonNumber,chef) %>%
  summarise(## C = challenges in
    C=n()
    ,In=sum(In),Top = sum(Top)
    ,E = sum(E), Q = sum(Q), ET = sum(ET), QT = sum(QT)
    ,EW = sum(EW),QW = sum(QW),W=sum(W) )%>%
  mutate(NBP = round(In/C,3)
         ,TOW = round(Top/C,3)
         ,NPT = NBP + TOW
         #win percent
         ,W = W/C) %>%
  ungroup() %>%
  group_by(series) %>%
  mutate(Average = mean(NPT)
         ,NPTcomparedToAverage = NPT/Average*100
         ,AvgNBP = mean(NBP)
         ,AvgTOW = mean(TOW)
         ,NPTplus = round(100*((NBP/AvgNBP) + (TOW/AvgTOW) - 1),0) #100 x (OBP/lgOBP + SLG/lgSLG - 1)
  ) %>%
  # What are people's rank within their season at this point?
  ungroup() %>% group_by(seasonNumber) %>%
  mutate(rank = dense_rank(desc(NPTplus))) %>%
  # drop unneeded variables
  select(!c(Average,NPTcomparedToAverage,AvgNBP,AvgTOW))

## Who was eliminated in RW, and what was their NPT+ and rank?
eliminated <- rwchalls %>%
  # Keep just the US RW episodes
  filter(series == "US" & !is.na(restaurantWarEliminated)) %>%
  select(season,seasonNumber,episode) %>%
  rename(rwep=episode) %>%
  # See who was liminated in the RW episode
  full_join(challwins %>%
              filter(series == "US" & outcome == "OUT")) %>%
  filter(episode == rwep) %>%
  # drop unneeded variables
  select(!c(rwep,episode,inCompetition,immune,advantage,challengeType,outcome
            ,rating)) %>%
  # merge on their stats
  left_join(stats %>%
              select(season,seasonNumber,chef,C,EW,QW,NBP,TOW,NPTplus,rank)) %>%
  arrange(NPTplus,seasonNumber,rank)




