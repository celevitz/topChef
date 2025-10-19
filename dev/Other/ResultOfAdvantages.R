## What is the result of advantages, and how does it differ by adv. type?
## Carly Levitz
## 10/5/2025

rm(list=ls())

library(tidyverse)
library(openxlsx)
directory <- "/Users/carlylevitz/Documents/Data/topChef/"

challdata <- read.csv(paste0(directory
                                    ,"Top Chef - Challenge wins.csv")
                             ,header = TRUE) %>%
  filter(series == "US") %>%
  ## Who ever had an advantage? Who ever had immunity?
    mutate(everimm = ifelse(immune == TRUE,1,0)
           ,everadv = ifelse(!(is.na(advantage)),1,0)) %>%
    group_by(series,season,chef) %>%
    mutate(everimm = max(everimm,na.rm=T)
           ,everadv = max(everadv,na.rm=T)
  ## did they win?
        ,win = ifelse(outcome %in% c("WIN","WINNER"),1,0)
  ## reclassify challenge types
        , challengeType = ifelse(challengeType %in% c("Elimination"
                            ,"Quickfire Elimination","Sudden Death Quickfire")
          ,"Elimination",challengeType
        )   )

## How did people who had immunity at some point in the season do
## when they had immunity versus when they didn't?
elim <- challdata %>%
  filter(challengeType %in% "Elimination" &
           everimm %in% 1)

  elim %>%
    ungroup() %>%
    group_by()

  table(elim$immune,elim$win)

























## Prep data
  ## Bring in the data
  challengedescr <- read.csv(paste0(directory
                                  ,"Top Chef - Challenge descriptions.csv")
                           ,header = TRUE) %>%
    filter(series == "US") %>%
  select(series,season,seasonNumber,episode,challengeType,outcomeType,advantage)

  ## keep just the ones that have an advantage
  advantages <- challengedescr %>%
    filter(!(is.na(advantage))) %>%
    # to make it easier to see the words, remove "advantage in elim chall"
    mutate(advantage = gsub("Advantage: ",""
                          ,gsub("Advantage in Elimination: ",""
                          ,gsub("Advantage in elimination challenge: ",""
                            ,gsub("Advantage in Elimination challenge: ",""
                           ,gsub("Advantage in Elimination Challenge: ",""
                           ,gsub(" Advantage in Elimination Challenge: ",""
                           ,gsub(" advantage in elimination challenge: ",""
                            ,advantage)))))))) %>%
  ## categorize the advantages
    mutate(AdvantageFlag = 1
      ,Immunity = ifelse(grepl("mmunity",advantage) |
                               advantage %in% c("Winner goes straight to finale"
                                              ,"Automatic spot in the finale") |
                               grepl("sit out",advantage),1,0)
           ,ExtraTime = ifelse(grepl("minutes",advantage) ,1,0)
          ,ExtraMoney = ifelse(grepl("\\$",advantage),1,0)
           ,ChooseTeam = ifelse(grepl("choose team",advantage) |
                                  grepl("choose team to join",advantage) |
                                  grepl("choose your team",advantage),1,0)
          ,OtherTeamAdvantage = ifelse(grepl("choose team captains",advantage) |
                                     grepl("choose the team leader",advantage)
                                       ,1,0)
          ,AssignDishToOthers = ifelse(grepl("choose the icon",advantage) |
                                          grepl("choose dish",advantage) |
                                          grepl("choose course",advantage),1,0)
           ,IngredientAdvantage = ifelse(grepl("better ingredients",advantage) |
                                           grepl("choose foods",advantage) |
                                           grepl("choose fish",advantage) |
                                           grepl("choose protein",advantage)
                                         ,1,0))
  ## merge it onto the challenge data


## Analyze data
  ## of the times people had each type of advantage, how often did they win?
  ## Excluding immunity as advantage, how often did ppl stay out of the bottom?
