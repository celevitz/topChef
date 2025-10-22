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
  filter(series == "US" & inCompetition == TRUE) %>%
  ## Who ever had an advantage? Who ever had immunity?
    mutate(everimm = ifelse(immune == TRUE,1,0)
           ,everadv = ifelse(!(is.na(advantage)),1,0)
            ,advTF = ifelse(!(is.na(advantage)),TRUE,FALSE)) %>%
    group_by(series,season,chef) %>%
    mutate(everimm = max(everimm,na.rm=T)
           ,everadv = max(everadv,na.rm=T)
  ## did they win?
        ,win = ifelse(outcome %in% c("WIN","WINNER"),1,0)
  ## reclassify challenge types
        , challengeType = ifelse(challengeType %in% c("Elimination"
                            ,"Quickfire Elimination","Sudden Death Quickfire")
          ,"Elimination",challengeType
        )   ) %>%
  ## Did anyone have immunity in that episode?
  ungroup() %>% group_by(series,season,seasonNumber,episode,challengeType) %>%
  mutate(immunityoffered = max(ifelse(immune == TRUE,1,0),na.rm=T))

## When immunity was offered, how did it affect odds of winning?
immoffereddata <- challdata %>% filter(immunityoffered == 1)

summary(glm(immoffereddata$win ~ immoffereddata$immune+immoffereddata$advTF))

immoffereddata %>%
  group_by(immune,win) %>%
  summarise(n=n()) %>%
  ungroup() %>% group_by(immune) %>%
  mutate(N=sum(n)
         ,percent=n/N)

immoffereddata %>%
  group_by(advTF,win) %>%
  summarise(n=n()) %>%
  ungroup() %>% group_by(advTF) %>%
  mutate(N=sum(n)
         ,percent=n/N)

## How did people who had immunity at some point in the season do
## when they had immunity versus when they didn't?
elim <- challdata %>%
  filter(challengeType %in% "Elimination" &
           everimm %in% 1)

  elim %>%
    ungroup() %>%
    group_by(win,immune) %>%
    summarise(n=n())

  table(elim$immune,elim$win)
  chisq.test(elim$immune,elim$win)

## Advantages?
  adv <- challdata %>%
    filter(challengeType %in% "Elimination" &
             everadv %in% 1)

  adv %>%
    ungroup() %>%
    group_by(win,advTF) %>%
    summarise(n=n())

  table(adv$advTF,adv$win)
  chisq.test(adv$advTF,adv$win)


























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
