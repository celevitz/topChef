rm(list=ls())

library(openxlsx)
library(tidyverse)
library(gt)

directory <- "/Users/carlylevitz/Documents/Data/"

chefdetails <- as_tibble(read.xlsx(paste(directory
                                     ,"TopChefData.xlsx",sep=""),sheet=1)) %>%
  filter(series == "Just Desserts")

challdescr <- as_tibble(read.xlsx(paste(directory
                                       ,"TopChefData.xlsx",sep=""),sheet=3))

challwins <- as_tibble(read.xlsx(paste(directory
                                     ,"TopChefData.xlsx",sep=""),sheet=2)) %>%
  filter(series == "Just Desserts" & inCompetition == "TRUE") %>%
  select(!c(series,dish,notes,rating)) %>%
  mutate(outcome = case_when(outcome %in% c("WIN","WINNER") ~ "WIN"
                           ,outcome %in% c("OUT","WITHDREW","RUNNER-UP") ~ "OUT"
                           ,outcome %in% c("HIGH","LOW") ~ outcome
                           ,TRUE ~ "IN")) %>%
  left_join(challdescr %>%
            select(season,seasonNumber,episode,challengeType,outcomeType)) %>%
  left_join(chefdetails %>% select(chef,placement))


## Calculate scores (TOW, NPT, ETC)
scores <- challwins %>%
  mutate(outcome2 = case_when(outcome %in% c("HIGH","WIN","WINNER") ~ "Top"
          ,outcome %in% c("DISQUALIFIED","LOW","OUT","RUNNER-UP","WITHDREW") ~ "Bottom"
          ,TRUE ~ "in")
    ,In = ifelse(outcome2 %in% c("in","Top"),1,0)
    ,Top = ifelse(outcome2 %in% c("Top"),1,0)
     ## E = eliminations in
     ,E = ifelse(challengeType %in% c("Elimination","Quickfire Elimination"
                                      ,"Sudden Death Quickfire"),1,0)
     ## Q = quickfires in
     ,Q = ifelse(challengeType %in% c("Quickfire"),1,0)
     ## ET = eliminations top or win
     ,ET = ifelse(challengeType %in% c("Elimination","Quickfire Elimination"
                                       ,"Sudden Death Quickfire") &
                    Top == 1,1,0)
     ## QT = quickfires top or win
     ,QT = ifelse(challengeType %in% c("Quickfire") & Top == 1,1,0)
     ## EW = elimination win
     ,EW = ifelse(challengeType %in% c("Elimination","Quickfire Elimination"
                                       ,"Sudden Death Quickfire") &
                    outcome %in% c("WIN","WINNER"),1,0)
     ## QW = quickfire win
     ,QW = ifelse(challengeType %in% c("Quickfire") &
                    outcome %in% c("WIN","WINNER"),1,0)
     ## Win percent
     ,W = ifelse(outcome %in% c("WIN","WINNER"),1,0) ) %>%
  group_by(season,seasonNumber,chef,placement) %>%
  summarise(## C = challenges in
    C=n()
    ,In=sum(In),Top = sum(Top)
    ,E = sum(E), Q = sum(Q), ET = sum(ET), QT = sum(QT)
    ,EW = sum(EW),QW = sum(QW),W=sum(W) ) %>%
  mutate(NBP = round(In/C,3)
         ,TOW = round(Top/C,3)
         ,NPT = NBP + TOW
         #win percent
         ,W = W/C) %>%

  ungroup() %>%
  # get NPT+
  mutate(Average = mean(NPT)
         ,NPTcomparedToAverage = NPT/Average*100
         ,AvgNBP = mean(NBP)
         ,AvgTOW = mean(TOW)
         ,NPTplus = round(100*((NBP/AvgNBP) + (TOW/AvgTOW) - 1),0) #100 x (OBP/lgOBP + SLG/lgSLG - 1)
  )



write.csv(scores %>%
            select(!c(seasonNumber,Average,NPTcomparedToAverage,AvgNBP
                      ,AvgTOW)) %>%
            relocate(chef,.before=season)
          ,paste0(directory,"topChef/JustDesserts_NPTplus.csv"),row.names=FALSE)


## are there folks who were on TCJD but were then guest judges?
chefdetails %>%
  select(chef) %>%
  left_join(as_tibble(read.xlsx(paste(directory
                                      ,"TopChefData.xlsx",sep=""),sheet=5)) %>%
              select(!c(gender,personOfColor,competedOnTC,otherShows)) %>%
              rename(chef=guestJudge)) %>%
  filter(!(is.na(season)))


