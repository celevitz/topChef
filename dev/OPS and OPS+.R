
rm(list=ls())
library(tidyverse)

directory <- "/Users/carlylevitz/Documents/Data/topChef/"

# Bring in data
challengewins <- read.csv(paste0(directory,"Top Chef - Challenge wins.csv"))
placement <- read.csv(paste0(directory,"Top Chef - Chef details.csv"))
placement <- placement %>%
  select(chef,name,seasonNumber,series,placement) %>%
  mutate(placement = as.numeric(placement))

temp <- challengewins %>%
  filter(challengeType %in% c("Elimination","Quickfire","Quickfire Elimination"
                              ,"Sudden Death Quickfire") &
           inCompetition == "TRUE" &
         # remove the didn't compete rows
           outcome != "DIDN'T COMPETE"
         ) %>%
  mutate(outcome2 = case_when(
    outcome %in% c("HIGH","WIN","WINNER") ~ "Top"
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
  # what about using the season as the league?
  group_by(series,seasonNumber) %>%
  mutate(AvgNBP = mean(NBP)
         ,AvgTOW = mean(TOW)
         ,NPTplus2 = round (100*((NBP/AvgNBP) + (TOW/AvgTOW) - 1) ,0)#100 x (OBP/lgOBP + SLG/lgSLG - 1)
  ) %>%
  select(series,season,seasonNumber,chef,C,E,Q,ET,QT,EW,QW,W
         ,NBP,TOW,NPT#,NPTcomparedToAverage
         ,NPTplus,NPTplus2) %>%
  filter(series == "US") %>%
  left_join(placement) %>%
  mutate(chef = name) %>%
  ungroup() %>%
  select(!c(series,season,name) )%>%
  relocate(chef,.before=seasonNumber) %>%
  relocate(placement,.after=seasonNumber) %>%
  mutate(rank = dense_rank(desc(NPTplus)))

write.csv(temp
          ,paste0(directory,"NPTplus.csv"),row.names=FALSE)


fordatawrapper <- temp %>%
  select(!NPTplus2) %>%
  filter(placement <= 6) %>%
  arrange(desc(NPTplus),desc(C),desc(W))
write.csv(fordatawrapper
          ,paste0(directory,"NPTplusForDataWrapperPlacement6orBetter.csv"),row.names=FALSE)

