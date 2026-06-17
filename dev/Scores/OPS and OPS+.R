
rm(list=ls())
library(tidyverse)

directory <- "/Users/carlylevitz/Documents/Data/topChef/"

# What do you want to do?
updatedata <-  "no"
finalfouranalysis <- "yes"

if (updatedata == "yes") {
# Bring in data
challengewins <- read.csv(paste0(directory,"Top Chef - Challenge wins.csv"))
placement <- read.csv(paste0(directory,"Top Chef - Chef details.csv"))
placement <- placement %>%
  select(chef,name,seasonNumber,series,placement) %>%
  mutate(placement = as.numeric(placement))

moneywon <-  read.csv(paste0(directory,"Top Chef - Rewards.csv")) %>%
  # keep just money
  filter(grepl("Money",rewardType) & series == "US") %>%
  group_by(series,season,seasonNumber,chef) %>%
  mutate(reward = as.numeric(reward)) %>%
  summarise(moneyearned = sum(reward))

temp <- challengewins %>%
  filter(challengeType %in% c("Elimination","Quickfire","Quickfire Elimination"
                              ,"Sudden Death Quickfire") &
           inCompetition == "TRUE" &
         # remove the didn't compete rows
           outcome != "DIDN'T COMPETE"
         ) %>%
  mutate(outcome2 = case_when(
    outcome %in% c("HIGH","WIN","WINNER") ~ "Top"
    ,outcome %in% c("DISQUALIFIED","LOW","OUT","RUNNER-UP","WITHDREW")~"Bottom"
    ,TRUE ~ "in")
    ,In = ifelse(outcome2 %in% c("in","Top"),1,0)
    ,Top = ifelse(outcome2 %in% c("Top"),1,0)
    ,Bottom =  ifelse(outcome2 %in% c("Bottom"),1,0)
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
    ## Elim bottom
    ,EB = ifelse(challengeType %in% c("Elimination","Quickfire Elimination"
                                      ,"Sudden Death Quickfire") &
                   Bottom == 1,1,0)
    ## QF bottom
    ,QB = ifelse(challengeType %in% c("Quickfire","Optional Quickfire") &
                   Bottom == 1,1,0)
    ) %>%
  group_by(series,season,seasonNumber,chef) %>%
  summarise(## C = challenges in
          C=n()
        ,In=sum(In),Top = sum(Top)
        ,E = sum(E), Q = sum(Q), ET = sum(ET), QT = sum(QT)
        ,EB = sum(EB), QB = sum(QB)
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
  select(series,season,seasonNumber,chef,C,E,Q,ET,QT,EW,QW,EB,QB,W
         ,NBP,TOW,NPT#,NPTcomparedToAverage
         ,NPTplus,NPTplus2) %>%
  filter(series == "US") %>%
  left_join(placement) %>%
  left_join(moneywon) %>%
  ungroup() %>%
  relocate(chef,.before=seasonNumber) %>%
  relocate(placement,.after=seasonNumber) %>%
  mutate(rank = dense_rank(desc(NPTplus))) %>%
  # how many episodes did they win?
  left_join(challengewins %>%
              filter(grepl("WIN",outcome) & inCompetition == TRUE) %>%
              group_by(season,seasonNumber,series,episode,chef) %>%
              summarise(wonepisode=n()) %>%
              filter(wonepisode>=2) %>%
              ungroup() %>% group_by(season,seasonNumber,series,chef) %>%
              summarise(episodeswon=n())
              ) %>%
  mutate(episodeswon = ifelse(is.na(episodeswon),0,episodeswon)) %>%
  ## what about LCK data?
  left_join(challengewins %>% filter(challengeType == "Last Chance Kitchen") %>%
              mutate(lckcomp = 1,lckwin = ifelse(grepl("WIN",outcome),1,0)
                     ,lckwinner = ifelse(outcome %in% "WINNER",1,0)) %>%
              group_by(season,seasonNumber,chef) %>%
              summarise(lckcomp=sum(lckcomp),lckwin = sum(lckwin)
                        ,lckwinner = sum(lckwinner))) %>%
    mutate(lckcomp = ifelse(is.na(lckcomp),0,lckcomp)
           ,lckwin = ifelse(is.na(lckwin),0,lckwin)
           ,lckwinner = ifelse(is.na(lckwinner),0,lckwinner))

write.csv(temp %>% mutate(series = "US")
          ,paste0(directory,"NPTplus.csv"),row.names=FALSE)


fordatawrapper <- temp %>%
  select(!NPTplus2) %>%
  filter(placement <= 6) %>%
  arrange(desc(NPTplus),desc(C),desc(W))
write.csv(fordatawrapper
          ,paste0(directory,"NPTplusForDataWrapperPlacement6orBetter.csv"),row.names=FALSE)
} else { print("not going to update the data") }

###########################################################################
## Top 4
###########################################################################
if (finalfouranalysis == "yes") {
f4data <- read.csv(paste0(directory,"NPTplus.csv")) %>%
  filter(placement <= 4)

## Average NPT+ and rank of NPT+ of the final four by season
f4byseason <- f4data %>%
    ## By Chef: Elimination TOW and QF TOW
    mutate(eTOW = (ET/E)
           ,qTOW = (QT/Q)) %>%
    group_by(seasonNumber) %>%
    summarise(meanNPTplus = mean(NPTplus)
              ,meanRank = mean(rank)
              ## By season: average TOW, NBP, W of the F4
              ,meanTOW = mean(TOW)
              ,meanNBP = mean(NBP)
              ,meanW = mean(W)
              ## By Season: Elimination TOW and QF TOW
              ,meaneTOW = mean(eTOW)
              ,meanqTOW = mean(qTOW)
              )

    ## What are the quartiles for each key measure?
    sapply(f4byseason, function(x) quantile(x, probs = seq(0, 1, 1/4)))

    ## how many of the F4 have NPT+ below 100?
    f4data %>%
      mutate(below100 = ifelse(NPTplus < 100, 1, 0)
             ,above200 = ifelse(NPTplus >= 200, 1, 0)) %>%
      group_by(seasonNumber) %>%
      summarise(below100 = sum(below100)
                ,above200 = sum(above200)) %>%
      print(n=50)



} else { print("not going to do final four analysis")}
