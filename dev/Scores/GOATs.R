rm(list=ls())
library(tidyverse)
library(openxlsx)
library(topChef)



resultsraw <- topcChef::challengewins %>%
  filter(series == "US" & seasonNumber != 21)

details <- topChef::chefdetails %>%
  filter(series == "US" & seasonNumber != 21) %>%
  select(season,seasonNumber,chef,placement,gender,personOfColor)

## simplify challenge & outcome categories
## NEED to document this when saying stats
results <- resultsraw %>%
  mutate(outcomecategory = case_when(outcome %in% c("WIN","WINNER") ~ "WIN"
                                     ,outcome %in% c("BUBBLE","DIDN'T COMPETE","QUALIFIED") ~ "IN"
                                     ,outcome %in% c("DISQUALIFIED","RUNNER-UP","WITHDREW") ~ "OUT"
                                     ,TRUE ~ outcome)
         ,challengecategory = case_when(challengeType %in% c("Quickfire Elimination","Sudden Death Quickfire") ~ "Elimination"
                                        ,TRUE ~ challengeType))

## Number of episodes
temp1 <- results %>%
  filter(inCompetition == "TRUE") %>%
  select(season,seasonNumber,chef,episode) %>%
  # get distinct episodes - because they'll show up multiple times if there are multiple challenges in an episode
  distinct() %>%
  group_by(season,seasonNumber,chef) %>%
  summarise(numberofepisodesin = n())

## Number of challenges by type & season
temp2 <- results %>%
  filter(challengecategory %in% c("Elimination","Quickfire")) %>%
  select(season,seasonNumber,episode,challengecategory) %>%
  distinct() %>%
  group_by(season,seasonNumber,challengecategory) %>%
  summarise(totalchalls = n()) %>%
  mutate(challengecategory = paste0("total_",challengecategory)) %>%
  pivot_wider(names_from = "challengecategory",values_from = "totalchalls")

for(varname in names(temp2)[!(names(temp2) %in% c("season","seasonNumber"))]) {
  temp2[is.na(temp2[,varname]),varname] <- 0
}


## Number of results by challenge type & season
## Excluding number of "safe"/"in"
## and only for the challenge cateogires of elim & qf
temp3 <- results %>%
  filter(challengecategory %in% c("Elimination","Quickfire")) %>%
  filter(outcomecategory %in% c("HIGH","LOW","OUT","WIN")) %>%
  group_by(season,seasonNumber,challengecategory,outcomecategory,chef) %>%
  summarise(numberincateg = n()) %>%
  pivot_wider(names_from = "outcomecategory",values_from = "numberincateg") %>%
  pivot_wider(names_from = "challengecategory",values_from = c("HIGH","LOW","OUT","WIN"))

for(varname in names(temp3)[!(names(temp3) %in% c("season","seasonNumber","chef"))]) {
  temp3[is.na(temp3[,varname]),varname] <- 0
}

## create combinations of High+Win and Low + out
temp3$highwin_Elimination <- temp3$HIGH_Elimination + temp3$WIN_Elimination
temp3$lowout_Elimination <- temp3$LOW_Elimination + temp3$OUT_Elimination
temp3$highwin_Quickfire <- temp3$HIGH_Quickfire + temp3$WIN_Quickfire
temp3$lowout_Quickfire <- temp3$LOW_Quickfire + temp3$OUT_Quickfire


## Bring together
fulldata <- temp1 %>%
  left_join(temp2) %>%
  left_join(temp3) %>%
  left_join(details) %>%
  mutate(
    ## ratio stats
    elimwinratio = WIN_Elimination/total_Elimination
    ,qfwinratio = WIN_Quickfire/total_Quickfire
    ,winratio = (WIN_Elimination + WIN_Quickfire)/(total_Elimination + total_Quickfire)
    ,winhighratio = (highwin_Elimination + highwin_Quickfire)/(total_Elimination + total_Quickfire)
    ,lowoutratio = (lowout_Elimination + lowout_Quickfire)/(total_Elimination + total_Quickfire)
    ,elimhighratio = HIGH_Elimination/total_Elimination
    ,elimlowratio = LOW_Elimination/total_Elimination
    ,qfhighratio = HIGH_Quickfire/total_Quickfire
    ,qflowratio = LOW_Quickfire/total_Quickfire
    ## top 1% of ratios
    ,bestwinratio = ifelse(winratio >= quantile(winratio,.99,na.rm=T),1,0)
    ,bestelimwinratio = ifelse(elimwinratio >= quantile(elimwinratio,.99,na.rm=T),1,0)
    ,bestqfwinratio = ifelse(qfwinratio >= quantile(qfwinratio,.99,na.rm=T),1,0)
    ## 5 elimination wins, 4 qf wins, five elimination highs
    ,fiveelimwins = ifelse(WIN_Elimination >= 5,1,0)
    ,fourqfwins = ifelse(WIN_Quickfire >= 4,1,0)
    ,fiveelimhigh = ifelse(HIGH_Elimination >= 5,1,0)
    ## how much of the GOAT are they?
    ,goat = bestwinratio + bestelimwinratio + bestqfwinratio + fiveelimwins + fourqfwins + fiveelimhigh
  )

summary(fulldata$winratio)
summary(fulldata$elimwinratio)
summary(fulldata$qfwinratio)

summary(fulldata$WIN_Elimination)
summary(fulldata$WIN_Quickfire)
summary(fulldata$HIGH_Elimination)

quantile(fulldata$winratio,.99)
quantile(fulldata$elimwinratio,.99)
quantile(fulldata$qfwinratio,.99)
quantile(fulldata$WIN_Elimination,.99)
quantile(fulldata$WIN_Quickfire,.99)
quantile(fulldata$HIGH_Elimination,.99)


## Greatest of all time
goats <- fulldata %>%
  filter(goat > 0) %>%
  arrange(desc(goat),desc(winratio)) %>%
  select(season,seasonNumber,chef,goat,bestwinratio,bestelimwinratio,bestqfwinratio,fiveelimwins,fourqfwins,fiveelimhigh
         ,total_Elimination,total_Quickfire
         ,elimwinratio,qfwinratio,winratio,WIN_Elimination,WIN_Quickfire,HIGH_Elimination)


data.frame(goats) %>% arrange(desc(winratio)) %>%
  select(season,seasonNumber,chef,winratio,total_Elimination,total_Quickfire)

data.frame(goats) %>% arrange(desc(elimwinratio)) %>%
  select(season,seasonNumber,chef,elimwinratio,total_Elimination)

data.frame(goats) %>% arrange(desc(qfwinratio)) %>%
  select(season,seasonNumber,chef,qfwinratio,total_Quickfire)

data.frame(goats) %>% arrange(desc(WIN_Elimination)) %>%
  select(season,seasonNumber,chef,WIN_Elimination,total_Elimination)

data.frame(goats) %>% arrange(desc(HIGH_Elimination)) %>%
  select(season,seasonNumber,chef,HIGH_Elimination,total_Elimination)

data.frame(goats) %>% arrange(desc(WIN_Quickfire)) %>%
  select(season,seasonNumber,chef,WIN_Quickfire,total_Quickfire)

## Weighted index

## at the end of their seasons

allseasons <- weightedindex("US",1,20,20)
for (season in seq(2,20,1)) {
  allseasons <- rbind(allseasons,weightedindex("US",season,20,20))

}

## at the shortest # of elim & qf challenges
shortestseason <- weightedindex("US",1,11,8)
for (season in seq(2,20,1)) {
  shortestseason <- rbind(shortestseason,weightedindex("US",season,11,8))

}

data.frame(allseasons %>%
             filter(placement == 1) %>%
             select(chef,season,seasonNumber,indexWeight) %>%
             rename(indexWeightFullSeason = indexWeight) %>%
             full_join(shortestseason %>%
                         filter(placement == 1) %>%
                         select(chef,season,seasonNumber,indexWeight) %>%
                         rename(indexWeightShortSeason=indexWeight)) %>%
             arrange(desc(indexWeightFullSeason)))
