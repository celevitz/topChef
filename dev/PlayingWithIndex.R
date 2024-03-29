rm(list=ls())
library(tidyverse)
library(openxlsx)
library(topChef)

challengewins <- topChef::challengewins %>%
  filter(series == "US")

chefdetails <- topChef::chefdetails %>%
  filter(series == "US" )


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




#############
##
temp <- weightedindex("US",1,2,1)
for (season in seq(2,21,1)) {
  temp <- rbind(temp,weightedindex("US",season,2,1))

}

ep1scores <- chefdetails %>%
  filter(seasonNumber == 21 & series == "US") %>%
  select(chef) %>%
  mutate(indexWeight1=case_when(chef == "Manny Barella" ~ 7
                               ,chef %in% c("Michelle Wallace","Danny Garcia") ~ 3
                               ,chef %in% c("Amanda Turner","Kenny Nguyen") ~ -3
                               ,chef == "David Murphy" ~ -7
                               ,TRUE ~ 0))

temp %>%
  filter(seasonNumber == 21) %>%
  select(chef,indexWeight) %>%
  full_join(ep1scores) %>%
  mutate(indexWeight=ifelse(is.na(indexWeight),0,indexWeight)) %>%
  rename(indexWeight2=indexWeight) %>%
  arrange(desc(indexWeight1))  %>%
  mutate(rank1=rank(-indexWeight1,ties.method = "min")) %>%
  arrange(desc(indexWeight2)) %>%
  mutate(rank2=rank(-indexWeight2,ties.method = "min")) %>%
  select(chef,rank1,rank2) %>%
  mutate(diff=-(rank2-rank1))


temp %>%
  filter(placement == 1) %>%
  arrange(desc(indexWeight)) %>%
  select(chef,seasonNumber,indexWeight) %>%
  full_join(chefdetails %>%
              filter(placement == 1 & series == "US") %>%
              select(chef,season,seasonNumber,series,placement))

