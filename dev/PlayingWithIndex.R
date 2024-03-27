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

allseasons <- weightedindex("US",1,1,1)
for (season in seq(2,20,1)) {
  allseasons <- rbind(allseasons,weightedindex("US",season,1,1))

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
