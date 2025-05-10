## Bring in data from Excel file

rm(list=ls())
library(tidyverse)

directory <- "/Users/carlylevitz/Documents/Data/topChef/"

rewards <- read.csv(paste0(directory,"Top Chef - Rewards.csv")
                    ,header=TRUE)

## Keep just US and money
  money <- rewards %>%
    filter(rewardType == "Money" & series == "US") %>%
    group_by(season,seasonNumber) %>%
    mutate(reward = as.numeric(reward)
      ,totalperseason = sum(reward,na.rm=T)) %>%
    group_by(season,seasonNumber,chef,totalperseason) %>%
    summarise(reward = sum(reward,na.rm=T)) %>%
    mutate(percent = reward/totalperseason)


  money %>%
    filter(seasonNumber == 22) %>%
    arrange(desc(percent))




