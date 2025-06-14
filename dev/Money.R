## Bring in data from Excel file

rm(list=ls())
library(tidyverse)

directory <- "/Users/carlylevitz/Documents/Data/topChef/"

rewards <- read.csv(paste0(directory,"Top Chef - Rewards.csv")
                    ,header=TRUE)
placement <- read.csv(paste0(directory,"Top Chef - Chef details.csv")
                      ,header=TRUE)
placement <- placement %>%
  select(chef,season,seasonNumber,series,placement) %>%
  mutate(placement = as.numeric(placement))

## Keep just US and money
  money <- rewards %>%
    filter(rewardType %in% c( "Money","Money & prize") &
             series == "US") %>%
    # there are a few things
    mutate(reward = case_when(reward %in% c("$10,000 and Sous Vide machine"
                                            ,"$10,000 and a 10 day cruise from Holland Cruise") ~ "10000"
                              ,reward %in% "$5,000 and year of dinner from Blue Apron + your recipe featured in Blue Apron" ~ "5000"
                              ,TRUE ~ reward)) %>%
    group_by(season,seasonNumber) %>%
    mutate(reward = as.numeric(reward)
      ,totalperseason = sum(reward,na.rm=T)) %>%
    group_by(season,seasonNumber,chef,totalperseason) %>%
    summarise(reward = sum(reward,na.rm=T)) %>%
    mutate(percent = reward/totalperseason) %>%
    left_join(placement)


  money %>%
    filter(seasonNumber %in% c(20,21) ) %>%
    arrange(desc(percent)) %>%
    print(n=75)


  #Buddha S20, Jeremy, S11 Nicholas and Nina tied, Kristen

  summary(money$percent)

  cor.test(money$placement[money$seasonNumber %in% c(20,21)]
           ,money$percent[money$seasonNumber %in% c(20,21)])


## Season 22
  money %>%
    filter(seasonNumber %in% c(20,21,22) ) %>%
    arrange(desc(percent)) %>%
    print(n)

  # by challenge type
  s22byChallType <- rewards %>%
    filter(rewardType %in% c( "Money","Money & prize") &
             series == "US" & seasonNumber == 22) %>%
    # there are a few things
    mutate(reward = case_when(reward %in% c("$10,000 and Sous Vide machine"
                                            ,"$10,000 and a 10 day cruise from Holland Cruise") ~ "10000"
                              ,reward %in% "$5,000 and year of dinner from Blue Apron + your recipe featured in Blue Apron" ~ "5000"
                              ,TRUE ~ reward)) %>%
    group_by(season,seasonNumber,challengeType,chef) %>%
    summarise(reward = sum(as.numeric(reward),na.rm=T)) %>%
    pivot_wider(names_from=challengeType,values_from=reward)


## leaderboard
  money %>%
    filter(reward > 20000) %>%
    arrange(desc(reward),desc(percent),seasonNumber,chef)



