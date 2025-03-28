## Analyze how many people have "won" episodes
## Carly Levitz
## March 27, 2025

rm(list=ls())
library(tidyverse)
library(devtools)

directory <- "/Users/carlylevitz/Documents/Data/topChef/"

# Bring in data
chefs <- read.csv(paste0(directory,"Top Chef - Chef details.csv"))
challenges <- read.csv(paste0(directory,"Top Chef - Challenge wins.csv"))

  wonepisode <- challenges %>%
    filter(outcome %in% "WIN" & series == "US") %>%
    group_by(series,season,seasonNumber,chef,episode) %>%
    summarise(numberofchallswoninepisode = n()) %>%
    filter(numberofchallswoninepisode > 1) %>%
    ungroup() %>%
    group_by(series,season,seasonNumber,chef) %>%
    summarise(numberofepswon = n()) %>%
    # number of seasons for which a person won an episode
    ungroup() %>%
    group_by(series,chef) %>%
    mutate(numberofseasons =n()) %>%
    # merge on info about gender & placement
    left_join(chefs %>%
                select(chef,season,seasonNumber,series,placement,gender))

  ## All people
  wonepisode %>%
    print(n=50)

## Stats
  # Number of people
  dim(wonepisode)

  # Number of people on the list in more than 1 season
  wonepisode %>%
    filter(numberofseasons>1)

  # Number of people who did it more than once in a season
  wonepisode %>%
    filter(numberofepswon>1) %>%
    arrange(desc(numberofepswon))

  # Any seasons in which it didn't happen?
  # not seasons 1, 16
  table(wonepisode$seasonNumber)
  wonepisode %>%
    ungroup() %>%
    group_by(series,season,seasonNumber) %>%
    summarise(numberofchefs = n()
              ,numberofepisodes = sum(numberofepswon)) %>%
    arrange(desc(numberofchefs),desc(numberofepisodes))

  # Average placement
  wonepisode %>%
    ungroup() %>%
    summarise(placement = mean(placement,na.rm=T))

  wonepisode %>%
    ungroup() %>% group_by(placement) %>%
    summarise(n=n())

  # Number of winners
  wonepisode %>%
    filter(placement == 1)

  # Gender distribution
  wonepisode %>%
    ungroup() %>%
    group_by(gender) %>%
    summarise(n=n()
              ,averageplacement=mean(placement))






