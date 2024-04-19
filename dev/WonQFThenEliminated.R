rm(list=ls())
library(tidyverse)
library(ggplot2)
library(devtools)
devtools::install_github("celevitz/topChef")

challengewins <- topChef::challengewins %>%
  filter(series == "US") %>%
  mutate(qfwin = ifelse(challengeType == "Quickfire" & outcome=="WIN",1,0)
         ,elimOUT = ifelse(challengeType == "Elimination" & outcome == "OUT",1,0)) %>%
  group_by(season,seasonNumber,chef,episode) %>%
  summarise(qfwin=max(qfwin)
            ,elimOUT=max(elimOUT)) %>%
  left_join(
    # number of chefs in the episode at the time
    topChef::challengewins %>%
      filter(series == "US" & inCompetition == "TRUE") %>%
      group_by(season,seasonNumber,episode) %>%
      select(season,seasonNumber,episode,chef) %>%
      distinct() %>%
      summarise(numberofchefs=n())
  )

# look at just those who won the QF and were eliminated
wonqfthenout <- challengewins %>%
  filter(qfwin == 1 & elimOUT == 1) %>%
  arrange(seasonNumber,episode)

# how many chefs were eliminated this way in these seasons?
wonqfthenout %>%
  ungroup() %>%
  group_by(season) %>%
  summarise(n=n())

# number of seasons in which this happened
nrow(wonqfthenout %>%
  ungroup() %>%
  select(season) %>%
  distinct() )

# number of chefs in the competition
summary(wonqfthenout$numberofchefs)

table(wonqfthenout$numberofchefs)








