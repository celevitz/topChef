rm(list=ls())
library(tidyverse)
library(openxlsx)
library(topChef)

challengewins <- topChef::challengewins %>%
  filter(series == "US")

chefdetails <- topChef::chefdetails %>%
  filter(series == "US" )

# keep just two elimination challenges
first2elimchalls <- challengewins %>%
  filter(challengeType %in% c("Elimination","Quickfire Elimination","Sudden Death Quickfire") &
           ( (episode < 3 & !(season %in% c("Miami","Seattle","Texas"))) |
           (episode < 4 & season %in% c("Miami","Seattle"))  |
           (episode < 5 & season %in% c("Texas")) ) ) %>%
  select(season,seasonNumber,series,episode,challengeType) %>%
  distinct()

# number of winners of first two elim challs
first2elimchalls %>%
  left_join(challengewins) %>%
  filter(outcome == "WIN") %>%
  group_by(season) %>%
  summarise(numberofchefs=n()) %>%
  print(n=21)






