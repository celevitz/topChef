library(topChef)
library(tidyverse)
library(devtools)
directory <- "/Users/carlylevitz/Documents/Data/topChef/"
# Exclude withdrawals, disqualifications and the "outs" from the finale

# Bring in data
challenges <- read.csv(paste0(directory,"Top Chef - Challenge wins.csv"))
challengedescriptions <- read.csv(paste0(directory
                                   ,"Top Chef - Challenge descriptions.csv"))

# Flag the episodes in which more than one person was eliminated, but could be across diff challs
episodeswithmorethan1personeliminated <- challenges %>%
  filter(outcome %in% c("OUT")) %>%
  group_by(series,season,seasonNumber,episode) %>%
  summarise(flagformorethanonepersoneliminated=n()) %>%
  filter(flagformorethanonepersoneliminated>1 & series == "US") %>%
  ungroup() %>%
  select(!c(flagformorethanonepersoneliminated,series)) %>%
  arrange(seasonNumber,episode) %>%
  rename(episodewithmultipleeliminations=episode)

# Flag the episodes in which more than one person was eliminated in the SAME challenge
episodeswithDE <- challenges %>%
  filter(outcome %in% c("OUT")) %>%
  group_by(series,season,seasonNumber,episode,challengeType) %>%
  summarise(flagformorethanonepersoneliminatedinchall=n()) %>%
  ungroup() %>%
  filter(flagformorethanonepersoneliminatedinchall>1 & series == "US") %>%
  select(!c(flagformorethanonepersoneliminatedinchall,series)) %>%
  arrange(seasonNumber,episode) %>%
  rename(episodewithDE=episode)

## Summary stats
  # With more than 1 person eliminated in an episode
    # number of episodes
    nrow(episodeswithmorethan1personeliminated)
    # number of seasons
    nrow(episodeswithmorethan1personeliminated %>%
           select(seasonNumber) %>%
           distinct())
    # number of seasons with different #s of episodes
    episodeswithmorethan1personeliminated %>%
      group_by(season,seasonNumber) %>%
      summarise(numberofepisodes = n()) %>%
      arrange(seasonNumber)

    episodeswithmorethan1personeliminated %>%
      group_by(season,seasonNumber) %>%
      summarise(numberofepisodes = n()) %>%
      ungroup() %>%
      group_by(numberofepisodes) %>%
      summarise(numberofseasonswiththisnumberofepswithmorethan1elim=n())
    print("and you'll need to add in the ones with no DEs")

  # With more than 1 person eliminated in a given challenge
    nrow(episodeswithDE)
    # number of seasons
    nrow(episodeswithDE %>%
           select(seasonNumber) %>%
           distinct())
    # number of seasons with different #s of episodes
    episodeswithDE %>%
      group_by(season,seasonNumber) %>%
      summarise(numberofepisodes = n()) %>%
      arrange(seasonNumber)

    episodeswithDE %>%
      group_by(season,seasonNumber) %>%
      summarise(numberofepisodes = n()) %>%
      ungroup() %>%
      group_by(numberofepisodes) %>%
      summarise(numberofseasonswiththisnumberofepsofDEs=n())
    print("and you'll need to add in the ones with no DEs")








