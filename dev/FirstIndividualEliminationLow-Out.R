## When was each chef's first elimination low for an individual challenge?

rm(list=ls())
library(tidyverse)
library(openxlsx)
library(topChef)
library(ggplot2)
library(devtools)
library(gt)

directory <- "/Users/carlylevitz/Documents/Data/topChef/"
chefs <- read.csv(paste0(directory,"Top Chef - Chef details.csv")) %>%
  select(chef,name,placement,season,seasonNumber,series) %>%
  filter(series == "US")
challengedetails <- read.csv(paste0(directory
                                  ,"Top Chef - Challenge descriptions.csv"))%>%
  filter(series == "US")

challengewins <- read.csv(paste0(directory,"Top Chef - Challenge wins.csv"))%>%
  filter(series == "US" & inCompetition == "TRUE") %>%
  select(!c(inCompetition,immune,rating))

allchallenges <- challengewins %>%
  full_join(challengedetails %>%
              select(season,seasonNumber,series,episode,challengeType
                     ,outcomeType))

maxepisodebyseason <- allchallenges %>%
  group_by(series,season,seasonNumber) %>%
  summarise(lastepisode = max(episode)) %>%
  # change S22 to be the number of episodes that it will be, not where the data
  # curerntly say it is
  mutate(lastepisode = ifelse(seasonNumber == 22, 14,lastepisode))

## keep just the challenges of interest: elimination-type challs,
## Look at lows and outs, but consider them the same, since we're looking for
## the first individual challenge they were on the bottom
challenges <- allchallenges %>%
  filter(outcomeType == "Individual" &
           !(challengeType %in% c("Qualifying challenge","Qualifying Challenge"
                                  ,"Quickfire")) &
         outcome %in% c("LOW","OUT","RUNNER-UP","DISQUALIFIED")
         ) %>%
  # get the episode # by chef
  ungroup() %>% group_by(season,seasonNumber,series,chef) %>%
  summarise(firstepisodeLow = min(episode)) %>%
  # since maaaaaybe no chefs were at the bottom ever, just to be sure
  # that all chefs are included, add back on the chefs & their placement
  full_join(chefs) %>%
  # make the placement a number
  # if it becomes NA that means that they didn't make it out of LCK or
  # into the main comp from a qualifying challenge
  mutate(placement = as.numeric(placement)) %>%
  # how does this compare to the number of episodes in the season?
  full_join(maxepisodebyseason) %>%
  mutate(proportionintoseason = firstepisodeLow/lastepisode)


# Regression
  reg <- lm(challenges$placement ~ challenges$proportionintoseason +
              challenges$season)
  summary(reg)

# Plot
  # This won't show people who were eliminated on team challenges prior to
  # having an individual challenge
  # There are five winners who were never low on an individual challenge:
  #   Harold, Michael, Richard (S8), Paul, and Kristen
  # It's only been 2nd placers whose first individual low was in the last
  # episode of the season (the finale, where they were runner-up)
  #   Richard (S4), Evelyn, Bryan (S6), Kevin (S6), Shota
  challenges %>%
    ggplot(aes(x=seasonNumber,y=proportionintoseason)) +
    geom_jitter(aes(color=placement))

  challenges %>%
    filter(placement <= 3 #| seasonNumber == 22
           )%>%
    ggplot(aes(x=seasonNumber,y=proportionintoseason)) +
    geom_text(aes(label=chef,color=placement),size=2)




