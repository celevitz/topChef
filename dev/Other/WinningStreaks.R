library(topChef)
library(tidyverse)
library(devtools)
#install_github("celevitz/topChef")

rm(list=ls())
directory <- "/Users/carlylevitz/Documents/Data/topChef/"

# Bring in data
chefs <- read.csv(paste0(directory,"Top Chef - Chef details.csv"))
challenges <- read.csv(paste0(directory,"Top Chef - Challenge wins.csv"))

## Keep just the wins
## for now, just going to keep elimination-type challenges
wins <- challenges %>%
  filter(outcome %in% c("WIN","WINNER") &
           series %in% "US" &
           challengeType %in% c("Elimination","Quickfire Elimination"
                                ,"Sudden Death Quickfire") &
           inCompetition %in% "TRUE") %>%
  # keep just relevant variables
  select(seasonNumber,episode,chef) %>%
  arrange(seasonNumber,chef,episode) %>%
  # is there someone who won both elim chall types in 1 episode?
  # I can use summarize b/c I'm using all variables as the grping vars
  # this will consolidate the episodes. I'm counting it as a consecutive win if
  #   they won at least 1 elim chall in that episode. May need to revisit this.
  #   Stefan S5E1, Gregory S12E3, Jeremy S13E13, Carrie S15E9
  group_by(seasonNumber,episode,chef) %>%
  mutate(nwins=n()) %>%
  # how many times have the chefs won?
  group_by(seasonNumber,chef) %>%
  mutate(numwins = n())

# Capture chefs with any streak?
winstreaks <- wins %>%
  # What's the difference between the episodes?
  bind_cols(twoinarow = sapply(1:dim(wins)[1], function(z){
              (wins$episode[z + 1] - wins$episode[z]) <2 &
                wins$seasonNumber[z+1] == wins$seasonNumber[z] &
                wins$chef[z+1] == wins$chef[z]    })    ) %>%
  # did a chef experience any streak? if so, keep them
  group_by(seasonNumber,chef) %>%
  mutate(streak2 = max(ifelse(twoinarow == TRUE,1,0),na.rm=T)  ) %>%
  filter(streak2==1   ) %>% select(!c(streak2,twoinarow) ) %>%
  # three in a row?
  bind_cols(threeinarow = sapply(1:dim(wins)[1], function(z){
    (wins$episode[z + 2] - wins$episode[z+1]) <2 &
      wins$numwins >= 3 &
      wins$seasonNumber[z+1] == wins$seasonNumber[z] &
      wins$chef[z+1] == wins$chef[z]    })    ) %>%
    # create a flag for if this is the case
      group_by(seasonNumber,chef) %>%
      mutate(streak3 = max(ifelse(threeinarow == TRUE,1,0),na.rm=T)  )




