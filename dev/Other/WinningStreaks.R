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
  filter(outcome %in% c("WIN","WINNER") & series %in% "US" &
           challengeType %in% c("Elimination","Quickfire Elimination"
                                ,"Sudden Death Quickfire"))
