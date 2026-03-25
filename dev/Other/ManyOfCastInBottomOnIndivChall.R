library(tidyverse)

rm(list=ls())
directory <- "/Users/carlylevitz/Documents/Data/topChef/"

# Bring in data
challenges <- read.csv(paste0(directory,"Top Chef - Challenge wins.csv"))

# For each challenge, how many people are in the competition, and how many
# are in a given outcome?
data <- challenges %>%
  # remove those not in the competition
  filter(inCompetition == TRUE) %>%
  group_by(series,season,seasonNumber,episode,challengeType) %>%
  summarise(numberofchefscompeting = n()
         # how many of those chefs are in the bottom, per challenge?
         ,bottom = sum(ifelse(outcome %in% c("LOW","OUT","RUNNER-UP","WITHDREW"
                                             ,"DISQUALIFIED"),1,0))
         ,percent = bottom/numberofchefscompeting)

filtered <- data %>%
  # look at the ones where at least half are in the bottom
  # and just US series - when there are at least 9 chefs in comp
  filter(percent > .5 & series == "US" & numberofchefscompeting >=9)

