library(topChef)
library(tidyverse)
library(devtools)
#install_github("celevitz/topChef")
directory <- "/Users/carlylevitz/Documents/Data/topChef/"

# Bring in data
challengedescriptions <- read.csv(paste0(directory
                                ,"Top Chef - Challenge descriptions.csv"))

# which challenges to keep?
# just elimination - not QF Elim or SDQ
  challs <- challengedescriptions %>%
    filter(challengeType %in% "Elimination") %>%
    # keep just the first elim chall
    group_by(series,season,seasonNumber) %>%
    mutate(keepepisode = min(episode,na.rm=T)) %>%
    filter(episode == keepepisode & series == "US") %>%
    select(season,seasonNumber,series,episode,outcomeType,shopTime,prepTime
           ,cookTime) %>%
    mutate(cookTime = as.numeric(cookTime))

## Notes
  challs %>%
    arrange(cookTime) %>%
    print(n=23)
  ## Seattle: 47 minutes (rotation of space needle)
  ## Missing two: NY (season 5) and Colorado (season 15); both episode 1
  ## Chicaog (season 4) and California (Season 13) both had 90 min, but
  ##    CA also had 180 minutes prep time.
  challs %>% ungroup() %>% group_by(cookTime) %>% summarise(n=n())
  ## 7 seasons 120
  ## 4 seasons 150
  ## 5 seasons 180
  challs %>% ungroup() %>% summarise(avg=mean(cookTime,na.rm=T)
                                     ,mdn = median(cookTime,na.rm=T))
  ## Average 137; median 120
