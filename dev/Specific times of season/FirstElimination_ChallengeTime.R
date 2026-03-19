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
    ## remove the two that are missing time (NY season 5; Colorado season 15)
    ## otherwise, it will affect the avgs if I make everything that is NA, 0
    filter(!(is.na(cookTime))) %>%
    mutate(cookTime = as.numeric(ifelse(is.na(cookTime),"0",cookTime))
           ,prepTime = ifelse(is.na(prepTime),0,prepTime)
           ,totalTime = cookTime+prepTime)

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
  ## cook time: Average 137 (2 hrs, 17 min); median 120 (2 hrs)
  ## total time: avg 169 (2 hrs, 49 min). Median 150 (3 hrs)
  challs %>% ungroup() %>% summarise(avg=mean(cookTime,na.rm=T)
                                     ,mdn = median(cookTime,na.rm=T)
                                     ,avgtotal=mean(totalTime,na.rm=T)
                                     ,mdntotal=median(totalTime,na.rm=T))

  ## prep time and cook time
  challs %>%
    filter(!(is.na(cookTime))) %>%
    arrange(cookTime,prepTime) %>%
    print(n=23)


