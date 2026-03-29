
rm(list=ls())
library(tidyverse)
library(gt)
library(ggplot2)

directory <- "/Users/carlylevitz/Documents/Data/topChef/"

# What is the main season I'm looking at?
seasonofinterest <- 23
# Through what episode do I want to compare seasons?
episodeofinterest <- 3
# What placement do we want to look at (or better)
placementofinterest <- 6
# Exclude those eliminated
eliminated <- c("Jaspratap Bindra","Day Anaїs Joseph","Nana Araba Wilmot")

# Placement data
  placement <- read.csv(paste0(directory,"Top Chef - Chef details.csv")
                        ,header=TRUE)  %>%
    # change placement to a numeric, and keep just those of interest
    mutate(placement = as.numeric(placement)) %>%
    filter((placement<=placementofinterest | seasonNumber==seasonofinterest) &
             # keep only US series
             series == "US" &
             # drop the seasons that I don't have all the data for
             seasonNumber %in% c(1,2,3,8,16,17,20,21,22,23)) %>%
    # keep variables of interest
    select(chef,season,seasonNumber,series,placement)

  placement$chef[placement$chef == "Kevin D'Andrea"] <- "Kévin D'Andrea"
  placement$chef[placement$chef == "Cesar Murillo"] <- "César Murillo"

# confessional data
  confsraw <- read.csv(paste0(directory
                           ,"Top Chef - Confessionals by episode.csv")
                    ,header=TRUE) %>%
    # Keep just the episodes of interest
    filter(episode <= episodeofinterest & series == "US" &
           # drop the seasons that I don't have all the data for
             seasonNumber %in% c(1,2,3,8,16,17,20,21,22,23)
           )



# Each season through episode of interest: # of confs and chef-episodes
  season <- confsraw %>%
    select(season,seasonNumber,series,chefsinepisode,totalconfsinep
           ,equalInEp) %>%
    distinct() %>%
    group_by(season,seasonNumber,series) %>%
    summarise(chefepisodes=sum(chefsinepisode)
              ,totalconfs = sum(totalconfsinep)
              ,equalInEpinSeason = sum(equalInEp))

# Each chef: total confs thru episode of interest
  chefs <- confsraw %>%
    group_by(season,seasonNumber,series,chef) %>%
    summarise(episodesIn = sum(ifelse(inCompetition == "TRUE",1,0))
              ,chefconfs = sum(count,na.rm=T) )

# Combine chef-specific information with season-specific
  confs <- chefs %>%
    full_join(season) %>%
    right_join(placement) %>%
    # how far off of equal are they?
    mutate(difffromexpected = (chefconfs-equalInEpinSeason)/
             equalInEpinSeason) %>%
    # drop unneeded vars
    ungroup() %>%
    select(!c(chefepisodes,totalconfs,series,episodesIn))

# Let's compare the current season to the winners
  confs %>%
    filter((seasonNumber == seasonofinterest | placement == 1) &
             !(chef %in% eliminated) )%>%
    arrange(desc(difffromexpected),chef) %>%
    select(chef,difffromexpected) %>%
    print(n=30)




