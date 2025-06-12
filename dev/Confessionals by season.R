rm(list=ls())
library(tidyverse)
library(gt)
library(ggplot2)
library(openxlsx)
library(RColorBrewer)

directory <- "/Users/carlylevitz/Documents/Data/"

confsRaw <- read.csv(paste0(directory
                        ,"topChef/Top Chef - Confessionals by episode.csv"))%>%
            filter(inCompetition == "TRUE" &
                     # keep just the seasons that I have full data for
                     seasonNumber %in% c(1,2,21)
                     )

placementdata <- read.csv(paste0(directory,"topChef/Top Chef - Chef details.csv"))

## I want to remove the episodes where there are 3 or fewer chefs
## Then, I'll aggregate
confs <- confsRaw %>%
  filter(chefsinepisode > 3) %>%
  ungroup() %>% group_by(season,seasonNumber,series) %>%
  mutate(totalconfs = sum(count,na.rm=T)
         ,totalchefsepisodes = sum(ifelse(inCompetition == "TRUE",1,0))) %>%
  # chef stats
  ungroup() %>% group_by(season,seasonNumber,series,chef
                         ,totalconfs,totalchefsepisodes) %>%
  ## How many episodes is the chef in?
  mutate(episodesIn = sum(ifelse(inCompetition == "TRUE",1,0))
         ## How many times are they the first confessional?
         ## keep just the ones that are first or before intro -not after intro
         ,first = ifelse(first == "after intro",NA,first)
         ,firstconfs = sum(ifelse(!(is.na(first)),1,0))
         ,phonecallsorphotos = sum(ifelse(!(is.na(phone.call)),1,0))
         ,chefconfs = sum(count,na.rm=T)
         ,expectedpercentofconfs = episodesIn/totalchefsepisodes
         ,observedpercent = chefconfs/totalconfs
         ,difffromexpected = observedpercent-expectedpercentofconfs
  ) %>% ungroup() %>%
  select(season,seasonNumber,series,chef
         ,firstconfs,chefconfs
         ,difffromexpected) %>%
  distinct() %>%
  left_join(placementdata %>%
              mutate(placement = as.numeric(placement)
                     ,seasonWinner = ifelse(placement == 1, 1,0)) %>%
              select(series,season,seasonNumber,chef,placement,seasonWinner) )


reg <- glm(confs$seasonWinner ~ confs$firstconfs + confs$difffromexpected)
summary(reg)



