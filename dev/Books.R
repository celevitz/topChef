## Carly Levitz
## March 31, 2024
## Combine info about books with the chefs' first season

library(tidyverse)

rm(list=ls())
directory <- "/Users/carlylevitz/Documents/Data/topChef/"

# Air date
epi <- read.csv(paste0(directory,"Top Chef - Episode information.csv"))
epi <- epi %>%
  group_by(series,season,seasonNumber) %>%
  filter(episode == min(episode)) %>%
  mutate(yearofairdate=as.numeric(substr(airDate,1,4))) %>%
  select(series,season,seasonNumber,yearofairdate)

chefs <- read.csv(paste0(directory,"Top Chef - Chef details.csv"))
  # First time they were on
    chefs <- chefs %>%
      group_by(name,series) %>%
      summarise(seasonNumber=min(seasonNumber)) %>%
      # Drop Bryan V from US Masters; he was on US first
      filter(!(name == "Bryan Voltaggio" & series == "US Masters")) %>%
      # bring on year of their season
      full_join(epi)

# Book data
bookdata <- read.csv(paste0(directory,"Top Chef - Books.csv"))

  books <- bookdata %>%
    left_join(chefs %>%
                rename(Chef=name)) %>%
    # was it before or after TC
    mutate(timing=ifelse(yearofairdate<=Year
                         ,"After Top Chef","Before Top Chef")
           ,series = ifelse(is.na(series),"-",series)
           ,seasonNumber = ifelse(is.na(seasonNumber),"-",seasonNumber)
           ,season = ifelse(is.na(season),"-",season)
           ,yearofairdate = ifelse(is.na(yearofairdate),"-",yearofairdate)
           ,timing = ifelse(is.na(timing),"-",timing))

  write.csv(books %>%
              select(!c(series,yearofairdate))
            ,paste0(directory,"Top Chef - Books with season data.csv")
            ,row.names=FALSE)

