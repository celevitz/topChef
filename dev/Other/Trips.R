## Bring in data from Excel file

rm(list=ls())
library(tidyverse)
library(openxlsx)
library(ggplot2)

directory <- "/Users/carlylevitz/Documents/Data/"

rewards <- as_tibble(read.xlsx(paste(directory
                                     ,"TopChefData.xlsx",sep=""),sheet=4))

trips <- rewards %>%
  filter(grepl("trip",rewardCategory))

## Number of trips given away by season
trips %>%
  group_by(series,season,seasonNumber) %>%
  summarise(n=n()) %>%
  arrange(seasonNumber)

## Where are the trips?
trips <- trips %>%
  mutate(place = case_when(
    grepl("New Zealand",reward) ~ "New Zealand"
    ,grepl("Australia",reward) ~ "Australia"
    ,grepl("Italy",reward) ~ "Italy"
    ,grepl("Amsterdam",reward) ~ "Netherlands"
    ,grepl("Napa",reward) ~ "California"
    ,grepl("Terlato",reward) ~ "California"
    ,grepl("Tokyo",reward) ~ "Japan"
    ,grepl("Olympics",reward) & seasonNumber == 17~ "Japan"
    ,grepl("Barbados",reward) ~ "Barbados"
    ,grepl("Spain",reward) ~ "Spain"
    ,grepl("Paris",reward) ~ "France"
    ,grepl("Cannes",reward) ~ "France"
    ,grepl("London",reward) ~ "Britain"
    ,grepl("Mexico",reward) ~ "Mexico"
    ,grepl("Cape Canaveral",reward) ~ "Florida"
    ,grepl("Palm Springs",reward) ~ "Florida"
    ,reward %in% c("VIP trip to world premiere of Jurassic World Dominion"
                   ,"Trip to Fast & Furious Movie premiere") ~ "Movie premiere"
    ,TRUE ~ "Location of chef's choice"
  ))

trips %>%
  group_by(place) %>%
  summarise(n=n()) %>%
  arrange(desc(n))

