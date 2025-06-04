rm(list=ls())
library(tidyverse)
library(openxlsx)
library(ggplot2)

directory <- "/Users/carlylevitz/Documents/Data/topChef/"

challengewins <- read.csv(paste0(directory,"Top Chef - Challenge wins.csv"))%>%
  filter(series == "US")

chefdetails <- read.csv(paste0(directory,"Top Chef - Chef details.csv"))  %>%
  filter(series == "US" )

epinfo <- read.csv(paste0(directory,"Top Chef - Episode information.csv"))  %>%
  filter(series == "US" )

## First, gender ratio at start
starting <- chefdetails %>%
  select(season,seasonNumber,chef,gender) %>%
  filter(!(is.na(gender))) %>%
  group_by(season,seasonNumber,gender) %>%
  summarise(n=n())%>%
  # how many are in the comp at that time?
  ungroup() %>%
  group_by(season,seasonNumber) %>%
  mutate(total = sum(n)
         ,timing="Including Qualifiers and LCK not in main comp.")

## Gender ratio of only those who were in the official competition
officialcomp <- chefdetails %>%
  mutate(placement=as.numeric(placement)) %>%
  filter(!(is.na(gender)) &
           # While Season 22 is ongoing, I need to include them since their
           # placement is NA for those still in the comp
           (!(is.na(placement)) | seasonNumber == 22)
         ) %>%
  select(season,seasonNumber,chef,gender) %>%
  group_by(season,seasonNumber,gender) %>%
  summarise(n=n()) %>%
  # how many are in the comp at that time?
  ungroup() %>%
  group_by(season,seasonNumber) %>%
  mutate(total = sum(n)
         ,timing="Only in official competition")

## Gender ratio of top 8 chefs
eight <- chefdetails %>%
  mutate(placement=as.numeric(placement)) %>%
  filter(!(is.na(gender)) &
           # While Season 22 is ongoing, I need to include them since their
           # placement is NA for those still in the comp
           (placement <=8 |  (seasonNumber == 22 & is.na(placement)) )
        ) %>%
  select(season,seasonNumber,chef,gender) %>%
  group_by(season,seasonNumber,gender) %>%
  summarise(n=n()) %>%
  mutate(total=8
         ,timing = "Final 8")

## Gender ratio of top 4 chefs
four <- chefdetails %>%
  mutate(placement=as.numeric(placement)) %>%
  filter(!(is.na(gender)) &
           # While Season 22 is ongoing, I need to include them since their
           # placement is NA for those still in the comp
           (placement <=4 | (seasonNumber == 22 & is.na(placement)) )
  ) %>%
  select(season,seasonNumber,chef,gender) %>%
  group_by(season,seasonNumber,gender) %>%
  summarise(n=n()) %>%
  mutate(total=4
         ,timing = "Final 4")

## Bring the data together
alldata <- starting %>%
  bind_rows(officialcomp) %>%
  bind_rows(eight) %>%
  bind_rows(four) %>%
  # Get percent of group that are women
  mutate(percent = round(n/total,3)) %>%
  filter(gender == "Female") %>%
  # get ratio of group that are women
  mutate(ratio = n/(total-n)) %>%
  select(!gender)

alldata %>%
ggplot(aes(x=seasonNumber,y=percent,color=timing)) +
  geom_line() +
  scale_y_continuous(limits=c(0,1)) +
  facet_wrap(~timing)



