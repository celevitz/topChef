rm(list=ls())
library(tidyverse)
library(topChef)

chefdetails <- topChef::chefdetails %>%
  filter(series == "US" ) %>%
  mutate(personOfColor = ifelse(is.na(personOfColor),"white",personOfColor))


nrow(chefdetails)
nrow(chefdetails %>% select(name) %>% distinct())

temp <- chefdetails %>%
  group_by(chef) %>%
  summarise(n=n())
table(temp$n)

chefdetails %>%
  group_by(gender) %>%
  summarise(n=n()) %>%
  mutate(percent = n/sum(n))

chefdetails %>%
  group_by(personOfColor) %>%
  summarise(n=n()) %>%
  mutate(percent = n/sum(n))

chefdetails  %>%
  group_by(personOfColor,gender) %>%
  summarise(n=n()) %>%
  mutate(percentOfRace = n/sum(n)) %>%
  ungroup() %>%
  mutate(percentOfTotal = n/sum(n)) %>%
  group_by(gender) %>%
  mutate(percentOfGender=n/sum(n))


chefdetails %>%
  group_by(seasonNumber,season,gender) %>%
  summarise(n=n()) %>%
  mutate(percent = n/sum(n)) %>%
  filter(gender == "Female") %>%
  print(n=21)


chefdetails %>%
  group_by(seasonNumber,season,personOfColor) %>%
  summarise(n=n()) %>%
  mutate(percent = n/sum(n)) %>%
  filter(personOfColor == "POC") %>%
  print(n=21)


region <- chefdetails %>%
  mutate(nyorca = ifelse(state %in% c("New York","California"),"NY/CA","other")) %>%
  group_by(seasonNumber,season,nyorca) %>%
  summarise(n=n()) %>%
  mutate(percent = n/sum(n)) %>%
  filter(nyorca == "other") %>%
  print(n=21)
summary(region$percent)







