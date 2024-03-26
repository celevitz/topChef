library(topChef)
library(tidyverse)
library(devtools)
install_github("celevitz/topChef")

chefs <- topChef::chefdetails

chefs %>%
  filter(series == "US") %>%
  group_by(occupation_category,series) %>%
  summarise(numberofchefs=n())


chefdetails %>%
  filter(series == "US") %>%
  group_by(occupation_category) %>%
  summarise(n=n())

chefdetails %>%
  filter(series == "US" & !(is.na(occupation))) %>%
  group_by(occupation_category) %>%
  summarise(n=n()) %>%
  mutate(percent=n/sum(n)) %>%
  arrange(desc(percent))
