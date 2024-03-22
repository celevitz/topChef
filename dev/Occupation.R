library(topChef)
library(tidyverse)
library(devtools)
install_github("celevitz/topChef")

chefs <- data.frame(topChef::chefdetails)

chefs %>%
  filter(series == "US") %>%
  group_by(occupation_category,series) %>%
  summarise(cnumberofchefs=n())



