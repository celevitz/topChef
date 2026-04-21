
rm(list=ls())
library(ggplot2)

directory <- "/Users/carlylevitz/Documents/Data/"

dishesraw <- read.csv(paste0(directory
             ,"/topChef/Top Chef - Dishes wide form with classifications.csv")
                      ,header = TRUE)

desserts <- dishesraw %>%
  filter(dessert == 1) %>%
  select(series,season,seasonNumber,episode,chef,challengeType
         ,dish,outcome,outcomeType,notes) %>%
  mutate(required = ifelse(grepl("required",notes),1,0))
