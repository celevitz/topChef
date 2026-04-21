
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
  mutate(required = ifelse(grepl("required",notes),1,0)
         ,outcomesimplified = case_when(outcome == "RUNNER-UP"~"OUT"
                                       ,outcome == "WINNER"~"WIN"
                                       ,TRUE ~ outcome)
         ,outcomesimplified2 = case_when(outcomesimplified %in%
                                                          c("HIGH","LOW")~"IN"
                                         ,TRUE ~ outcomesimplified))

# How well do people do depending on if the challenge required a dessert?
table(desserts$outcomesimplified2,desserts$required)
chisq.test(desserts$outcomesimplified2,desserts$required)
