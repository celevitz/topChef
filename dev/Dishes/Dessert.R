
rm(list=ls())
library(ggplot2)

directory <- "/Users/carlylevitz/Documents/Data/"

dishesraw <- read.csv(paste0(directory
             ,"/topChef/Top Chef - Dishes wide form with classifications.csv")
                      ,header = TRUE)
placement <- read.csv(paste0(directory
                             ,"/topChef/Top Chef - Chef details.csv")
                      ,header = TRUE) %>%
  select(chef,series,seasonNumber,season,placement) %>%
  mutate(placement = as.numeric(placement))

desserts <- dishesraw %>%
  # how many people are in the challenge at this time?
  group_by(series,season,seasonNumber,episode,challengeType) %>%
  mutate(numberofchefs = n()) %>%
  # keep just those that did dessert - and that there were at least
  # 5 chefs still in the competition, to try to exclude finales
  # and also elimination challenges
  filter(dessert == 1 & numberofchefs > 4 & challengeType == "Elimination") %>%
  select(series,season,seasonNumber,episode,chef,challengeType
         ,dish,outcome,outcomeType,notes,numberofchefs) %>%
  mutate(required = ifelse(grepl("required",notes),1,0)
         ,outcomesimplified = case_when(outcome == "RUNNER-UP"~"OUT"
                                       ,outcome == "WINNER"~"WIN"
                                       ,TRUE ~ outcome)
         ,outcomesimplified2 = case_when(outcomesimplified %in%
                                                          c("HIGH","LOW")~"IN"
                                         ,TRUE ~ outcomesimplified)) %>%
  # merge on the placement info
  left_join(placement) %>%
  mutate(placementcategory = case_when(placement < 5 ~ "1st to 4th"
                                       ,placement >=5 & placement < 9 ~ "5th through 8th"
                                       ,TRUE ~ "9th and higher"))

# How well do people do depending on if the challenge required a dessert?
table(desserts$outcomesimplified2,desserts$required)
chisq.test(desserts$outcomesimplified2,desserts$required)

table(desserts$outcomesimplified,desserts$required)

## non-required
nonreqd <- desserts %>%
  # for now, exclude the required ones
  filter(required == 0)


table(nonreqd$outcomesimplified)

# placement of people who do desserts in elim challs??
# people who were high/win
temp <- nonreqd %>%
  ungroup() %>%
  select(series,season,seasonNumber,chef,placement)





