
rm(list=ls())
library(tidyverse)

directory <- "/Users/carlylevitz/Documents/Data/topChef/"

# Bring in data
challengewins <- read.csv(paste0(directory,"Top Chef - Challenge wins.csv"))


temp <- challengewins %>%
  filter(challengeType %in% c("Elimination","Quickfire","Quickfire Elimination"
                              ,"Sudden Death Quickfire") &
           inCompetition == "TRUE"
         # maaaaybe remove the didn't compete rows
         ) %>%
  mutate(outcome = case_when(
    outcome %in% c("HIGH","WIN","WINNER") ~ "Top"
    ,outcome %in% c("DISQUALIFIED","LOW","OUT","RUNNER-UP","WITHDREW") ~ "Bottom"
    ,TRUE ~ "in")
    ,In = ifelse(outcome %in% c("in","Top"),1,0)
    ,Top = ifelse(outcome %in% c("Top"),1,0)) %>%
  group_by(series,season,seasonNumber,chef) %>%
  summarise(numberofchallenges=n()
         ,In=sum(In)
         ,Top = sum(Top)) %>%
  mutate(InP = round(In/numberofchallenges,3)
         ,TopP = round(Top/numberofchallenges,3)
         ,ITP = InP + TopP) %>%
  ungroup() %>%
  group_by(series) %>%
  mutate(Average = mean(ITP)
         ,ITPplus = ITP/Average*100) %>%
  select(series,season,seasonNumber,chef,numberofchallenges
         ,InP,TopP,ITP,ITPplus)

tcus <- temp %>%
  filter(series == "US") %>%
  select(!series)


tcus %>%
  arrange(desc(ITPplus),desc(numberofchallenges)) %>%
  filter(numberofchallenges > 4) %>%
  print(n=20)


