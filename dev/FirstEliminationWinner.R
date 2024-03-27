library(topChef)
library(tidyverse)
library(devtools)
install_github("celevitz/topChef")

challenges <- topChef::challengewins
chefs <- topChef::chefdetails

# number of seasons with multiple first elim winners: Charleston, NY, Texas
challenges %>%
  filter(series == "US") %>%
  # First Elimination challenge
  filter(challengeType %in% c("Elimination","Quickfire Elimination","Sudden Death Quickfire")) %>%
  group_by(season) %>%
  mutate(episodeflag = min(episode,na.rm=T)) %>%
  # keep just the winners from that episode
  filter(outcome == "WIN" & episodeflag == episode) %>%
  group_by(season) %>%
  summarise(numberofwinners=n()) %>%
  filter(numberofwinners>1)

# Placement of chefs who won first elimination challenge
# just those seasons with only one winner

placement <- challenges %>%
  filter(series == "US"
         #& !(season %in% c("Texas","Charleston","New York"))
         ) %>%
  # First Elimination challenge
  filter(challengeType %in% c("Elimination","Quickfire Elimination","Sudden Death Quickfire")) %>%
  group_by(season) %>%
  mutate(episodeflag = min(episode,na.rm=T)) %>%
  # keep just the winners from that episode
  filter(outcome == "WIN" & episodeflag == episode) %>%
  left_join(chefs %>% select(season,seasonNumber,series,chef,placement)) %>%
  select(season,seasonNumber,chef,challengeType,placement) %>%
  distinct() %>%
  ungroup() %>% group_by(challengeType,placement) %>%
  summarise(n=n())

sum(placement$n)


statsbynumberofchalls <- challenges %>%
  filter(seasonNumber == 21) %>%
  group_by(chef,challengeType,outcome) %>%
  filter(!(is.na(outcome))) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = outcome, values_from = n) %>%
  mutate(IN = ifelse(is.na(IN) ,0,IN)
         ,LOW = ifelse(is.na(LOW),0,LOW)
         ,HIGH = ifelse(is.na(HIGH),0,HIGH)
         ,OUT = ifelse(is.na(OUT),0,OUT)
         ,WIN = ifelse(is.na(WIN),0,WIN)) %>%
  pivot_wider(names_from = challengeType,values_from = c("IN","LOW","HIGH","OUT","WIN")) %>%
  ungroup()


statsbynumberofchalls$score <- statsbynumberofchalls$WIN_Elimination*7+
  statsbynumberofchalls$HIGH_Elimination*3 -
  statsbynumberofchalls$LOW_Elimination*3-
  statsbynumberofchalls$OUT_Elimination*7
# +
#   statsbynumberofchalls$WIN_Quickfire*4+
#   statsbynumberofchalls$HIGH_Quickfire*2-
#   statsbynumberofchalls$LOW_Quickfire*2
#
statsbynumberofchalls %>%
  select(chef,score) %>%
  arrange(desc(score))


