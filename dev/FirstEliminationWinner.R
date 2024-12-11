library(topChef)
library(tidyverse)
library(devtools)
#install_github("celevitz/topChef")
directory <- "/Users/carlylevitz/Documents/Data/topChef/"

# Bring in data
chefs <- read.csv(paste0(directory,"Top Chef - Chef details.csv"))
challenges <- read.csv(paste0(directory,"Top Chef - Challenge wins.csv"))
challengedescriptions <- read.csv(paste0(directory
                                ,"Top Chef - Challenge descriptions.csv"))

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

# Seasons without an elimination challenge first episode
challenges %>%
  filter(series == "US" & episode == 1) %>%
  select(season,challengeType) %>%
  distinct() %>%
  group_by(season) %>%
  mutate(hadelimchallfirstep = max(case_when(challengeType %in% c("Elimination","Quickfire Elimination","Sudden Death Quickfire") ~ 1
                   ,TRUE ~ 0))) %>%
  filter(hadelimchallfirstep %in% 0)

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
  filter(outcome %in% "WIN" & episodeflag == episode) %>%
  left_join(chefs %>% select(season,seasonNumber,series,chef,placement)) %>%
  select(season,seasonNumber,chef,placement) %>%
  distinct() %>%
  ungroup() %>% group_by(placement) %>%
  summarise(n=n()) %>%
  mutate(foraveragesnumerator=placement*n) %>%
  ungroup() %>%
  mutate(averageplacement=sum(foraveragesnumerator)/sum(n))

## People who won first elimination challenge and then went on to win/get 2nd
challenges %>%
  filter(series == "US") %>%
  # First Elimination challenge
  filter(challengeType %in% c("Elimination","Quickfire Elimination","Sudden Death Quickfire")) %>%
  group_by(season) %>%
  mutate(episodeflag = min(episode,na.rm=T)) %>%
  # keep just the winners from that episode
  filter(outcome %in% "WIN" & episodeflag == episode) %>%
  left_join(chefs %>% select(season,seasonNumber,series,chef,placement)) %>%
  select(season,seasonNumber,chef,placement) %>%
  distinct() %>% filter(placement<4) %>% arrange(seasonNumber)

## Were the elim challenges Team or Indiv?
challenges %>%
  filter(series == "US") %>%
  # First Elimination challenge
  filter(challengeType %in% c("Elimination","Quickfire Elimination","Sudden Death Quickfire")) %>%
  group_by(season) %>%
  mutate(episodeflag = min(episode,na.rm=T)) %>%
  # keep just that one
  filter(episodeflag == episode) %>%
  select(season,seasonNumber,series,episode) %>%
  distinct() %>%
  left_join(challengedescriptions %>% select(season,seasonNumber,series,episode
                                           ,challengeType,outcomeType)) %>%
  filter(challengeType %in% c("Elimination","Quickfire Elimination","Sudden Death Quickfire")) %>%
  group_by(challengeType,outcomeType) %>%
  summarise(n=n())

## How did the average placement differ if they won indivudally or as a team?
challenges %>%
  filter(series == "US" ) %>%
  left_join(challengedescriptions %>% select(season,seasonNumber,series,episode
                                             ,challengeType,outcomeType)) %>%
  # First Elimination challenge
  filter(challengeType %in% c("Elimination","Quickfire Elimination","Sudden Death Quickfire")) %>%
  group_by(season) %>%
  mutate(episodeflag = min(episode,na.rm=T)) %>%
  # keep just the winners from that episode
  filter(outcome %in% "WIN" & episodeflag == episode) %>%
  left_join(chefs %>% select(season,seasonNumber,series,chef,placement)) %>%
  select(season,seasonNumber,chef,placement,outcomeType) %>%
  distinct() %>%
  ungroup() %>% group_by(outcomeType) %>%
  summarise(averageplacement = mean(placement,na.rm=T))

####################################################################
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


