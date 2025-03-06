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

## Straight-up elimination challenges
challenges %>%
  filter(series == "US" ) %>%
  left_join(challengedescriptions %>% select(season,seasonNumber,series,episode
                                             ,challengeType,outcomeType)) %>%
  # First Elimination challenge
  filter(challengeType %in% c("Elimination")) %>%
  group_by(season) %>%
  mutate(episodeflag = min(episode,na.rm=T)) %>%
  # keep just the winners from that episode
  filter(outcome %in% "WIN" & episodeflag == episode) %>%
  left_join(chefs %>% select(season,seasonNumber,series,chef,placement)) %>%
  select(season,seasonNumber,chef,placement,outcomeType) %>%
  distinct() %>%
  ungroup() %>% group_by(outcomeType) %>%
  summarise(averageplacement = mean(placement,na.rm=T))

## Episode-specific information for people who won first elim challenge
firstelimwinners <- challenges %>%
  filter(series == "US") %>%
  # First Elimination challenge
  filter(challengeType %in% c("Elimination","Quickfire Elimination","Sudden Death Quickfire")) %>%
  group_by(season) %>%
  mutate(episodeflag = min(episode,na.rm=T)) %>%
  # keep just the winners from that episode
  filter(outcome %in% "WIN" & episodeflag == episode) %>%
  select(season,seasonNumber,series,chef,episodeflag) %>%
  distinct()

trajectory <- firstelimwinners %>%
  left_join(challenges) %>%
  filter(#immune %in% TRUE |
           outcome %in% c("WIN","WINNER","RUNNER-UP","OUT"
                          ,"DISQUALIFIED","WITHDREW")) %>%
  select(!c(rating,inCompetition)) %>%
  mutate(outcome = ifelse(outcome %in% "WINNER","WIN",outcome)
         ,outcome = ifelse(outcome %in% "RUNNER-UP","OUT",outcome)
         ,seasonNumberAsString=case_when(
           seasonNumber < 10~paste0("0",as.character(seasonNumber))
           ,seasonNumber >=10 ~ as.character(seasonNumber))
         ,challengeType = ifelse(challengeType %in% c("Quickfire Elimination"
                                                      ,"Quickfire elimination"
                                          ,"Sudden Death Quickfire")
                           ,"Elimination",challengeType)
           ,id=paste0(seasonNumberAsString,chef)) %>%
  group_by(id) %>%
  # order things by when they were eliminated
  mutate(episodeout = max(episode,na.rm=T)) %>%
  arrange(desc(episodeout),desc(episode),id)

trajectory %>%
  ggplot(aes(x=episode,y=id,shape=challengeType,color=outcome)) +
  geom_point(alpha=.5) +
  theme_minimal()

## Just the first elim chall win to when they were out of the game
  trajectory %>%
    ungroup() %>%
    select(season,seasonNumberAsString,chef,episodeflag,episodeout) %>%
    distinct() %>%
    mutate(lengthofrun=episodeout-episodeflag+1) %>%
    arrange(desc(lengthofrun),seasonNumberAsString) %>%
    select(!lengthofrun) %>%
    ## Add on the final episode number
    left_join(
      challenges %>%
        filter(outcome %in% "WINNER" & series %in% "US") %>%
        select(season,episode) %>%
        rename(finalepisode=episode)
    ) %>%
    print(n=30)














