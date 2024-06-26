rm(list=ls())
library(tidyverse)
library(openxlsx)
library(topChef)

challengewins <- topChef::challengewins %>%
  filter(series == "US")

chefdetails <- topChef::chefdetails %>%
  filter(series == "US" )

descriptions <- topChef::challengedescriptions %>% filter(series == "US")

# keep just two elimination challenges
first2elimchalls <- challengewins %>%
  filter(challengeType %in% c("Elimination","Quickfire Elimination","Sudden Death Quickfire") &
           ( (episode < 3 & !(season %in% c("Miami","Seattle","Texas"))) |
           (episode < 4 & season %in% c("Miami","Seattle"))  |
           (episode < 5 & season %in% c("Texas")) ) ) %>%
  select(season,seasonNumber,series,episode,challengeType) %>%
  distinct()

# number of winners of first two elim challs
first2elimchalls %>%
  left_join(challengewins) %>%
  filter(outcome == "WIN") %>%
  group_by(season) %>%
  summarise(numberofchefs=n()) %>%
  print(n=21)

## Double eliminations

  doubleelims <- challengewins %>%
    filter(outcome %in% c("OUT")) %>%
    group_by(season,seasonNumber,episode) %>%
    summarise(numbereliminated=n()) %>%
    filter(numbereliminated > 1) %>%
    select(!numbereliminated)

  trueDE <- challengewins %>%
    filter(outcome %in% c("OUT")) %>%
    group_by(season,seasonNumber,episode,challengeType) %>%
    summarise(numbereliminated=n()) %>%
    filter(numbereliminated > 1) %>%
    ungroup() %>%
    select(!c(numbereliminated,challengeType)) %>%
    mutate(true="trueDE")

  lck <- descriptions %>%
    filter(!(is.na(lastChanceKitchenWinnerEnters)) & seasonNumber != 1) %>%
    select(season,seasonNumber,episode,lastChanceKitchenWinnerEnters) %>%
    mutate(numberchefsreentered=ifelse(seasonNumber == 15 & episode == 5,2,1))


  temp <- doubleelims %>%
    full_join(lck) %>%
    arrange(seasonNumber,episode) %>%
    # flag LCK seasons
    group_by(seasonNumber) %>%
    mutate(flagforlck = max(case_when(!(is.na(lastChanceKitchenWinnerEnters)) ~ 1
                                      ,TRUE ~ 0)))


  temp2 <- trueDE %>%
    full_join(lck) %>%
    arrange(seasonNumber,episode) %>%
    # flag LCK seasons
    group_by(seasonNumber) %>%
    mutate(flagforlck = max(case_when(!(is.na(lastChanceKitchenWinnerEnters)) ~ 1
                                      ,TRUE ~ 0))) %>%
    arrange(seasonNumber,episode)

  print(paste0("There have been ",nrow(doubleelims)," episodes with a double elimination"))
  print(paste0("There have been ",nrow(doubleelims[doubleelims$seasonNumber>=9,])," episodes with a double elimination when they have had LCK"))
  print("Distribution of number of episodes with double eliminations by season:")
  doubleelims %>%
    ungroup() %>%
    group_by(season,seasonNumber) %>%
    summarise(numberofdoubleelims = n()) %>%
    arrange(seasonNumber) %>%
    full_join(lck %>%
                group_by(season,seasonNumber,numberchefsreentered) %>%
                summarise(numberofepisodeswithsomeoneentering =n()
                        ,numberchefsreentered=sum(numberchefsreentered)))
  doubleelims %>% ungroup() %>% group_by(season,seasonNumber) %>% summarise(numberofdoubleelims = n()) %>% ungroup() %>% group_by(numberofdoubleelims) %>% summarise(numberofseasons = n())



  temp %>%
    filter(flagforlck == 1) %>%
    select(season,seasonNumber,episode,lastChanceKitchenWinnerEnters) %>%
    rename(lckwinner=lastChanceKitchenWinnerEnters) %>%
    #mutate(lckwinner=ifelse(is.na(lckwinner),0,1)) %>%
    print(n=40)

  temp2 %>%
    filter(flagforlck == 1) %>%
    select(season,seasonNumber,episode,lastChanceKitchenWinnerEnters) %>%
    rename(lckwinner=lastChanceKitchenWinnerEnters) %>%
    print(n=40)

  # How many chefs were in the competition when there was a double elim?

  numberincompatde <- doubleelims %>%
    left_join(challengewins %>%
                filter(inCompetition == "TRUE") %>%
                group_by(season,series,seasonNumber,episode,challengeType) %>%
                mutate(numberincomp = n()) %>%
                filter(outcome == "OUT")) %>%
    ungroup() %>%
    select(season,seasonNumber,episode,challengeType,numberincomp) %>%
    distinct() %>%
    arrange(seasonNumber,episode)

  numberincompatde %>%
    mutate(numberincompcat = case_when(numberincomp == 4 ~ "04"
                           ,numberincomp %in% c(5,6,7,8) ~ "05-08"
                          ,numberincomp %in% c(9,10,11) ~ "09-11"
                          ,numberincomp %in% c(12) ~ "12"
                          ,numberincomp %in% c(13,14,15,16,17) ~ "13-17")) %>%
    group_by(numberincompcat) %>%
    summarise(n=n())

  #### Get the index for how people were doing JUST before the elimination challenge in which they were eliminated
  # so get the # of elim/SDQ challenges for that episode, and subtract 1. But keep # of QF same
  doubleelims %>%
    left_join(challengewins %>% filter(outcome == "OUT")) %>%
    group_by(seasonNumber,episode) %>%
    mutate(flagfortruedoubleelim = min(case_when(challengeType == "Elimination" ~ 1
                                                 ,TRUE ~ 0))) %>%
    select(!c(inCompetition,immune,outcome,rating)) %>%
    filter(flagfortruedoubleelim == 1) %>%
    arrange(seasonNumber,episode) %>% print(n=50)

  ## LA season 2
  challengewins %>% filter(seasonNumber == 2 & episode <= 5) %>% select(episode,challengeType) %>% distinct() %>% group_by(challengeType) %>% summarise(n=n())
  weightedindex("US",2,4,5) %>% select(chef,indexWeight) %>%
    full_join(challengewins %>% filter(episode == 5 & seasonNumber == 2 & outcome == "OUT")) %>%
    arrange(desc(indexWeight))

  challengewins %>% filter(seasonNumber == 2 & episode <= 12) %>% select(episode,challengeType) %>% distinct() %>% group_by(challengeType) %>% summarise(n=n())
  weightedindex("US",2,11,11) %>% select(chef,indexWeight) %>%
    full_join(challengewins %>% filter(episode == 12 & seasonNumber == 2 & outcome == "OUT")) %>%
    arrange(desc(indexWeight))

  ## new york (episodes 7 & 13)
  challengewins %>% filter(seasonNumber == 5 & episode <= 7) %>% select(episode,challengeType) %>% distinct() %>% group_by(challengeType) %>% summarise(n=n())
  weightedindex("US",5,7,6) %>% select(chef,indexWeight) %>%
    full_join(challengewins %>% filter(episode == 7 & seasonNumber == 5 & outcome == "OUT")) %>%
    arrange(desc(indexWeight))

  challengewins %>% filter(seasonNumber == 5 & episode <= 13) %>% select(episode,challengeType) %>% distinct() %>% group_by(challengeType) %>% summarise(n=n())
  weightedindex("US",5,13,12) %>% select(chef,indexWeight) %>%
    full_join(challengewins %>% filter(episode == 13 & seasonNumber == 5 & outcome == "OUT")) %>%
    arrange(desc(indexWeight))

  ## all stars 8
  challengewins %>% filter(seasonNumber == 8 & episode <= 6) %>% select(episode,challengeType) %>% distinct() %>% group_by(challengeType) %>% summarise(n=n())
  weightedindex("US",8,5,5) %>% select(chef,indexWeight) %>%
    full_join(challengewins %>% filter(episode == 6 & seasonNumber == 8 & outcome == "OUT")) %>%
    arrange(desc(indexWeight))

  ## season 11, first double - ep 3
  challengewins %>% filter(seasonNumber == 11 & episode <= 3) %>% select(episode,challengeType) %>% distinct() %>% group_by(challengeType) %>% summarise(n=n())
  weightedindex("US",11,3,5) %>% select(chef,indexWeight) %>%
    full_join(challengewins %>% filter(episode == 3 & seasonNumber == 11 & outcome == "OUT")) %>%
    arrange(desc(indexWeight))

#############################################
## number of group challenges before RW

  RWepnumber <- challengedescriptions %>%
    filter(((grepl("restaurant wars",tolower(challengeDescription ) ) |
               grepl("restaurants wars",tolower(challengeDescription ) )) &
             outcomeType == "Team" &
             series == "US")) %>%
    group_by(seasonNumber) %>%
    # taking the minimum episode because some seasons had two episodes of RW
    summarise(rwep = min(episode))

  challengesbeforeRW <- challengewins %>%
    filter(series == "US" & challengeType %in% c("Elimination","Quickfire","Quickfire Elimination","Sudden Death Quickfire")) %>%
    left_join(challengedescriptions %>%
                select(series,season,seasonNumber,episode,challengeType,outcomeType)) %>%
    right_join(RWepnumber) %>%
    select(series,season,seasonNumber,episode,challengeType,outcomeType,rwep) %>%
    distinct() %>%
    filter(episode < rwep & series == "US") %>%
    ungroup() %>%
    group_by(seasonNumber,season,outcomeType,challengeType) %>%
    summarise(n=n()) %>%
    filter(!(is.na(outcomeType))) %>%
    pivot_wider(names_from=outcomeType,values_from=n) %>%
    mutate(Individual = ifelse(is.na(Individual),0,Individual)
           ,Team = ifelse(is.na(Team),0,Team)
           ,percentbeforeRW = Team/(Team+Individual))

  challengesbeforeRW %>%
    filter(challengeType == "Elimination") %>%
    print(n=50)

  summary( challengesbeforeRW %>%
             filter(challengeType == "Elimination") %>%
             ungroup() %>%
             select(percentbeforeRW))


  challengesthruep9 <- challengewins %>%
    filter(series == "US" & challengeType %in% c("Elimination","Quickfire","Quickfire Elimination","Sudden Death Quickfire")) %>%
    full_join(challengedescriptions %>%
                select(series,season,seasonNumber,episode,challengeType,outcomeType)) %>%
    select(series,season,seasonNumber,episode,challengeType,outcomeType) %>%
    distinct() %>%
    filter(episode <=9 & series == "US") %>%
    ungroup() %>%
    group_by(seasonNumber,season,outcomeType,challengeType) %>%
    summarise(n=n()) %>%
    filter(!(is.na(outcomeType))) %>%
    pivot_wider(names_from=outcomeType,values_from=n) %>%
    mutate(Individual = ifelse(is.na(Individual),0,Individual)
           ,Team = ifelse(is.na(Team),0,Team)
           ,percentthru9 = Team/(Team+Individual))

  challengesthruep9 %>%
    filter(challengeType == "Elimination") %>%
    print(n=50)

  summary( challengesthruep9 %>%
             filter(challengeType == "Elimination") %>%
             ungroup() %>%
             select(percentthru9))


  challengesthruep9 %>%
    filter(challengeType == "Elimination") %>%
    select(seasonNumber,season,percentthru9) %>%
    full_join(challengesbeforeRW %>%
                filter(challengeType == "Elimination") %>%
                select(seasonNumber,season,percentbeforeRW) ) %>%
    print(n=21)

  # The data are listed as follows: season #, % of elimination challenges that were team challenges through episode 9, # of elimination challenges that were team challenges prior to restaurant wars. S1: 33%, 33%. S2: 44%, 44%. S3: 43%, 43%. S4: 78%, 75%. S5: 44%, 38%. S6: 44%, 38%. S7: 44%, 38%. S8: 56%, 67%. S9: 71%, 71%. S10: 50%, 44%. S11: 56%, 50%. S12: 44%, 50%. S13: 44%, 38%. S14: 75%, 83%. S15: 56%, 43%. S16: 56%, 67%. S17: 56%, 57%. S18: 44%, 43%. S19: 56%, 57%. S20: 56%, 50%. S21: 56%, 57%.






