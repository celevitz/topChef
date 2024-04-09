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




