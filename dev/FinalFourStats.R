## Carly Levitz
## June 9, 2024
## Purpose: stats about the F4 in each season
## stats PRIOR to the F4 challenge
## remove qualifying challenges
## outcome consolidation:
##  winner = win (this shouldn't be a thing, given we're not including
##          anything F4 onward)
##  OUT == Low.
## diving by the # of challenges, not # of challenges each individual
##  participated in. I.e., people from LCK will be downgraded...

rm(list=ls())

library(tidyverse)
library(topChef)

finalfours <- topChef::chefdetails %>%
  filter(series=="US" &
           (placement <=4 ) | (seasonNumber == 21 & is.na(placement))) %>%
  select(series,season,seasonNumber,chef,placement)

numberofchallseachseasonbeforeF4 <- topChef::challengewins %>%
  right_join(topChef::episodeinfo %>%
               filter(series == "US" & nCompetitors >= 5 &
                        !(is.na(nCompetitors)))) %>%
  select(series,season,seasonNumber,challengeType,episode) %>%
  # remove the reunions and qualifying challenges
  filter(!(is.na(challengeType)) & challengeType != "Qualifying Challenge") %>%
  distinct() %>%
  # going to consolidate all challenge types to get overall rates
  group_by(series,season,seasonNumber) %>%
  summarise(nchall=n())

challoutcomes <- topChef::challengewins %>%
  # keep just the episodes prior to final four
  right_join(topChef::episodeinfo %>%
               filter(series == "US" & nCompetitors >= 5 &
                        !(is.na(nCompetitors)))) %>%
  # remove the reunions and qualifying challenges
  filter(!(is.na(challengeType)) & challengeType != "Qualifying Challenge") %>%
  # consolidate outcomes
  mutate(outcome = case_when(outcome == "WINNER"~"WIN"
                             ,outcome == "OUT" ~ "LOW"
                             ,TRUE ~ outcome)) %>%
  # keep just the ones we want stats on: high, low, win
  filter(inCompetition == TRUE & outcome %in% c("HIGH","LOW","WIN")) %>%
  ## going to consolidate all challenge types to get overall rates
  group_by(series,season,seasonNumber,chef,outcome) %>%
  summarise(n=n())

## Bring data together to get the %s in each category

  f4stats <- challoutcomes %>%
    full_join(numberofchallseachseasonbeforeF4) %>%
    right_join(finalfours) %>%
    group_by(series,season,seasonNumber,chef,outcome) %>%
    mutate(percent = n/nchall) %>%
    select(!n) %>%
    pivot_wider(names_from=outcome,values_from=percent)

  for (var in c("HIGH","LOW","WIN")) {
    f4stats[is.na(f4stats[,var]),var] <- 0
  }

  f4stats$top <- f4stats$WIN+f4stats$HIGH


## detailed tables
  f4stats %>%
    filter(seasonNumber == 21)  %>%
    arrange(desc(WIN),desc(HIGH),LOW)

  f4stats %>%
    arrange(desc(top),desc(WIN),desc(HIGH),LOW) %>%
    filter(top > .5) %>%
    mutate(atjudgestable=top+LOW)

## # of people better than S21 people
  nrow(f4stats[f4stats$top > .429,])
  nrow(f4stats[f4stats$WIN > .238,])
  nrow(f4stats[f4stats$LOW < .0952,])

## summary by season
  summarybyseason <- data.frame(f4stats %>%
    group_by(series,season,seasonNumber) %>%
    summarise(HIGH =mean(HIGH)
              ,LOW=mean(LOW)
              ,WIN=mean(WIN)
              ,top=mean(top)) %>%
    arrange(desc(top),desc(WIN),desc(HIGH),LOW) %>%
    relocate(top,.before=HIGH) %>%
      relocate(WIN,.after=top))

  summarybyseason %>%
    arrange(WIN)




