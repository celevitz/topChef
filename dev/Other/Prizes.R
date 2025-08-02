## Bring in data from Excel file

rm(list=ls())
library(tidyverse)
library(openxlsx)
library(ggplot2)

directory <- "/Users/carlylevitz/Documents/Data/"

rewards <- as_tibble(read.xlsx(paste(directory
                                     ,"TopChefData.xlsx",sep=""),sheet=4))
placement <- read.csv(paste0(directory,"topChef/Top Chef - Chef details.csv")
                      ,header=TRUE)
placement <- placement %>%
  select(chef,season,seasonNumber,series,placement) %>%
  mutate(placement = as.numeric(placement))

## Keep just US and money for each season
## I want to compare this to the number of prizes offered
money <- rewards %>%
  filter(rewardType %in% c( "Money","Money & prize") & series == "US") %>%
  # there are a few things
  mutate(reward = case_when(reward %in% c("$10,000 and Sous Vide machine"
                                          ,"$10,000 and a 10 day cruise from Holland Cruise") ~ "10000"
                            ,reward %in% "$5,000 and year of dinner from Blue Apron + your recipe featured in Blue Apron" ~ "5000"
                            ,TRUE ~ reward)) %>%
  group_by(season,seasonNumber) %>%
  mutate(reward = as.numeric(reward)) %>%
  summarise(reward=sum(reward))

## Keep just US and prizes
  prizes <- rewards %>%
    filter(rewardType %in% c( "Prize","Money & prize") & series == "US") %>%
    select(!c(rewardType,series) )%>%
  # separate out the reward categories
    separate(rewardCategory,c("cat1","cat2","cat3"),", ") %>%
  # Make the data "long" so that I can get accurate counts of the categories
    pivot_longer(!c(season,seasonNumber,episode,challengeType,outcomeType
                    ,reward,chef),names_to = "temp",values_to="Category") %>%
    # drop the NA rows because not all prizes had multiple categories
    filter(!(is.na(Category))) %>%
    select(!temp) %>%
    # Drop the advantages; those aren't really rewards
    filter(Category != "advantage")

## how many prizes were included in the reward?
## if there were multiple winners, it's onloy included once
  prizes %>%
    group_by(season,seasonNumber,episode,challengeType,outcomeType) %>%
    summarise(numberofprizes=n()) %>%
    ungroup() %>%
    group_by(challengeType,outcomeType,numberofprizes) %>%
    summarise(numberoftimes=n())

## For how many challenges was each prize type offered?
  # remove the chef variable and then remove duplicates
  # then, I can just do the counts without multiple uses of group_by
  prizes %>%
    select(season,seasonNumber,episode,challengeType,outcomeType,Category) %>%
    distinct() %>%
    group_by(challengeType,Category) %>%
    summarise(n=n()) %>%
    arrange(desc(n)) %>%
    pivot_wider(names_from=challengeType,values_from=n) %>%
    print(n=30)

  prizes %>%
    select(season,seasonNumber,episode,challengeType,outcomeType,Category) %>%
    distinct() %>%
    group_by(outcomeType,Category) %>%
    summarise(n=n()) %>%
    arrange(desc(n)) %>%
    pivot_wider(names_from=outcomeType,values_from=n) %>%
    print(n=30)

## Has the number of prizes changed by season?
  # remove the chef variable and then remove duplicates
  # then, I can just do the counts without multiple uses of group_by
  numberofprizesbymoney <- prizes %>%
    select(season,seasonNumber,episode,challengeType,outcomeType,Category) %>%
    distinct() %>%
    group_by(season,seasonNumber) %>%
    summarise(n=n()) %>%
    arrange(seasonNumber) %>%
    left_join(money) %>%
    # if there was no cash $ offered, make that 0
    mutate(reward = ifelse(is.na(reward),0,reward))


  numberofprizesbymoney %>%
    mutate(reward = reward/10000) %>%
    ggplot(aes(x=seasonNumber,y=n)) +
    geom_point(pch=16,color="blue") +
    geom_line(color="blue")+
    geom_point(aes(x=seasonNumber,y=reward),pch=22,color="forestgreen") +
    geom_line(aes(x=seasonNumber,y=reward),lty=2,color="forestgreen") +
    ggtitle("Money in 10,000s in green; # of prizes in blue")
  ## seasons 1-5: only prizes
  ## season 6-8: mix - high on both
  ## season 9: a TON of money
  ## Season 10-14 generally low on both
  ## seasons 15 - 20 lower on prizes, higher on money
  ## season 21-22: a ton of money

## By season, how many of each category?
  ## keep the chefs in there - so we can count how many were given out
  prizes %>%
    group_by(seasonNumber,Category) %>%
    summarise(n=n()) %>%
    arrange(seasonNumber) %>%
     pivot_wider(names_from=Category,values_from=n)
