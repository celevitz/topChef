## Analyze a single season's dishes
rm(list=ls())

library(stringr)
library(tidyverse)
library(wordcloud2)
library(tm)
library(openxlsx)

directory <- "/Users/carlylevitz/Documents/Data/"

## What season am I interested in?
seasonnumber <- 22

  ## what word do I think is unique to this season?
    wordofinterest <- "maple"

## Bring in the data
dishesraw <- as_tibble(read.xlsx(paste(directory,"TopChefData.xlsx",sep="")
                                                    ,sheet=2)) %>%
  filter(!(is.na(dish))) %>%
  filter(seasonNumber == seasonnumber & series == "US")

cleandisheslongraw <- read.csv(paste0(directory
                                   ,"/topChef/Top Chef - Dishes long form.csv")
         ,header=TRUE)
cleandisheslong <- cleandisheslongraw %>%
  filter(seasonNumber == seasonnumber & series == "US")

classifiedraw <- read.csv(paste0(directory
              ,"/topChef/Top Chef - Dishes wide form with classifications.csv")
                       ,header=TRUE) %>%
  filter(seasonNumber == seasonnumber & series == "US")

## how many dishes are in the season?
  # how many people made dishes in the challenges
  # and then need to take into account when people made multiple dishes
  temp <- dishesraw %>%
    bind_cols(data.frame(str_split_fixed(dishesraw$dish,";",5))) %>%
    # don't need dish because it'll be the same as X1 if there's only 1 dish
    select(!dish) %>%
    rename(dish1=X1,dish2=X2,dish3=X3,dish4=X4,dish5=X5)

  dim(temp %>% filter(!(is.na(dish1))))[1] +
    dim(temp %>% filter(!(is.na(dish2)) & dish2 != ""))[1] +
    dim(temp %>% filter(!(is.na(dish3)) & dish3 != ""))[1] +
    dim(temp %>% filter(!(is.na(dish4)) & dish4 != ""))[1] +
    dim(temp %>% filter(!(is.na(dish5)) & dish5 != ""))[1]

## How many challenges are in the season?
  nrow(dishesraw %>%
    select(series,season,challengeType,episode) %>%
    distinct())

  # how many challenge-chefs are in the season?
  nrow(dishesraw %>%
         select(series,season,challengeType,episode,chef) %>%
         distinct())

## Word cloud for a specific season
  seasonwordclouddata <- cleandisheslong %>%
    group_by(season,seasonNumber,series,dish) %>%
    summarise(Frequency=n()) %>%
    ungroup() %>%
    group_by(series,season,seasonNumber) %>%
    mutate(N=n()
           ,prop_term_to_total_terms=Frequency/N) %>%
    rename(Term=dish) %>%
    ungroup() %>%
    select(Term,Frequency,prop_term_to_total_terms)

  wordcloud2(seasonwordclouddata, shape="pentagon",color="random-dark")

## How many words are in the season?
  dim(seasonwordclouddata)[1]
  sum(seasonwordclouddata$Frequency)

## Number of times a word shows up
  wordtimes <- cleandisheslong %>%
    group_by(series,dish) %>%
    summarise(totaltimesused=n()) %>%
    arrange(desc(totaltimesused))

## number of episodes a word is used in
  wordinepisodes <- cleandisheslong %>%
    select(series,season,seasonNumber,episode,dish) %>%
    distinct() %>%
    group_by(series,dish) %>%
    summarise(numberofepisodesusedin = n()) %>%
    arrange(desc(numberofepisodesusedin))

## Classifications
  # Denominator is the sum of the # of chefs in each challenge.
  denom <- nrow(classifiedraw)

  # how many of each
  classified <- classifiedraw %>%
    # change sauce to be 0/1
    mutate(sauce = as.numeric(ifelse(is.na(sauce),"0","1"))) %>%
    select(!c(series,season,seasonNumber,episode,chef,challengeType,outcome
              ,notes,trend,outcomeType)) %>%
    pivot_longer(!dish,names_to = "category",values_to="count") %>%
    group_by(category) %>%
    summarise(count=sum(count)) %>%
    mutate(denominator = denom
           ,percent=count/denominator) %>%
    arrange(desc(count))

  classified %>% print(n=dim(classified)[1])

## Unique word of the season
  uniquewordanalysis <- cleandisheslongraw %>%
  group_by(series,seasonNumber,dish) %>%
    summarise(n=n()) %>%
    # how many times does this actually show up?
    ungroup() %>% group_by(series,dish) %>%
    mutate(N=sum(n)) %>%
    arrange(seasonNumber,desc(N),dish) %>%
    filter(dish %in% c(wordofinterest)) %>%
    left_join(
      # number of dishes I have in that season
      as_tibble(read.xlsx(paste(directory,"TopChefData.xlsx",sep="")
                          ,sheet=2)) %>%
        filter(!(is.na(dish))) %>%
        group_by(series,season,seasonNumber) %>%
        summarise(total = n()) ) %>%
    mutate(percent = (n/total)*100)

  uniquewordanalysis %>%
    filter(series == "US") %>%
    select(series,seasonNumber,total,percent)







