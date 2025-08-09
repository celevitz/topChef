## Analyze a single season's dishes
rm(list=ls())

library(stringr)
library(tidyverse)
library(wordcloud2)
library(tm)

directory <- "/Users/carlylevitz/Documents/Data/"

## What season am I interested in?
seasonnumber <- 22

## Bring in the data

read.csv(paste0(directory,"/topChef/Top Chef - Dishes long form.csv")
         ,header=TRUE)

## Word cloud for a specific season
  season <- cleandisheslong %>%
    group_by(season,seasonNumber,series,dish) %>%
    summarise(Frequency=n()) %>%
    ungroup() %>%
    group_by(series,season,seasonNumber) %>%
    mutate(N=n()
           ,prop_term_to_total_terms=Frequency/N) %>%
    filter(seasonNumber == seasonnumber & series == "US") %>%
    rename(Term=dish) %>%
    ungroup() %>%
    select(Term,Frequency,prop_term_to_total_terms)

  wordcloud2(season22, shape="pentagon",color="random-dark")
