rm(list=ls())
library(tidyverse)
library(openxlsx)
library(topChef)
library(ggplot2)
library(devtools)
library(gt)

directory <- "/Users/carlylevitz/Documents/Data/topChef/"

scores <- read.csv(paste0(directory
                                 ,"Top Chef - All seasons final scores.csv"))

seasonofinterest <- 22

# What's the ranking of the 5th place chef from the season of interest?
  # index weight
    fifthplace <- scores %>%
      filter(placement == 5) %>%
      select(!c(placement,stillincomp,RankOfThoseStillIn)) %>%
      arrange(desc(indexWeight))
      # Massimo is 4th

    fifthplace %>%
      arrange(desc(Elimination.WIN)) %>%
      select(chef,seasonNumber,Elimination.WIN)
    # Massimo is 2nd, tied with Carla S8

    fifthplace %>%
      arrange(desc(Elimination.HIGH)) %>%
      select(chef,seasonNumber,Elimination.HIGH)
    # Massimo is tied for 5th with 7 other people
    # 6th is the worst rank

    fifthplace %>%
      arrange(Elimination.LOW) %>%
      select(chef,seasonNumber,Elimination.LOW)
    # Massimo is tied for third with five other people

    fifthplace %>%
      arrange(desc(Quickfire.WIN)) %>%
      select(chef,seasonNumber,Quickfire.WIN)
    # Massimo is tied for 5th with five other people
    # 5th is the worst rank

    fifthplace %>%
      arrange(desc(Quickfire.HIGH)) %>%
      select(chef,seasonNumber,Quickfire.HIGH)
    # Massimo has the most out of any other fifth placers

    fifthplace %>%
      arrange(Quickfire.LOW) %>%
      select(chef,seasonNumber,Quickfire.LOW)
    # Tied for 5th; 6th is the worst rank



