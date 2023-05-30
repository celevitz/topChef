## Carly Levitz
## Date written: 2023-05-28
## Date updated: 2023-05-28
## Purpose: Compare the Top Four across seasons


#####
## Set up
rm(list=ls())
devtools::install_github("celevitz/topChef")

library(tidyverse); library(topChef); library(dplyr); library(ggplot2); library(ggbump)

savedirectory <- "/Users/carlylevitz/Documents/Data/TCSeason20/Episode-agnostic/"

#############################################################################
# Top Four
##################
  # Which episode was the final four for each season?
  #################
    topfourepisode <- topChef::episodeinfo %>%
      #keep just the episodes with four or more chefs;
      # we want to keep those episodes to see how well they've done up until that point
      filter(`#.of.competitors` >= 4) %>%
      select(szn,sznnumber,series,episode,`#.of.competitors`) %>%
      # merge on the challenge wins
      left_join(topChef::challengewins %>% select(!rating)) %>%
      # keep just the final four
        mutate(f4 = ifelse(`#.of.competitors` == 4 & in.competition == "TRUE",1,0)) %>%
        group_by(szn,sznnumber,series,chef) %>%
        mutate(f4=max(f4)) %>%
        filter(f4 == 1) %>%
      # get challenge statistics
        # don't include the wins of the final four episode
        # combine low & out because want to consider them both as low; otherwise, only just a few chefs will have "out" as a separate category
        # and do some edits to issues of
        mutate(outcome = case_when(outcome %in% c("WITHDREW","OUT","RUNNER-UP") ~ "LOW"
                                   ,outcome %in% c("Didn't compete","Qualified") ~ "IN"
                                   ,outcome %in% c("WINNER") ~ "WIN"
                                   ,TRUE ~ outcome)
        # combine different types of challenges: Eliminations or not
               #,challenge_type = case_when(challenge_type %in% c("Quickfire Elimination","Sudden Death Quickfire"))
        ) %>%
      # Keep just US (not Masters or Canada)
        filter(series == "US") %>%
      # count number of wins and stuff
        group_by(szn,sznnumber,series,chef,challenge_type,outcome) %>%
        summarise(n=n()) %>%
      # set up for graphing
        mutate(x = case_when(challenge_type == "Elimination" & outcome == "WIN" ~ 1
                             ,challenge_type == "Quickfire" & outcome == "WIN" ~ 2
                             ,challenge_type == "Elimination" & outcome == "HIGH" ~ 3
                             ,challenge_type == "Quickfire" & outcome == "HIGH" ~ 4
                             ,challenge_type == "Elimination" & outcome == "LOW" ~ 5))



## Visualize
  topfourepisodeSorted <- topfourepisode[order(topfourepisode$x,desc(topfourepisode$n),topfourepisode$chef),]
  topfourepisodeSorted$Y <- NA
  tempcount <- length(unique(topfourepisodeSorted$chef))
  for (chefname in unique(topfourepisodeSorted$chef)) {
    topfourepisodeSorted$Y[topfourepisodeSorted$chef == chefname] <- tempcount
    tempcount <- tempcount -1
  }



  topfourepisodeSorted %>%
    ggplot(aes(x=x,y=Y,fill=n)) +
    geom_tile() +
    scale_fill_gradient(low="white", high="blue")  +
    geom_text(stat="identity",label=topfourepisodeSorted$chef,size=3)

















