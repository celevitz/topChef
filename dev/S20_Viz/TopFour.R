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
        # since there are some seasons where there are multiple episodes with four contestants,
        # choose the episode that is the latest in the season
        group_by(szn,sznnumber,series) %>%
        mutate(highestepisode = max(episode)) %>%
        filter(episode == highestepisode) %>%
      select(szn,sznnumber,series,episode,`#.of.competitors`) %>%
      # merge on the challenge wins
      left_join(topChef::challengewins %>% select(!rating)) %>%
      # keep just the final four
        mutate(f4 = ifelse(`#.of.competitors` == 4 & in.competition == "TRUE",1,0)) %>%
        ungroup() %>% group_by(szn,sznnumber,series,chef) %>%
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
                             ,challenge_type == "Elimination" & outcome == "LOW" ~ 5)) %>%
      # merge on placement information
        left_join(topChef::chefdetails %>% select(szn,sznnumber,series,chef,gender,placement))



## Visualize all top fours
  topfourepisodeSorted <- topfourepisode[order(topfourepisode$x,desc(topfourepisode$n),topfourepisode$chef,topfourepisode$sznnumber),]
  topfourepisodeSorted$chefseason <- paste0(topfourepisodeSorted$chef," (",topfourepisodeSorted$szn,")")
  topfourepisodeSorted$Y <- NA
  #if people were in final four more than once, they only get plotted once
  # so take into account the season number
  tempcount <- length(unique(topfourepisodeSorted$chefseason))
  for (chefname in unique(topfourepisodeSorted$chefseason)) {
    topfourepisodeSorted$Y[topfourepisodeSorted$chefseason == chefname] <- tempcount
    tempcount <- tempcount -1
  }

  topfourepisodeSorted %>%
    ggplot(aes(x=x,y=Y,fill=n)) +
    geom_tile() +
    scale_fill_gradient(low="gray95", high="blue")  +
    geom_text(stat="identity",label=topfourepisodeSorted$n,size=2) +
    xlab("") + ylab("") +
    scale_x_continuous(breaks=seq(1,5,1),labels=c("Elimination\nwin","Quickfire\nwin","Elimination\nhigh","Quickfire\nhigh","Elimination\nlow")) +
    scale_y_continuous(breaks=seq(1,max(topfourepisodeSorted$Y),1)
                       ,labels=rev(unique(topfourepisodeSorted$chefseason))) +
    theme_minimal() +
    theme(panel.grid=element_blank()
          ,axis.text.y=element_text(size=5)) +
    labs(title="Top Four Chefs Across 20 Seasons")


## Graphing function
challengevar <- "Elimination"
outcomevar <- "LOW"

topfourgraphs <- function(challengevar,outcomevar) {
  graphdata <- topfourepisode %>% filter(challenge_type == challengevar & outcome == outcomevar)

  # Graph one: boxplot
  graphdata %>%
    ggplot(aes(x=n)) +
    geom_boxplot() +
    xlab(paste0("Number of ",challengevar," ",outcomevar,"S",sep="")) +
    scale_x_continuous(lim=c(0,max(graphdata$n,na.rm=T)+2)
                       , breaks = seq(0,max(graphdata$n,na.rm=T)+2,2)
                       ,labels =seq(0,max(graphdata$n,na.rm=T)+2,2)) +
    labs(title = paste0("Distribution of number of ",challengevar," ",outcomevar,"S of all Final Fours",sep="")) +
    theme_minimal() +
    theme(axis.text.y=element_blank()
          ,panel.grid=element_blank()
          ,axis.ticks.x = element_line(color="black")
          ,axis.line.x = element_line(color="black"))

  # Graph two: average by placement
    graphdata %>%
      group_by(placement) %>%
      summarise(mean=mean(n))
  # Graph 3: people with the most


}













