## Carly Levitz
## Date written: 2023-05-28
## Date updated: 2023-05-28
## Purpose: Compare the Top Four across seasons


#####
## Set up
rm(list=ls())
devtools::install_github("celevitz/topChef")

library(tidyverse); library(topChef); library(dplyr); library(ggplot2); library(ggpubr)

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
        # drop reunions
        filter(!(grepl("Reunion",episode_name)) & episode_name != "Top Chef Holiday Special") %>%
        # since there are some seasons where there are multiple episodes with four contestants,
        # choose the episode that is the latest in the season
        group_by(szn,sznnumber,series) %>%
        mutate(highestepisode = max(episode)) %>%
        filter(episode == highestepisode) %>%
      select(szn,sznnumber,series,episode) %>%
      rename(episodeflag = episode)

  # Which Chefs are in the final four?
    finalfourchefs <- topfourepisode %>%
      left_join(topChef::challengewins) %>%
      filter(in.competition == "TRUE" & episode == episodeflag) %>%
      select(series,szn,sznnumber,chef) %>%
      distinct()

  # Get the challenge wins for all episodes leading up to the Final Four
  # But exclude the wins of the episode in which there was the final four
    topfourdata <-
    topChef::challengewins %>% select(!rating) %>%
      left_join(topfourepisode) %>%
      filter(episode < episodeflag) %>%
      # Merge on the top four chef names -- and keep only those
      right_join(finalfourchefs) %>%
      # get challenge statistics
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
  topfourepisodeSorted <- topfourdata[order(topfourdata$x,desc(topfourdata$n),topfourdata$chef,topfourdata$sznnumber),]
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
outcomevar <- "HIGH"

topfourgraphs <- function(challengevar,outcomevar) {
  graphdata <- topfourdata %>%
    filter(challenge_type == challengevar & outcome == outcomevar)

  # Graph one: boxplot
  graphone <-  graphdata %>%
    ggplot(aes(x=n)) +
    geom_boxplot() +
    xlab(paste0("Number of ",challengevar," ",outcomevar,"S",sep="")) +
    scale_x_continuous(lim=c(0,max(graphdata$n,na.rm=T)+2)
                       , breaks = seq(0,max(graphdata$n,na.rm=T)+2,2)
                       ,labels =seq(0,max(graphdata$n,na.rm=T)+2,2)) +
    labs(title = paste0("Distribution of number of ",challengevar," ",outcomevar,"S of those in episodes with the final four chefs",sep="")
         ,subtitle="If chef was in Last Chance Kitchen at the Final Four, they are not included here") +
    theme_minimal() +
    theme(axis.text.y=element_blank()
          ,panel.grid=element_blank()
          ,axis.ticks.x = element_line(color="black")
          ,axis.line.x = element_line(color="black"))

  # Graph two: average by placement
  placementdata <-  graphdata %>%
      group_by(placement) %>%
      summarise(mean=mean(n),N=n()) %>%
      mutate(placement=as.character(placement)
             ,placement=case_when(placement == "1" ~ "1st"
                                  ,placement == "2" ~ "2nd"
                                  ,placement == "3" ~ "3rd"
                                  ,placement == "4" ~ "4th"
                                  ,placement == "5" ~ "5th"
                                  ,is.na(placement) ~ "Current season")
           ,placement=paste0(placement," (N = ",N,")"))

  placementdata$placement <- factor(placementdata$placement
                                    ,levels = rev(sort(placementdata$placement)))

  graphtwo <-   placementdata %>%
    ggplot(aes(x=mean,y=placement,label = round(mean,1))) +
    geom_bar(stat="identity") +
    xlab(paste0("Average number of ",challengevar," ",outcomevar,"S",sep="")) +
    labs(title = paste0("Average number of ",challengevar," ",outcomevar,"S by placement for those in\nepisodes with the final four chefs",sep="")) +
    theme_minimal() +
    theme(panel.grid=element_blank()
          ,axis.ticks.x = element_line(color="black")
          ,axis.line.x = element_line(color="black")) +
    geom_text(hjust=0,nudge_x = .1)

  # Graph 3: average by gender
  genderdata <-  graphdata %>%
    group_by(gender) %>%
    summarise(mean=mean(n),N=n()) %>%
    mutate(gender=paste0(gender," (N = ",N,")"))

  graphthree <-   genderdata %>%
    ggplot(aes(x=mean,y=gender,label = round(mean,1))) +
    geom_bar(stat="identity") +
    xlab(paste0("Average number of ",challengevar," ",outcomevar,"S",sep="")) +
    labs(title = paste0("Average number of ",challengevar," ",outcomevar,"S by gender for those in\nepisodes with the final four chefs",sep="")) +
    theme_minimal() +
    theme(panel.grid=element_blank()
          ,axis.ticks.x = element_line(color="black")
          ,axis.line.x = element_line(color="black")) +
    geom_text(hjust=0,nudge_x = .1)


  # Graph 4: people with the most
  # threshold for this changes by variable
  if (challengevar == "Elimination" & outcomevar == "LOW") {
    threshold <- 6
  }  else if (challengevar == "Elimination" & outcomevar == "HIGH") {
      threshold <- 5
  }  else { threshold <- 4}


  chefswithmost <-  graphdata %>%
    filter(n >= threshold) %>%
    arrange(desc(n)) %>%
    mutate(chefinseason = paste0(chef," (",szn,")"))

  graphfour <-
    chefswithmost %>%
    ggplot(aes(x=n,y=reorder(chefinseason,n),label=n)) +
    geom_bar(stat="identity") +
    xlab(paste0("Number of ",challengevar," ",outcomevar,"S",sep="")) +
    labs(title = paste0("Chefs with the most ",challengevar," ",outcomevar,"S",sep="")) +
    theme_minimal() +
    theme(panel.grid=element_blank()
          ,axis.ticks.x = element_line(color="black")
          ,axis.line.x = element_line(color="black")
          ,axis.title.y = element_blank()) +
    geom_text(hjust=0,nudge_x = .1)

  # bring all graphs together
  ggarrange(graphone,ggarrange(graphtwo,graphthree),graphfour,
            ncol=1,nrow=3,heights = c(1,1,2))


}





## Notes
## If people came back from LCK after F4, then they may not be included here, e.g., Kristen in Seattle







