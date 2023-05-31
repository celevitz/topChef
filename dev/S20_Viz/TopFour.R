## Carly Levitz
## Date written: 2023-05-28
## Date updated: 2023-05-28
## Purpose: Compare the Top Four across seasons


#####
## Set up
rm(list=ls())
devtools::install_github("celevitz/topChef")

library(tidyverse); library(topChef); library(dplyr); library(ggplot2); library(ggpubr);library(stringr)

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
  # Exclude the people who made it to F4 but because of LCK ended up 5th
  # and INCLUDE those who weren't in F4 but came in after and did 4th or better (e.g., Kristen in Seattle)
    finalfourchefs <- topfourepisode %>%
      left_join(topChef::challengewins) %>%
      filter(in.competition == "TRUE" & episode == episodeflag) %>%
      select(series,szn,sznnumber,chef) %>%
      distinct() %>%
      mutate(keep="keep") %>%
      # merge on placement information
      full_join(topChef::chefdetails %>% select(szn,sznnumber,series,chef,gender,placement)) %>%
      # need to do <=4 twice
      filter((keep == "keep" | placement <= 4) & (placement <= 4 | is.na(placement))) %>%
      select(!keep)

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
        group_by(szn,sznnumber,series,chef,challenge_type,outcome,placement,gender) %>%
        summarise(n=n()) %>%
      # set up for graphing
        mutate(x = case_when(challenge_type == "Elimination" & outcome == "WIN" ~ 1
                             ,challenge_type == "Quickfire" & outcome == "WIN" ~ 2
                             ,challenge_type == "Elimination" & outcome == "HIGH" ~ 3
                             ,challenge_type == "Quickfire" & outcome == "HIGH" ~ 4
                             ,challenge_type == "Elimination" & outcome == "LOW" ~ 5))



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

# set up ggplot stuff
    themestuff <- theme_minimal() +
      theme(panel.grid=element_blank()
            ,axis.ticks.x = element_line(color="black")
            ,axis.line.x = element_line(color="black")
            ,axis.text.x = element_text(size=18,color="black")
            ,axis.text.y = element_text(size=18,color="black")
            ,axis.title.x = element_text(size=18,color="black")
            ,axis.title.y = element_blank()
            ,plot.title=element_text(size=20,face="bold",color="black")
            ,plot.subtitle = element_text(size=18,color="black")
            ,plot.caption = element_text(size=15,hjust=0))

topfourgraphs <- function(challengevar,outcomevar) {
  graphdata <- topfourdata %>%
    filter(challenge_type == challengevar & outcome == outcomevar)

  # Notes
  graphzero <- graphdata %>%
    ggplot(aes(x=n)) +
    theme_void() +
    labs(title = str_wrap(paste0("    Top Four Chefs: Number of ",tolower(challengevar)," ",tolower(outcomevar),"s",sep=""),width=65)
         ,subtitle=str_wrap("If a chef was in Last Chance Kitchen at the Final Four, but later came back into the competition, they are included here. If there were multiple episodes with four chefs, the later episode of the season was chosen. The one chef that was in the Final Four but due to Last Chance Kitchen came in fifth is excluded. The statistics are only calculated up until the episode of the Final Four.",width = 85)) +
    theme(plot.title=element_text(size=28,face="bold",color="black")
          ,plot.subtitle = element_text(size=22,color="black"))

  # Graph one: boxplot
  graphone <-  graphdata %>%
    ggplot(aes(x=n)) +
    geom_boxplot() +
    xlab(paste0("Number of ",tolower(challengevar)," ",tolower(outcomevar),"s",sep="")) +
    scale_x_continuous(lim=c(0,max(graphdata$n,na.rm=T)+2)
                       , breaks = seq(0,max(graphdata$n,na.rm=T)+2,2)
                       ,labels =seq(0,max(graphdata$n,na.rm=T)+2,2)) +
    labs(title = str_wrap(paste0("\nDistribution of number of ",tolower(challengevar)," ",tolower(outcomevar),"s at time of Final Four",sep=""),width=100) ) +
    themestuff +
    theme(axis.text.y=element_blank())

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
    xlab(paste0("Average number of ",tolower(challengevar)," ",tolower(outcomevar),"s\nat time of Final Four",sep="")) +
    scale_x_continuous(lim=c(0,max(placementdata$mean)+2),breaks=seq(0,max(placementdata$mean)+2,1),labels=seq(0,max(placementdata$mean)+2,1)) +
    labs(title = "\nAverage by placement") +
    themestuff +
    geom_text(hjust=0,nudge_x = .1,size=6)

  # Graph 3: average by gender
  genderdata <-  graphdata %>%
    group_by(gender) %>%
    summarise(mean=mean(n),N=n()) %>%
    mutate(gender=paste0(gender," (N = ",N,")"))


  graphthree <-   genderdata %>%
    ggplot(aes(x=mean,y=gender,label = round(mean,1))) +
    geom_bar(stat="identity") +
    xlab(paste0("Average number of ",tolower(challengevar)," ",tolower(outcomevar),"s\nat time of Final Four",sep="")) +
    scale_x_continuous(lim=c(0,max(genderdata$mean)+2),breaks=seq(0,max(genderdata$mean)+2,1),labels=seq(0,max(genderdata$mean)+2,1)) +
    labs(title = "\nAverage by gender") +
    themestuff +
    geom_text(hjust=0,nudge_x = .1,size=6)


  # Graph 4: people with the most
  # threshold for this changes by variable
  if (challengevar == "Elimination" & outcomevar == "LOW") {
    threshold <- 5
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
    xlab(paste0("\nNumber of ",tolower(challengevar)," ",tolower(outcomevar),"s at time of Final Four",sep="")) +
    labs(title = paste0("\n\nChefs with the most ",tolower(challengevar)," ",tolower(outcomevar),"s at time of the Final Four",sep="")
         ,caption="Data and code at github.com/celevitz/topChef    Twitter @carlylevitz") +
    themestuff

  # bring all graphs together
  print(ggarrange(ggarrange(graphzero,graphone,heights=c(1.5,1),nrow=2),ggarrange(graphtwo,graphthree),graphfour,
            ncol=1,nrow=3,heights = c(1,1,2)))

  dev.print(png, file = paste(savedirectory,"TopFour_",challengevar,"_",outcomevar,".png",sep=""), width = 900, height = 1200)
  dev.off()


}



## Save all the graphs
topfourgraphs(challengevar="Elimination",outcomevar="HIGH")
topfourgraphs(challengevar="Elimination",outcomevar="WIN")
topfourgraphs(challengevar="Elimination",outcomevar="LOW")
topfourgraphs(challengevar="Quickfire",outcomevar="HIGH")
topfourgraphs(challengevar="Quickfire",outcomevar="WIN")







