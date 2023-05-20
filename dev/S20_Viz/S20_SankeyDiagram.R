## Carly Levitz
## Date written: 2023-05-20
## Date updated: 2023-05-20
## Purpose: Create a sankey diagram for this season of top chef


#####
## Set up
rm(list=ls())
devtools::install_github("celevitz/topChef")
library(tidyverse); library(topChef); library(ggalluvial)

savedirectory <- "/Users/carlylevitz/Documents/Data/TCSeason20/Episode-specific/"

#############################################################################
# Alluvial plot
  # dataset
    alluvialdata <- topChef::challengewins %>%
      filter(series=="US" & szn == "World All Stars") %>%
      # for sorting purposes, make the episode a two-digit character
      mutate(episodechar=case_when(episode <= 9 ~paste0("0",as.character(episode))
                                   ,TRUE ~as.character(episode) )
             # for sorting purposes, make quickfire come before elimination alphabetically
             ,challenge_type=case_when(challenge_type == "Quickfire" ~ "AQuickfire"
                                       ,TRUE ~ challenge_type)
             # make the x label a combination of episode number & challenge type
             ,epichallenge=paste("Ep. ",episodechar,":\n",challenge_type,sep="")
             # want everyone to be listed as out after they are eliminated
             ,outcome=ifelse(in.competition == "FALSE","OUT",outcome)
             # but if they're OUT, put them as low in the episode that they were eliminated
             ,outcome=ifelse(in.competition == "TRUE" & outcome == "OUT","LOW",outcome)
             ,freq=1 ) %>%
      select(!(c(szn,sznnumber,series,in.competition,rating,episode)))

      alluvialdata$outcome <- factor(alluvialdata$outcome,levels=c("OUT","LOW","IN","HIGH","WIN"))

      alluvialdata <- alluvialdata[order(alluvialdata$episodechar,alluvialdata$challenge_type,alluvialdata$outcome,alluvialdata$chef),]

  # Viz draft 1
      alluvialdata %>%
        ggplot(aes(x = epichallenge, stratum = outcome, alluvium = chef,
                   fill = outcome, label = chef)) +
        geom_flow(stat = "alluvium", lode.guidance = "frontback") +
        geom_stratum()

  # Viz draft 2
      alluvialdata %>%
          ggplot(aes(x = epichallenge, stratum = outcome, alluvium = chef,
                     fill = outcome, label = chef)) +
          geom_flow(stat = "alluvium", lode.guidance = "frontback") +
          geom_stratum(aes(fill=outcome))+
          ggtitle("Top Chef Season 20: World All Stars") +
          theme_minimal() +
          xlab("") +
          theme(panel.grid=element_blank()
                ,axis.text.y=element_blank()
                ,legend.position="bottom") +
          guides(fill=guide_legend(title="Outcome")) +
          scale_fill_manual(values=c("OUT" = "#fc7d0b"
                                     ,"LOW" = "#ffbc69"
                                     ,"IN" = "gray90"
                                     ,"HIGH" = "#a3cce9"
                                     ,"WIN" = "#1170AA"))


    # Viz draft 3
      alluvialdata %>%
        ggplot(aes(x = epichallenge, stratum = outcome, alluvium = chef,
                   fill = outcome, label = chef)) +
        geom_flow(aes(color=chef,fill=chef),stat = "alluvium", lode.guidance = "frontback") +
        geom_stratum(aes(fill=outcome),color="white" )+
        ggtitle("Top Chef Season 20: World All Stars") +
        theme_minimal() +
        xlab("") +
        theme(panel.grid=element_blank()
              ,axis.text.y=element_blank()
              ,legend.position="bottom")  +
        guides(fill=guide_legend(title="Outcome")) +
        scale_fill_manual(values=c("OUT" = "#fc7d0b"
                                   ,"LOW" = "#ffbc69"
                                   ,"IN" = "gray90"
                                   ,"HIGH" = "#a3cce9"
                                   ,"WIN" = "#1170AA"))
    # Viz draft 4
      alluvialdata %>%
        ggplot(aes(x = epichallenge, stratum = outcome, alluvium = chef)) +
        geom_flow(aes(fill=chef)) +
        geom_stratum(aes(fill=outcome),width=.3) +
        #geom_lode()+
        ggtitle("Top Chef Season 20: World All Stars") +
        theme_minimal() +
        xlab("") +
        scale_x_discrete(breaks = unique(alluvialdata$epichallenge),labels=gsub("AQuickfire","Quickfire",unique(alluvialdata$epichallenge))) +
        theme(panel.grid=element_blank()
              ,axis.text.y=element_blank()
              ,legend.position="bottom")  +
        guides(fill=guide_legend(title="Outcome")) +
        scale_fill_manual(values=c("OUT" = "gray95"
                                   ,"LOW" = "#ffbc69"
                                   ,"IN" = "#a3cce9"
                                   ,"HIGH" = "#5fa2ce"
                                   ,"WIN" = "#1170AA"))







