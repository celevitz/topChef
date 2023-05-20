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
             # make the x label a combination of episode number & challenge type
             ,epichallenge=paste("Ep. ",episodechar,":\n",challenge_type,sep="")
             # want everyone to be listed as out after they are eliminated
             ,outcome=ifelse(in.competition == "FALSE","Don't plot",outcome)
             ,freq=1 ) %>%
      select(!(c(szn,sznnumber,series,in.competition,challenge_type,rating,episode,episodechar)))

      alluvialdata$outcome <- factor(alluvialdata$outcome,levels=c("Don't plot","OUT","LOW","IN","HIGH","WIN"))


  # Viz
      alluvialdata %>%
          ggplot(aes(x = epichallenge, stratum = outcome, alluvium = chef,
                     fill = chef, label = chef)) +
          geom_flow(stat = "alluvium", lode.guidance = "frontback",
                    color = "darkgray") +
          geom_stratum() +
          ggtitle("Top Chef Season 20: World All Stars") +
          theme_minimal() +
          xlab("") +
          theme(panel.grid=element_blank()
                ,axis.text.y=element_blank()
                ,legend.position="bottom") +
          guides(fill=guide_legend(title="Outcome"))


      # scale_fill_manual(values=c("Don't plot"= "transparent"
      #                            ,"OUT" = "#fc7d0b"
      #                            ,"LOW" = "#ffbc69"
      #                            ,"IN" = "gray90"
      #                            ,"HIGH" = "#a3cce9"
      #                            ,"WIN" = "#1170AA"))
