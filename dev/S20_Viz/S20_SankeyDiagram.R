## Carly Levitz
## Date written: 2023-05-20
## Date updated: 2023-05-20
## Purpose: Create a sankey diagram for this season of top chef


#####
## Set up
rm(list=ls())
devtools::install_github("celevitz/topChef")
devtools::install_github("davidsjoberg/ggsankey")
library(tidyverse); library(topChef); library(ggsankey); library(ggalluvial)

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
    ggplot(alluvialdata,
           aes(x = epichallenge, stratum = outcome, alluvium = chef,
               fill = chef, label = chef)) +
    #scale_fill_brewer(type = "qual", palette = "Set2") +
    geom_flow(stat = "alluvium", lode.guidance = "frontback",
              color = "darkgray") +
    geom_stratum() +
    theme(legend.position = "bottom") +
    ggtitle("Top Chef Season 20: World All Stars")

############ SANKEY #######################################################
## Manual data -- this is creating errors in the actual plotting
manualsankey <- topChef::challengewins %>%
  filter(series=="US" & szn == "World All Stars") %>%
  # for sorting purposes, make the episode a two-digit character
  mutate(episodechar=case_when(episode <= 9 ~paste0("0",as.character(episode))
                           ,TRUE ~as.character(episode) )
         # to get the "next node" of the sankey, tick up the episode number
         ,episodechar_tickup=case_when(episode+1 <= 9 ~paste0("0",as.character(episode+1))
                               ,TRUE ~as.character(episode+1) )
         # make the x label a combination of episode number & challenge type
         ,x=paste0("Ep. ",episodechar,":\n",challenge_type)
         # the next x will be an elimination of the same episode # if it's a quickfire, and the quickfire of the next episode if it's an elimination
         # if the subsequent episode doesn't have a quickfire, then the elimination challenge of preceding episode is followed by another elimination
         ,next_x=case_when(challenge_type == "Elimination" & !(episode %in% c(2,5,8))~ paste0("Ep. ",episodechar_tickup,":\nQuickfire")
                          ,challenge_type == "Elimination" & episode %in% c(2,5,8)~ paste0("Ep. ",episodechar_tickup,":\nElimination")
                          ,challenge_type == "Quickfire" ~ paste0("Ep. ",episodechar,":\nElimination")
                          ,TRUE ~ "NEED TO EDIT")
         # for people who are out, we want them to always be listed as out
         ,outcome=ifelse(in.competition == "FALSE","OUT",outcome)
         # create placeholder for next node
         ,next_node=""
         # for last episode of season, have next x & nex node be blank
         ,next_x=ifelse(episode == max(episode) & challenge_type == "Elimination",NA,next_x)
         ,next_node=ifelse(episode == max(episode) & challenge_type == "Elimination",NA,next_node)) %>%
  rename(node=outcome) %>%
  select(chef,node,x,next_x,next_node)

  # The next node is the outcome from the next challenge
  # For the last episode of the season, there won't be a next X or next Node
    for (c in unique(manualsankey$chef) ) {
      for (nextchallenge in unique(manualsankey$next_x[!(is.na(manualsankey$next_x ))])) {
        nextchallengeoutcome <-  manualsankey$node[manualsankey$chef == c & manualsankey$x == nextchallenge]
        manualsankey$next_node[manualsankey$chef == c & manualsankey$next_x == nextchallenge] <- nextchallengeoutcome
      }
    }

###############
## ggsankey for just Elimination challenges
elimchallengewins <- topChef::challengewins %>%
  filter(series=="US" & szn == "World All Stars" & challenge_type == "Elimination") %>%
  # for sorting purposes, make the episode a two-digit character
  mutate(episodechar=case_when(episode <= 9 ~paste0("0",as.character(episode))
                               ,TRUE ~as.character(episode) )
         # make the x label a combination of episode number & challenge type
         ,epichallenge=paste("Ep. ",episodechar,":\n",challenge_type,sep="")
         # want everyone to be listed as out after they are eliminated
         ,outcome=ifelse(in.competition == "FALSE","Don't plot",outcome)
         ) %>%
  select(!(c(szn,sznnumber,series,in.competition,challenge_type,rating,episode,episodechar))) %>%
  # make it wide so that we can then "make longer" with the proper function from the sankey package
  pivot_wider(names_from = epichallenge,values_from = outcome)



sankeydata <- elimchallengewins %>%
  group_by(chef) %>%
  group_modify(~make_long(.x,names(elimchallengewins)[names(elimchallengewins) != "chef"]))



  # make the nodes a factor
  sankeydata$node <- factor(sankeydata$node,levels=c("Don't plot","OUT","LOW","IN","HIGH","WIN"))
  sankeydata$next_node <- factor(sankeydata$next_node,levels=c("Don't plot","OUT","LOW","IN","HIGH","WIN"))



sankeydata %>%
ggplot(aes(x = x,
           next_x = next_x,
           node = node,
           next_node = next_node,
           fill = node)) +
  geom_sankey(flow.alpha=.5) +
  # scale_fill_manual(values=c("Don't plot"= "transparent"
  #                            ,"OUT" = "#fc7d0b"
  #                            ,"LOW" = "#ffbc69"
  #                            ,"IN" = "gray90"
  #                            ,"HIGH" = "#a3cce9"
  #                            ,"WIN" = "#1170AA")) +
  theme_minimal() +
  xlab("") +
  theme(panel.grid=element_blank()
        ,axis.text.y=element_blank()) +
  guides(fill=guide_legend(title="Outcome"))








