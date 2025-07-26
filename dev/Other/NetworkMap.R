# Carly Levitz
# 2024-04-17

########################################################################################
## 1. Set up (packages, data)
rm(list=ls())
library(tidyverse)
library(devtools)
library(igraph)
library(ggpubr)
library(glue)
library(ggtext)
library(showtext)
library(ggraph)
library(tidygraph)

devtools::install_github("celevitz/topChef")
devtools::install_github("celevitz/touRnamentofchampions")

tcChefs <- topChef::chefdetails %>%
  select(series,season,name)

TOCchefs <- touRnamentofchampions::results %>%
  select(chef,season) %>%
  distinct() %>%
  mutate(series = "TOC"
         ,season = paste0("TOC ",as.character(season))) %>%
  rename(name = chef)


allchefs <- tcChefs %>% rbind(TOCchefs) %>%
  # for now, I just want to look at the top chefs
  filter(series %in% c("US") )

########################################################################################
## 2. Create the dataset that will feed into the node & vertex data
  combined <- tcChefs[1,]
  combined$name <- NULL
  combined$source <- combined$target <- NA

  # for each series
  # for each season
  # for each chef in that series-season, create a to-from relationship with
  #   each of the other chefs
  for (seriesname in unique(allchefs$series)) {
    for (seasonname in unique(allchefs$season[allchefs$series == seriesname])) {
      for (chefname in unique(allchefs$name[allchefs$series == seriesname &
                                            allchefs$season == seasonname])) {
          temp <- allchefs[allchefs$series == seriesname &
                             allchefs$season == seasonname,"name"] %>%
            rename(target=name)

          temp$series <- seriesname
          temp$season <- seasonname
          temp$source <- chefname

          combined <- rbind(combined,temp)
      }
    }
  }

  # drop the first row which is empty, plus when target == source
  combined <- combined %>% filter(!(is.na(source)) & target != source)

  ## do a Top Chef only version of the data, sort of like a circle pack format
    cp <- topChef::chefdetails %>%
      select(series,seasonNumber,name) %>%
      filter(series == "US") %>%
      rename(source=seasonNumber,target=name) %>%
      mutate(type = "seasonsource"
             ,importance=1.1) %>%
      rbind(topChef::chefdetails %>%
              select(series,seasonNumber,name) %>%
              filter(series == "US")  %>%
              rename(target=seasonNumber,source=name)  %>%
              mutate(type = "namesource") %>%
              full_join(
                # Get "importance" (i.e., number of times someone is in the data)
                topChef::chefdetails %>%
                  select(series,seasonNumber,name) %>%
                  filter(series == "US")   %>%
                  rename(source=name) %>%
                  group_by(source) %>%
                  summarise(importance = n())
              ) ) %>%
      select(!series)

########################################################################################
## 3. network graphs
  ## set up data for network graphs
  # (just for connections in here more than 1x, if you're doing TOC)
  # edges
  edges <- combined %>%
    group_by(source,target) %>%
    summarise(importance = n())
    # %>%
    #   filter(importance >1)

    # shorten the names
    # RIGHT NOW THIS ISN"T WORKING -- check out edges %>% view()
    # mutate(tempsource = paste0(str_split(source," ")[[1]][1]," "
    #                            ,substr(str_split(source," ")[[1]][2],1,1))
    #        ,temptarget = paste0(str_split(target," ")[[1]][1]," "
    #                             ,substr(str_split(target," ")[[1]][2],1,1))) %>%
    # ungroup() %>%
    # select(!c(source,target)) %>%
    # rename(source=tempsource,target=temptarget)

  # vertices
  vertices <- data.frame(edges %>% ungroup() %>%
                        group_by(source) %>%
                        summarise(n=n()) %>%
                        select(!n) %>%
                        rename(name= source))

  # graph object
  graph <- graph_from_data_frame(d=edges
                                   , vertices=vertices
                                   , directed=F)
  # Calculate node importance, e.g., based on degree
  V(graph)$importance <- degree(graph, mode = 'in')

  threshold <- 40

  # circlepack edges & vertices
  edges <- cp %>% filter(type == "namesource")
  vertices <- cp %>%
    select(source,type,importance) %>%
    rename(name=source) %>% distinct()

  cp_graph <- graph_from_data_frame(d=edges
                                    , vertices=vertices
                                    , directed=F)

########################################################################################
## 4. Visualization

  # Aesthetics
  V(cp_graph)$color <- case_when(V(cp_graph)$importance == 1 ~ "darkcyan"
                                 ,V(cp_graph)$importance == 1.1 ~ "orange"
                                 ,V(cp_graph)$importance == 2 ~ "white"
                                 ,V(cp_graph)$importance == 3 ~ "white")
  V(cp_graph)$frame.color <- case_when(V(cp_graph)$importance == 1 ~ "darkcyan"
                                       ,V(cp_graph)$importance == 1.1 ~ "orange"
                                       ,V(cp_graph)$importance == 2 ~ "navyblue"
                                       ,V(cp_graph)$importance == 3 ~ "navyblue")
  V(cp_graph)$label.cex <- case_when(V(cp_graph)$importance == 1.1~  .5
                                     ,V(cp_graph)$importance > 1.1~  .4
                                     ,TRUE ~ 0.01)
  V(cp_graph)$size <- case_when(V(cp_graph)$importance == 1 ~ 1
                                ,V(cp_graph)$importance == 1.1 ~ 5
                                ,V(cp_graph)$importance == 2 ~ .5
                                ,V(cp_graph)$importance == 3 ~ .5)

  # Plot
  plot.igraph(cp_graph
       ,layout = layout.fruchterman.reingold
       ,asp = 0
       ,main="Top Chef Network across 21 Seasons"
       ,sub="IG: @carly.sue.bear /// X: @carlylevitz"
       )



