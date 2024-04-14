rm(list=ls())
library(tidyverse)
library(devtools)
library(igraph)
library(ggpubr)

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
  filter(series %in% c("US") &
           season != "Wisconsin")


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


############################################
## network graphs
  ## set up data for network graphs
  # (just for connections in here more than 1x, if you're doing TOC)
  links <- combined %>%
    group_by(source,target) %>%
    summarise(importance = n()) %>%
    # %>%
    #   filter(importance >1)

    # shorten the names
    # RIGHT NOW THIS ISN"T WORKING -- check out links %>% view()
    # mutate(tempsource = paste0(str_split(source," ")[[1]][1]," "
    #                            ,substr(str_split(source," ")[[1]][2],1,1))
    #        ,temptarget = paste0(str_split(target," ")[[1]][1]," "
    #                             ,substr(str_split(target," ")[[1]][2],1,1))) %>%
    # ungroup() %>%
    # select(!c(source,target)) %>%
    # rename(source=tempsource,target=temptarget)


  nodes <- data.frame(links %>% ungroup() %>%
                        group_by(source) %>%
                        summarise(n=n()) %>%
                        select(!n) %>%
                        rename(name= source))

  network <- graph_from_data_frame(d=links
                                   , vertices=nodes
                                   , directed=F)

  # V(network)$color <- ifelse(V(network)$colorid == 1, "white", "white")
  # V(network)$frame.color <- ifelse(V(network)$colorid == 1, "white", "white")
  # V(network)$shape <- ifelse(V(network)$colorid == 1, "rectangle", "circle")
  # V(network)$size <- ifelse(V(network)$colorid == 1, 10, 3)
  # V(network)$size2 <- ifelse(V(network)$colorid == 1, 3, 3)
  # V(network)$label.cex <- ifelse(V(network)$colorid == 1, 1.3, .7)
  # V(network)$label.cex <- ifelse(V(network)$colorid == 1, 1, .8)
  # V(network)$label.color <- ifelse(V(network)$colorid == 1, "navyblue", "black")

  V(network)$label.cex <- .5
  V(network)$size <- 3
  plot(network
       ,edge.width = E(network)$importance*.1
       ,vertex.color="white"
       ,layout=layout.fruchterman.reingold)

  #plot(network
       #,edge.width=E(network)$importance*.4
       #,edge.color=my_color
       #,edge.arrow.size=.5
       #, edge.curved=seq(-0.5, 0.5, length = ecount(network))
       #,vertex.color="gray90"
       #,vertex.label.cex=1.3
       #,vertex.size=8
       #,vertex.size2=3
       #,main = "ORG Network"
       #,sub="@carly.sue.bear"
       #,layout=layout.fruchterman.reingold
       #, main="fruchterman.reingold"
  #)

