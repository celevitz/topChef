rm(list=ls())
library(tidyverse)
library(openxlsx)
library(topChef)
library(ggplot2)
library(devtools)
devtools::install_github("celevitz/topChef")

challengewins <- topChef::challengewins %>%
  filter(series == "US")

chefdetails <- topChef::chefdetails %>%
  filter(series == "US" )

episodenumber <- 4
numberofelimchalls <- 4
numberofquickfirechalls <- 2

## at the end of their seasons

allseasons <- weightedindex("US",1,20,20)
for (season in seq(2,20,1)) {
  allseasons <- rbind(allseasons,weightedindex("US",season,20,20))

}


## at the shortest # of elim & qf challenges
shortestseason <- weightedindex("US",1,11,8)
for (season in seq(2,20,1)) {
  shortestseason <- rbind(shortestseason,weightedindex("US",season,11,8))

}

data.frame(allseasons %>%
             filter(placement == 1) %>%
             select(chef,season,seasonNumber,indexWeight) %>%
             rename(indexWeightFullSeason = indexWeight) %>%
             full_join(shortestseason %>%
                         filter(placement == 1) %>%
                         select(chef,season,seasonNumber,indexWeight) %>%
                         rename(indexWeightShortSeason=indexWeight)) %>%
             arrange(desc(indexWeightFullSeason)))




#############
## Season 21 info
# compare season 21 to all others
temp <- weightedindex("US",1,3,2)
for (season in seq(2,21,1)) {
  temp <- rbind(temp,weightedindex("US",season,3,2))

}

# Season 21 over time
  # I can't get the index functino to work for 0 quickfires
    ep1scores <- chefdetails %>%
      filter(seasonNumber == 21 & series == "US") %>%
      select(chef) %>%
      mutate(indexWeight1=case_when(chef == "Manny Barella" ~ 7
                                   ,chef %in% c("Michelle Wallace","Danny Garcia") ~ 3
                                   ,chef %in% c("Amanda Turner","Kenny Nguyen") ~ -3
                                   ,chef == "David Murphy" ~ -7
                                   ,TRUE ~ 0))

  # combine the season 21 scores by episode
    episodespecific <- ep1scores %>%
      full_join(weightedindex("US",21,2,1) %>%
                  select (chef,indexWeight) %>%
                  rename(indexWeight2 = indexWeight) ) %>%
      mutate(indexWeight2 = ifelse(is.na(indexWeight2),0,indexWeight2)) %>%
      full_join(weightedindex("US",21,3,2) %>%
                  select (chef,indexWeight) %>%
                  rename(indexWeight3 = indexWeight) ) %>%
      mutate(indexWeight3 = ifelse(is.na(indexWeight3),0,indexWeight3)) %>%
      full_join(weightedindex("US",21,4,2) %>%
                  select (chef,indexWeight) %>%
                  rename(indexWeight4 = indexWeight) ) %>%
      mutate(indexWeight4 = ifelse(is.na(indexWeight4),0,indexWeight4)) %>%
      arrange(desc(indexWeight1))  %>%
      mutate(rank1=rank(-indexWeight1,ties.method = "min")) %>%
      arrange(desc(indexWeight2)) %>%
      mutate(rank2=rank(-indexWeight2,ties.method = "min")) %>%
      arrange(desc(indexWeight3)) %>%
      mutate(rank3=rank(-indexWeight3,ties.method = "min")) %>%
      arrange(desc(indexWeight4)) %>%
      mutate(rank3=rank(-indexWeight4,ties.method = "min"))

  # make the data long form to make it easier to plot
  episodespecificlong <- episodespecific %>%
    pivot_longer(!chef,names_to = "measure",values_to = "value") %>%
    mutate(episode = gsub("rank","",gsub("indexWeight","",measure))
           ,out = ifelse(chef %in% c("Soo Ahn","David Murphy"
                                     ,"Valentine Howell Jr.","Kenny Nguyen"
                                     ,"Kaleena Bliss","Alisha Elenz")
                         ,"out","in")
           ,rank_yvalue = 16-value+1
           ,rank_yvalue = ifelse(grepl("indexWeight",measure),NA,rank_yvalue))

  summary(episodespecificlong$value[grepl("indexWeight",episodespecificlong$measure)])

  # graph for the index weight by episode
    episodespecificlong %>%
      filter(grepl("indexWeight",measure)) %>%
      ggplot(aes(x=episode,y=value,label=chef),color=out) +
      geom_text(hjust=0.5,size=4,aes(color=out)) +
      scale_color_manual(values = c("black","gray75")) +
      theme_minimal() +
      labs(title=paste0("Top Chef 21 Weighted Index Scores Through Episode 4")
           ,subtitle="Higher scores are better\n")+
      ylab("Index Score") + xlab("Episode") +
      scale_y_continuous(lim=c(-15,20),breaks = seq(-15,20,5)) +
      #scale_x_continuous(lim=c(0,20),breaks=seq(1,18,2),labels = seq(1,18,2)) +
      theme(panel.grid = element_blank()
            ,axis.text.x=element_text(size=12,color="black")
            ,axis.text.y=element_text(size=12,color="black")
            ,axis.ticks=element_line(color="gray15")
            ,axis.line=element_line(color="gray15")
            ,legend.position = "none")

  # graph for the rank of index weight by episode
    episodespecificlong %>%
      filter(grepl("rank",measure)) %>%
      ggplot(aes(x=episode,y=rank_yvalue,label=chef),color=out) +
      geom_text(hjust=0.5,size=4,aes(color=out)) +
      scale_color_manual(values = c("black","gray75")) +
      theme_minimal() +
      labs(title=paste0("Top Chef 21 Rank of Weighted Index Scores Through Episode 4")
           ,subtitle="Lower ranks scores are better so I've put them at the top of the graph\n")+
      ylab("Rank of index score (lower is better)") + xlab("Episode") +
      scale_y_continuous(lim=c(1,16),breaks = seq(1,16,1),labels=seq(16,1,-1)) +
      #scale_x_continuous(lim=c(0,20),breaks=seq(1,18,2),labels = seq(1,18,2)) +
      theme(panel.grid = element_blank()
            ,axis.text.x=element_text(size=12,color="black")
            ,axis.text.y=element_text(size=12,color="black")
            ,axis.ticks=element_line(color="gray15")
            ,axis.line=element_line(color="gray15")
            ,legend.position = "none")


dev.off()
## Where do current chefs stand among other seasons' chefs?

    allseasons <- weightedindex("US",1,4,2)
    for (season in seq(2,21,1)) {
      allseasons <- rbind(allseasons,weightedindex("US",season,4,2))

    }

    allseasons <- allseasons %>%
      select(chef,season,seasonNumber,placement,indexWeight) %>%
      arrange(desc(indexWeight)) %>%
      mutate(rank=rank(-indexWeight,ties.method = "min"))

    row.names(allseasons) <- NULL

    allseasons %>%
      filter(rank <= 10 | seasonNumber == 21)

    allseasons %>% filter(placement ==1)






