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

episodenumber <- 9
numberofelimchalls <- 9
numberofquickfirechalls <- 6
eliminatedchefs <- c("David Murphy"
                     ,"Valentine Howell Jr.","Kenny Nguyen"
                     ,"Charly Pierre"
                     ,"Kaleena Bliss","Alisha Elenz"
                     ,"Rasika Venkatesa","Kevin D'Andrea"
                     ,"Amanda Turner","Laura Ozyilmaz")

## Stats about S21
s21challstats <- weightedindex("US",21,numberofelimchalls,numberofquickfirechalls) %>%
  mutate(eliminated = ifelse(chef %in% eliminatedchefs,"Out","In the competition")) %>%
  select(!c(season,seasonNumber,series,placement)) %>%
  arrange(eliminated,desc(indexWeight))

s21challstats <- s21challstats[,c("chef","eliminated","Quickfire.WIN","Quickfire.HIGH","Quickfire.LOW"
                                  ,"Elimination.WIN","Elimination.HIGH","Elimination.LOW"
                                  ,"indexWeight")]


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
temp <- weightedindex("US",1,numberofelimchalls,numberofquickfirechalls)
for (season in seq(2,21,1)) {
  temp <- rbind(temp,weightedindex("US",season,numberofelimchalls,numberofquickfirechalls))

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
      full_join(weightedindex("US",21,5,3) %>%
                  select (chef,indexWeight) %>%
                  rename(indexWeight5 = indexWeight) ) %>%
      mutate(indexWeight5 = ifelse(is.na(indexWeight5),0,indexWeight5)) %>%
      full_join(weightedindex("US",21,6,4) %>%
                  select (chef,indexWeight) %>%
                  rename(indexWeight6 = indexWeight) ) %>%
      mutate(indexWeight6 = ifelse(is.na(indexWeight6),0,indexWeight6)) %>%
      full_join(weightedindex("US",21,7,5) %>%
                  select (chef,indexWeight) %>%
                  rename(indexWeight7 = indexWeight) ) %>%
      mutate(indexWeight7 = ifelse(is.na(indexWeight7),0,indexWeight7)) %>%
      full_join(weightedindex("US",21,8,5) %>%
                  select (chef,indexWeight) %>%
                  rename(indexWeight8 = indexWeight) ) %>%
      mutate(indexWeight8 = ifelse(is.na(indexWeight8),0,indexWeight8)) %>%
      full_join(weightedindex("US",21,9,6) %>%
                  select (chef,indexWeight) %>%
                  rename(indexWeight9 = indexWeight) ) %>%
      mutate(indexWeight9 = ifelse(is.na(indexWeight9),0,indexWeight9)) %>%
      full_join(weightedindex("US",21,10,7) %>%
                  select (chef,indexWeight) %>%
                  rename(indexWeight10 = indexWeight) ) %>%
      mutate(indexWeight10 = ifelse(is.na(indexWeight10),0,indexWeight10)) %>%

      arrange(desc(indexWeight1))  %>%
      mutate(rank1=rank(-indexWeight1,ties.method = "min")) %>%
      arrange(desc(indexWeight2)) %>%
      mutate(rank2=rank(-indexWeight2,ties.method = "min")) %>%
      arrange(desc(indexWeight3)) %>%
      mutate(rank3=rank(-indexWeight3,ties.method = "min")) %>%
      arrange(desc(indexWeight4)) %>%
      mutate(rank4=rank(-indexWeight4,ties.method = "min")) %>%
      arrange(desc(indexWeight5)) %>%
      mutate(rank5=rank(-indexWeight5,ties.method = "min")) %>%
      arrange(desc(indexWeight6)) %>%
      mutate(rank6=rank(-indexWeight6,ties.method = "min")) %>%
      arrange(desc(indexWeight7)) %>%
      mutate(rank7=rank(-indexWeight7,ties.method = "min")) %>%
      arrange(desc(indexWeight8)) %>%
      mutate(rank8=rank(-indexWeight8,ties.method = "min")) #%>%
      #arrange(desc(indexWeight9)) %>%
      #mutate(rank9=rank(-indexWeight9,ties.method = "min")) %>%
      #arrange(desc(indexWeight10)) %>%
      #mutate(rank9=rank(-indexWeight10,ties.method = "min"))

  # make the data long form to make it easier to plot
  episodespecificlong <- episodespecific %>%
    pivot_longer(!chef,names_to = "measure",values_to = "value") %>%
    mutate(episode = gsub("rank","",gsub("indexWeight","",measure))
           ,out = ifelse(chef %in% eliminatedchefs
                         ,"out","in")
           ,rank_yvalue = 16-value+1
           ,rank_yvalue = ifelse(grepl("indexWeight",measure),NA,rank_yvalue))

  # label the last data point for each chef
      episodespecificlong <- episodespecificlong %>%
        group_by(chef) %>%
        mutate(maxepisode = max(as.numeric(episode))
               ,label = ifelse(as.numeric(episode) = maxepisode,chef,NA ))




  episodespecificlong$label[episodespecificlong$episode == max(as.numeric(episodespecificlong$episode))] <- episodespecificlong$chef[episodespecificlong$episode == max(as.numeric(episodespecificlong$episode))]

  summary(episodespecificlong$value[grepl("indexWeight",episodespecificlong$measure)])

  # graph for the index weight by episode
    episodespecificlong %>%
      filter(grepl("indexWeight",measure)) %>%
      ggplot(aes(x=episode,y=value,label=chef),color=out) +
      geom_text(hjust=0.5,size=4,aes(color=out)) +
      scale_color_manual(values = c("black","gray75")) +
      theme_minimal() +
      labs(title=paste0("Top Chef 21 Weighted Index Scores Through Episode ",episodenumber)
           ,subtitle="Higher scores are better\n")+
      ylab("Index Score") + xlab("Episode") +
      scale_y_continuous(lim=c(-15,26),breaks = seq(-15,26,5)) +
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
      labs(title=paste0("Top Chef 21 Rank of Weighted Index Scores Through Episode ",episodenumber)
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

    allseasons <- weightedindex("US",1,numberofelimchalls,numberofquickfirechalls)
    for (season in seq(2,21,1)) {
      allseasons <- rbind(allseasons,weightedindex("US",season,numberofelimchalls,numberofquickfirechalls))

    }

    allseasons <- allseasons %>%
      select(chef,season,seasonNumber,placement,indexWeight) %>%
      arrange(desc(indexWeight)) %>%
      mutate(rank=rank(-indexWeight,ties.method = "min"))

    row.names(allseasons) <- NULL

    allseasons %>%
      filter((rank <= 10 | seasonNumber == 21 | placement == 1 |
                chef %in% c("Sara B.","Justin S.","Shota N.")) &
               !(chef %in% eliminatedchefs)) %>%
      select(!season) %>%
    relocate(rank,.before=chef)

    allseasons %>% filter(placement ==1)

    ## what were the standard deviations of the scores for each season?
    allseasons %>%
      group_by(seasonNumber,season) %>%
      summarise(mean=mean(indexWeight)
                ,median=median(indexWeight)
                ,stdev = sd(indexWeight)) %>%
      print(n=21)


###############
## keeping score constant at same # of challenges as in Buddha's season 19: 14 elims, 10 qfs
    allseasons <- weightedindex("US",1,14,10)
    for (season in seq(2,20,1)) {
      allseasons <- rbind(allseasons,weightedindex("US",season,14,10))

    }

    allseasons %>%
      select(chef,seasonNumber,placement,indexWeight) %>%
      arrange(desc(indexWeight)) %>%
      group_by(seasonNumber) %>%
      mutate(stdev = sd(indexWeight,na.rm=T)) %>%
      filter(indexWeight >=42)

    allseasons %>%
      group_by(seasonNumber) %>%
      summarise(stdev=sd(indexWeight,na.rm=T))


### 7 episodes in , ish
    allseasons <- weightedindex("US",1,7,7)
    for (season in seq(2,20,1)) {
      allseasons <- rbind(allseasons,weightedindex("US",season,7,7))

    }

    allseasons %>%
      group_by(seasonNumber) %>%
      summarise(stdev=sd(indexWeight,na.rm=T))

