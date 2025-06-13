rm(list=ls())
library(tidyverse)

directory <- "/Users/carlylevitz/Documents/Data/topChef/"

numchefsofinterest <- 3

challengewinsRaw <- read.csv(paste0(directory,"Top Chef - Challenge wins.csv"))%>%
  filter(series == "US")

chefdetails <- read.csv(paste0(directory,"Top Chef - Chef details.csv"))  %>%
  filter(series == "US" )

episodeinfo <- read.csv(paste0(directory,"Top Chef - Episode information.csv"))  %>%
  filter(series == "US" )

## Combine the challenge data with the # of chefs in the challenge
## Actually, i could just use the # of chefs with In Competition == TRUE

  numchefs <- challengewinsRaw %>%
    select(series,season,seasonNumber,episode,inCompetition,chef) %>%
    distinct() %>%
    #mutate(inepisode = sum(ifelse(inCompetition == "TRUE",1,0))) %>%
    group_by(series,season,seasonNumber,episode) %>%
    summarise(numberofchefs = sum(ifelse(inCompetition == "TRUE",1,0)))

  challengewins <- challengewinsRaw %>%
    select(!c(rating,inCompetition,immune)) %>%
    left_join(numchefs) %>%
    # remove challenge types we're not interested in
    filter(!(challengeType %in% c("Battle of the Sous Chefs"
                                  ,"Qualifying Challenge"))) %>%
    # simplify the challenge types & outcome types
    mutate(challengeType = ifelse(challengeType %in% c("Quickfire Elimination"
                                                   ,"Sudden Death Quickfire")
                                  ,"Elimination",challengeType)
           ,outcome = case_when(outcome %in% c("HIGH") ~ "HIGH"
                               ,outcome %in% c("WIN","WINNER") ~ "WIN"
                               ,outcome %in% c("RUNNER-UP","DISQUALIFIED"
                                               ,"WITHDREW") ~ "OUT"
                               ,outcome %in% c("DIDN'T COMPETE") ~ "IN"
                               ,TRUE ~ outcome))

## Scores across seasons with a given number of chefs
  scoresDetails <- challengewins %>%
    filter(numberofchefs > numchefsofinterest) %>%
    select(!numberofchefs) %>%
    group_by(series,season,seasonNumber,challengeType,outcome,chef) %>%
    summarise(n=n()) %>%
    # calculate the points
    # Multiply by the number of times they got each of those outcomes
    mutate(points = (case_when(outcome == "HIGH" & challengeType ==
                                                            "Elimination" ~ 3
                      ,outcome == "WIN" & challengeType == "Elimination" ~ 7
                      ,outcome == "LOW" & challengeType == "Elimination" ~ -3
                      ,outcome == "OUT" & challengeType == "Elimination" ~ -7
                      ,outcome == "HIGH" & challengeType == "Quickfire" ~ 2
                      ,outcome == "WIN" & challengeType == "Quickfire" ~ 4
                    ,outcome == "LOW" & challengeType == "Quickfire" ~ -2))*n
           ) %>%
    ungroup() %>%
    left_join(chefdetails %>%
                select(series,season,seasonNumber,chef,placement) %>%
                mutate(placement = as.numeric(placement))) %>%
    mutate(seasonWinner = ifelse(placement == 1, 1, 0))

  # total score
  # this already takes into account the # of chefs in the game -
  # see "numchefsofinterest
  scores <- scoresDetails%>%
    group_by(series,season,seasonNumber,chef,placement,seasonWinner) %>%
    summarise(points=sum(points,na.rm=T))


## Keep just the number of chefs of interest and see if that's related to
  ## winning
  reg <- glm(scores$seasonWinner[scores$placement <= 3] ~
               scores$points[scores$placement <= 3])
  summary(reg)


  # scores at final 3 -- who is the underdog and do they win?
  final3 <- scores %>%
    filter(placement <= 3 | (seasonNumber == 22 & chef %in% c("Bailey Sullivan"
                                                              ,"Shuai Wang"
                                                          ,"Tristen Epps"))) %>%
    # for now, put season winner as 0 for the Season 22 chefs
    mutate(seasonWinner = ifelse(seasonNumber == 22,0,seasonWinner)) %>%
    # get the smallest score of each season's final 3
    # flag that one as the underdog
    ungroup() %>% group_by(series,season,seasonNumber) %>%
    mutate(min=min(points,na.rm=T)
           ,max=max(points,na.rm=T)
           ,difference = max-min
           ,stdev = sd(points,na.rm=T)
           ,average = median(points,na.rm=T)
           ,flag = ifelse(abs((points-average)/average) > .5,"flag","fine")
           ,underdog = case_when(points == min & flag == "flag" ~ "Underdog"
                                 ,points == max & flag == "flag" ~ "Overdog"
                                 ,TRUE ~ "dog")
           )

    table(final3$underdog,final3$seasonWinner)

    final3 %>%
    ggplot(aes(x=seasonNumber,y=points,label = chef
               ,color=underdog,shape=as.character(seasonWinner)))  +
      geom_point() +
      scale_y_continuous(lim=c(-20,60))














