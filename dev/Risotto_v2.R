## Analyze dish data

rm(list=ls())
library(gpubr)
library(openxlsx)
library(ggplot2)
library(tidyverse)

directory <- "/Users/carlylevitz/Documents/Data/"

dishesraw <- read.csv(paste0(directory
                             ,"/topChef/Top Chef - Dishes wide form with classifications.csv")
                      ,header = TRUE) %>%
  mutate(topbottom = case_when(outcome %in% c("WIN","WINNER","HIGH") ~ "Top"
                             ,outcome %in% c("LOW","OUT","RUNNER-UP") ~ "Bottom"
                               ,TRUE ~ "In the middle")
         ,challengeType = ifelse(challengeType %in% c("Quickfire Elimination"
                                                   ,"Sudden Death Quickfire")
                                 ,"Elimination",challengeType))

## Compare risotto with pasta
  ## set up the data
  risotto <- dishesraw %>%
    filter(grepl("risotto",dish) & series =="US" ) %>%
    mutate(category = ifelse(grepl("risotto-style",dish) |
                                      grepl("risotto cake",dish) |
                                    grepl("risotto foam",dish) |
                                      grepl("cauliflower risotto",dish) |
                                      grepl("risotto paella",dish)
                                    ,"Risotto adjacent","Risotto")) %>%
    select(series,season,seasonNumber,episode,chef,challengeType,dish,outcome
           ,outcomeType,topbottom,category)

  noodles <- dishesraw %>%
    filter(pasta == 1  & series =="US" ) %>%
    select(series,season,seasonNumber,episode,chef,challengeType,dish,outcome
           ,outcomeType,topbottom) %>%
    mutate(category = "noodles")

  duotrio <- dishesraw %>%
    filter(duotrio == 1  & series =="US" ) %>%
    select(series,season,seasonNumber,episode,chef,challengeType,dish,outcome
           ,outcomeType,topbottom) %>%
    mutate(category = "duotrio")

  rawish <- dishesraw %>%
    filter(rawish == 1  & series =="US" ) %>%
    select(series,season,seasonNumber,episode,chef,challengeType,dish,outcome
           ,outcomeType,topbottom) %>%
    mutate(category = "Carpaccio & crudo etc.")

  ## Combine the data
  alldata <- risotto %>%
    bind_rows(noodles) %>%
    bind_rows(duotrio) %>%
    bind_rows(rawish)

## Summary data
  summary <- alldata %>%
    # overall
    group_by(category) %>%
    mutate(N_Dishes = n()) %>%
    ungroup() %>%
    # overall in outcome category
    group_by(category,topbottom) %>%
    mutate(N_Dishes_In_Outcome = n()) %>%
    # by challenge type
    ungroup() %>% group_by(category,challengeType,topbottom
                           ,N_Dishes,N_Dishes_In_Outcome) %>%
    summarise(N_Dishes_In_CT_and_Outcome=n()) %>%
    # get the % witin the challenge type
    ungroup() %>% group_by(category,challengeType) %>%
    mutate(N_challengeType=sum(N_Dishes_In_CT_and_Outcome) ) %>%
    # drop the "in the middle" and "top" things
    filter(topbottom %in% c("Bottom")) %>%
    mutate(
      # get the % in the top/bottom overall and in chall type
      # O is for overall
      O = round(N_Dishes_In_Outcome/N_Dishes,3)
      ,percent_CT = round(N_Dishes_In_CT_and_Outcome/N_challengeType,3)
      ,challengeType = ifelse(challengeType == "Elimination","E","Q")
    )

  scatterplotdata <-  summary %>%
    select(!c(N_Dishes_In_Outcome,N_Dishes_In_CT_and_Outcome
              ,N_challengeType,topbottom)) %>%
    pivot_wider(names_from=challengeType,values_from=percent_CT) %>%
    pivot_longer(!c(category,N_Dishes)
                 ,values_to = "percent",names_to = "type")

  NsData <- summary %>%
    select(category,N_Dishes,challengeType,N_challengeType) %>%
    mutate(challengeType = ifelse(challengeType == "E"
                                  ,"Elimination","Quickfire"))

## Visualize
  atbottom <- scatterplotdata %>%
    ggplot(aes(x=percent,y=category,color=type,label=type)) +
    geom_text() +
    scale_x_continuous("% of dishes at the bottom"
                       ,lim=c(0,1.05)
                       ,breaks=seq(0,1,.2)
                       ,labels=paste0(seq(0,100,20),"%")) +
    scale_color_manual(values=c("red","forestgreen","blue")) +
    theme(
      legend.position = "none"
    )
  # 5,301 dishes total. I have data on less than half. 2,321 is 43.8%
  Ns <- NsData %>%
    ggplot(aes(x=N_challengeType,y=category,label=N_challengeType
               ,color=challengeType,fill=challengeType)) +
    geom_bar(stat="identity") +
    geom_text(color="white")+
    scale_color_manual(values=c("red","blue")) +
    scale_fill_manual(values=c("red","blue")) +
    scale_x_continuous("# of dishes in this category") +
    theme(
      axis.text.y = element_blank()
      ,axis.title.y = element_blank()
    )

  ggarrange(atbottom,Ns,ncol=2,nrow=1,widths=c(2,1))

