## Analyze dish data

rm(list=ls())
library(ggpubr)
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

## Compare risotto with pasta and other dishes
  ## set up the data
  risotto <- dishesraw %>%
    filter(grepl("risotto",dish) & series =="US" ) %>%
    mutate(category = "Risotto" #ifelse(grepl("risotto-style",dish) |
                                    #   grepl("risotto cake",dish) |
                                    # grepl("risotto foam",dish) |
                                    #   grepl("cauliflower risotto",dish) |
                                    #   grepl("risotto paella",dish)
                                    # ,"Risotto adjacent","Risotto")
           ) %>%
    select(series,season,seasonNumber,episode,chef,challengeType,dish,outcome
           ,outcomeType,topbottom,category)

    risottosummary <- risotto %>%
      group_by(series,challengeType,outcomeType,topbottom) %>%
      summarise(n=n()) %>%
      ungroup() %>% group_by(series,challengeType,outcomeType) %>%
      mutate(N=sum(n)
             ,percent=n/N
             ,challenge = paste0(outcomeType," ",challengeType)) %>%
      ungroup() %>% select(!c(challengeType,outcomeType)) %>%
      filter(topbottom == "Bottom")

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

## Visualize all dishes
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

  ggarrange(atbottom,Ns,ncol=2,nrow=1,widths=c(2,1.5))

## Visualize just risotto
  risottosummary$challenge <- factor(risottosummary$challenge
                             ,levels=c("Team Quickfire","Individual Quickfire"
                                       ,"Team Elimination"
                                       ,"Individual Elimination")
                             ,labels = c("Team Quickfire","Individual Quickfire"
                                         ,"Team Elimination"
                                         ,"Individual Elimination"))

  risottoPercent <- risottosummary  %>%
    ggplot(aes(x=percent,y=challenge
               ,label=paste0(round(percent*100,1),"%"))) +
    scale_x_continuous("% of dishes at the bottom"
                       ,lim=c(0,1.02)
                       ,breaks=seq(0,1,.2)
                       ,labels=paste0(seq(0,100,20),"%")) +
    scale_y_discrete("Challenge type") +
    geom_point(size=26) +
    geom_text(color="white",size=7) +
    theme(
      legend.position = "none"
      ,plot.background = element_blank()
      ,panel.background =  element_blank()
      ,panel.grid = element_blank()
      ,axis.ticks.y = element_blank()
      ,axis.ticks.x = element_line(color="black")
      ,axis.line.x = element_line(color="black")
      ,axis.text=element_text(size=30,color="black")
      ,axis.title=element_text(size=35,color="black")
    )

  risottoNs <- risottosummary %>%
    ggplot(aes(x=N,y=challenge,label=N)) +
    geom_bar(stat="identity") +
    geom_text(color="white",hjust=1.5,size=10) +
    scale_x_continuous("# of dishes in this category"
                       ,lim=c(0,20)) +
    theme(
      axis.text.y = element_blank()
      ,axis.title.y = element_blank()
      ,plot.background = element_blank()
      ,panel.background =  element_blank()
      ,panel.grid = element_blank()
      ,axis.ticks.y = element_blank()
      ,axis.ticks.x = element_line(color="black")
      ,axis.line.x = element_line(color="black")
      ,axis.text.x=element_text(size=30,color="black")
      ,axis.title.x=element_text(size=35,color="black")
    )

  risottofigure <- ggarrange(risottoPercent,risottoNs,ncol=2,nrow=1
                             ,widths=c(2,1))
  risottofigureAnnotated <- annotate_figure(risottofigure
              ,top = text_grob("Risotto dishes on Top Chef",color = "black"
                               , face = "bold", size = 40)
              ,bottom=text_grob("Created by Carly Levitz for Pack Your Knives"
                                ,color = "gray45",hjust = 1, x = 1
                                , face = "italic", size = 20)
              #,left = text_grob("Challenge type", color = "black", rot = 90)
              # ,right = "I'm done, thanks :-)!"
              # ,fig.lab = "Figure 1", fig.lab.face = "bold"
              )

  ggexport(risottofigureAnnotated,width = 1600,height = 900
           ,filename = paste(directory
                             ,"topChef/RisottoDishesOnTopChef.png",sep=""))

## Who made risotto more than once?!
  risotto %>% group_by(seasonNumber,chef) %>% summarise(n=n()) %>% filter(n>1)

  multiples <- risotto %>% filter(chef %in% c("Lee Anne W.","Howie K.","Tre W."
                                 ,"Manny Barella","Bailey Sullivan")) %>%
    select(seasonNumber,episode,chef,outcome) %>%
    arrange(seasonNumber,episode)


## % of all dishes at the bottom
  table(dishesraw$topbottom[dishesraw$series == "US"])
  table(risotto$topbottom)
## % of all dishes that were eliminated
  table(dishesraw$outcome[dishesraw$series == "US" & dishesraw$challengeType == "Elimination"])



  chisqdata <- dishesraw %>%
    filter(series == "US") %>%
    select(series,season,topbottom,outcome,challengeType) %>%
    mutate(category = "all" ) %>%
    bind_rows(risotto %>%
            select(series,season,topbottom,outcome,category,challengeType)) %>%
    mutate(eliminated = ifelse(outcome %in% c("OUT","RUNNER-UP"),"Eliminated"
                               ,"In or win"))



  chisq.test(table(chisqdata$category,chisqdata$topbottom))
  chisq.test(table(chisqdata$category[chisqdata$challengeType == "Elimination"]
                   ,chisqdata$eliminated[chisqdata$challengeType == "Elimination"]))


