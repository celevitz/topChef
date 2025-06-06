## Analyze dish data

rm(list=ls())

library(stringr)
library(topChef)
library(openxlsx)
library(ggplot2)
library(tidyverse)

directory <- "/Users/carlylevitz/Documents/Data/"

dishesraw <- read.csv(paste0(directory
                             ,"/topChef/Top Chef - Dishes wide form with classifications.csv")
                      ,header = TRUE) %>%
  mutate(topbottom = case_when(outcome %in% c("WIN","WINNER","HIGH") ~ "Top"
                             ,outcome %in% c("LOW","OUT","RUNNER-UP") ~ "Bottom"
                               ,TRUE ~ "In the middle"))

risotto <- dishesraw %>%
  filter(grepl("risotto",dish) & series =="US")

  ## In what types of challenges did people make risotto?
  table(risotto$challengeType,risotto$outcomeType)

  barchartdata <- as.data.frame(risotto %>%
    group_by(series,challengeType,outcomeType) %>%
    mutate(n_group=n())  %>%
    ungroup() %>% group_by(series,challengeType) %>%
    mutate(n_CT = n()) %>%
    ungroup() %>% group_by(series,outcomeType) %>%
    mutate(n_OT=n()) %>%
    select(series,challengeType,outcomeType,n_group,n_CT,n_OT) %>%
    distinct() %>%
      mutate(challengeType = paste0(challengeType," (n=",n_CT,")")
             ,outcomeType = paste0(outcomeType," (n=",n_OT,")"))  )

  barchartpng <- barchartdata %>%
    ggplot(aes(x=challengeType,y=n_group
               ,color=outcomeType,fill=outcomeType,label=n_group)) +
    geom_bar(stat="identity",position = "dodge") +
    geom_text(color="white",position = position_dodge(width = .9),vjust=1.5) +
    ggtitle("Most risottos were made in elimination challenges"
            ,subtitle = "Data collection is complete for seasons 1 through 3, 16, and 20 through 22.\nThis also includes times when people made things in the style of risotto.") +
    labs(caption = "Created by Carly Levitz for Pack Your Knives") +
    scale_y_continuous("Number of dishes") +
    scale_x_discrete("Challenge type") +
    scale_color_manual(values=c("#c85200","#1170AA")) +
    scale_fill_manual(values=c("#c85200","#1170AA")) +
    guides(color=guide_legend(title="Outcome type")
           ,fill=guide_legend(title="Outcome type"))+
  theme(#panel.grid = element_blank()
    panel.background = element_rect(fill="white")
    ,plot.background = element_rect(fill="white")
    ,plot.title.position="plot",plot.caption = element_text(hjust=0,size=5)
    ,plot.caption.position="plot"
    ,plot.title = element_text(size=12),plot.subtitle = element_text(size=10)
    ,legend.text = element_text(size=7),legend.title = element_text(size=8)
    ,axis.line = element_line(color="black")
    ,axis.ticks = element_line(color="black")
    ,axis.text = element_text(color="black"))
  ggsave(paste0(directory,"/topChef/Risotto in what type of challenges.png")
         ,barchartpng,width = 6,height = 4,dpi = 1200 )


  ## How did people do in the challenges in which they made risotto?
  outcomes <- risotto %>%
    group_by(series,challengeType,outcomeType) %>%
    mutate(N=n()) %>% ungroup() %>%
    group_by(series,challengeType,outcomeType,N,topbottom) %>%
    summarise(n=n() ) %>%
    mutate(percent = round(n/as.numeric(N),3)
           ,challenge = paste0(outcomeType," ",challengeType)) %>%
    ungroup()


    outcomes %>%
      ggplot(aes(x=percent,y=challenge,color=topbottom,shape=topbottom
                 ,size=n)) +
      geom_point() +
      scale_x_continuous("% of dishes in those challenges"
                         ,lim=c(0,1.05)
                         ,breaks=seq(0,1,.2)
                         ,labels=paste0(seq(0,100,20),"%")) +
      scale_color_manual(values=c("#c85200","gray50","#1170AA")) +
      scale_shape_manual(values=c(20,21,22)) +
      scale_fill_manual(values=NA) +
      facet_wrap(~topbottom)






# # Using old function Function
#
# seriesname <- "US"
# wordorwordsofinterest <- "risotto"
#
#   basicdata <- dishesraw[dishesraw$series == seriesname,]
#   basicdata$word <- 0
#
#   for (w in wordorwordsofinterest) {
#     basicdata$word[grepl(w,basicdata$dish) | grepl(w,basicdata$notes)] <- 1
#   }
#
#   table(basicdata$challengeType[basicdata$word == 1]
#         ,basicdata$outcome[basicdata$word == 1])
#
#   ## Multiple functions:
#     ## descriptions with and without the word/phrase
#       highlevel <- basicdata
#       highlevel$temp <- 1
#       highlevel <- aggregate(highlevel$temp
#                              ,by=list(highlevel$seasonNumber,highlevel$word)
#                              ,FUN=length)
#       names(highlevel) <- c("seasonNumber","usesWord","count")
#       highlevel <- reshape(highlevel
#                            ,direction="wide"
#                            ,timevar="usesWord"
#                            ,idvar="seasonNumber")
#       highlevel$count.1[is.na(highlevel$count.1)] <- 0
#       highlevel$percent <- paste0(round((highlevel$count.1/(highlevel$count.0+
#                                                 highlevel$count.1))*100,1),"%")
#       names(highlevel) <- c("seasonNumber","disheswithoutwordorwords"
#                             ,"disheswithwordorwords"
#                             ,"percentofdisheswithwordorwords")
#       print("Number of dishes")
#       print(sum(highlevel$disheswithwordorwords))
#
#       print(highlevel)
#
#     ## Now just look at when the word/s were used:
#       basicdata <- basicdata[basicdata$word == 1,]
#
#       ## Number of episodes in which that word was used
#       episodes <- unique(basicdata[,c("series","season","seasonNumber"
#                                       ,"episode")])
#       print("Number of episodes in which word/s of interest was used by season")
#       print(table(episodes$seasonNumber))
#
#       ## number of chefs who used that word in a season
#       chefs <- unique(basicdata[,c("series","season","seasonNumber","chef")])
#       print("Number of chefs whose dishes had word/s of interest by season")
#       print(table(chefs$seasonNumber))
#
#       ## outcomes of dishes with the word/s of interest
#       print("Outcomes of dishes with the word/s of interest")
#       print(table(basicdata$outcome))
#
#       ## % of times that it won or was eliminated
#       print("What's the % of times this dish won?")
#       print(paste0(round(nrow(basicdata[basicdata$outcome %in% c("WIN","WINNER"),])/
#               nrow(basicdata[,])*100,1),"%"))
#       print("What's the % of times this dish won or was at the top?")
#       print(paste0(round(nrow(basicdata[basicdata$outcome %in% c("WIN","WINNER","HIGH"),])/
#                            nrow(basicdata[,])*100,1),"%"))
#
#       print("What's the % of times this dish was eliminated?")
#       print(paste0(round(nrow(basicdata[basicdata$outcome %in% c("OUT","RUNNER-UP"),])/
#                            nrow(basicdata[,])*100,1),"%"))
#       print("What's the % of times this dish was eliminated or at the bottom?")
#       print(paste0(round(nrow(basicdata[basicdata$outcome %in% c("OUT","RUNNER-UP","LOW"),])/
#                            nrow(basicdata[,])*100,1),"%"))
#
#       ## Who won?
#       print("Who won?")
#       print(basicdata[basicdata$outcome %in% c("WIN","WINNER")
#                       ,c("season","seasonNumber","episode","chef"
#                          ,"challengeType")])
#
#
#
