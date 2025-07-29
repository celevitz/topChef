
rm(list=ls())
library(tidyverse)
library(gt)
library(ggplot2)

directory <- "/Users/carlylevitz/Documents/Data/topChef/"

confs <- read.csv(paste0(directory,"Top Chef - Confessionals by episode.csv")
                  ,header=TRUE)

placement <- read.csv(paste0(directory,"Top Chef - Chef details.csv")
                      ,header=TRUE)
placement$chef[placement$chef == "Kevin D'Andrea"] <- "Kévin D'Andrea"
placement$chef[placement$chef == "Cesar Murillo"] <- "César Murillo"

## Keep just first episode & winners
winners <- confs %>% ungroup() %>%
  left_join(placement %>%
              select(season,seasonNumber,chef,placement,name) %>%
              mutate(placement=as.numeric(placement))) %>%
  # For each episode, which chef was shown the most?
  group_by(series,season,seasonNumber,episode) %>%
  mutate(shownthemost = ifelse(
    max(percentdifffromequalinEp)==percentdifffromequalinEp
    ,"Shown the most","Not shown the most")) %>%
  # first episode that a chef was in
  ungroup() %>% group_by(series,season,seasonNumber,chef) %>%
  mutate(keep = ifelse(min(episode)==episode,"Keep","Drop")) %>%
  filter((placement == 1 ) &  keep == "Keep") %>% ungroup() %>%
  select(season,seasonNumber,name,episode,placement,count,equalInEpPercent
         ,percentdifffromequalinEp,percentofEpsConfs,shownthemost
         ,ElimWinner,atJTElim,OutWithdrewElim,first) %>%
  arrange(seasonNumber) %>%
  # category of winner
  mutate(category = case_when(ElimWinner == 1 ~ "Won elimination"
                              ,ElimWinner == 0 & atJTElim == 1 ~ "In the top of elimination"
                              ,TRUE ~ "Safe")
         ,xValue=ifelse(placement == 1, 0, NA)
         ,y = dense_rank(desc(seasonNumber))
         ,edit = case_when(percentdifffromequalinEp < 0 ~ "Undershown"
                        ,percentdifffromequalinEp == 0 ~ "Shown as expected"
                        ,percentdifffromequalinEp > 0 ~ "Overshown")
         ,first = ifelse(first == "first","*",""))

winnerssimplified <- winners %>%
  select(season,seasonNumber,name,equalInEpPercent,percentofEpsConfs
         ,category,xValue,y,edit,first) %>%
  pivot_longer(!c(season,seasonNumber,name,category,xValue,y,edit,first)
               ,names_to="variable",values_to="value") %>%
  # replace category for the percent equal data
  mutate(category = ifelse(variable %in% "equalInEpPercent"
                           ,"Expected %",category))

## Winners
winners %>%
  ggplot(aes(x=equalInEpPercent,y=y)) +
  scale_y_continuous(breaks=seq(1,length(unique(winners$seasonNumber)),1)
                    ,labels=gsub("NA","",paste0("S ",rev(unique(winners$seasonNumber))
                                   ," winner",winners$first))) +
  scale_x_continuous(lim=c(0,.12),breaks = seq(0,.12,.01)
                     ,labels = paste0(seq(0,.12,.01)*100,"%")
                     ,"% of episode 1's confessionals") +
  geom_segment(aes(x=equalInEpPercent,xend=percentofEpsConfs
                ,y=y,yend=y,lty=edit,color=category)) +
  geom_point(aes(x=percentofEpsConfs,y=y,color=category,fill=category
                 ,shape=category),cex=5) +
  geom_point(aes(x=equalInEpPercent,y=y),shape="|",color="black",cex=5) +
  labs(title="% of confessionals winners had in the 1st episode of their season"
       ,subtitle="Each chef's total is compared to the number if the episode's confessionals were evenly distributed.\n\n| = expected % of confessionals a chef would receive if everyone received the same # in the 1st episode.\n\n* = chef had the first confessional of the first episode."
       ,caption = "Created by Carly Levitz for Pack Your Knives") +
  scale_linetype_manual(values=c("solid","73", "dashed"))+
  scale_color_manual(values = c("#f8931f","#364156","#57b8ff")) +
  theme_minimal()+
  theme(
    panel.grid = element_blank()
    ,axis.ticks.y = element_blank()
    ,axis.title.y = element_blank()
    ,axis.text = element_text(color="black",size = 22)
    ,axis.title.x = element_text(color="black",size = 22,face="bold")
    ,axis.line = element_line(color="black")
    ,axis.ticks.x = element_line(color="black")
    ,plot.title = element_text(color="black",size = 32,face="bold")
    ,plot.title.position="plot"
    ,plot.subtitle = element_text(color="black",size=25)
    ,plot.caption = element_text(color="black",size=20,hjust=.5)
    ,panel.background = element_rect(color="white", fill="white")
    ,plot.background  = element_rect(color="white", fill="white")
    ,plot.margin = margin(t=25,r=10,l=10,b=10)
    ,legend.key.size = unit(1, 'cm')
    ,legend.key.height = unit(1, 'cm')
    ,legend.key.width = unit(1, 'cm')
    ,legend.title = element_text(size=20)
    ,legend.text = element_text(size=18)
  )

dev.print(png, file = paste0(directory,"WinnersConfsEp1.png")
          , width = 1200, height = 900)
dev.off()

## For social media

## Verifying that there are no winners who were at the bottom of 1st elim chall
winnersraw <- read.csv(paste0(directory,"Top Chef - Chef details.csv"))

winners<- winnersraw %>%
  filter(series=="US" & placement =="1.0" ) %>%
  select(series,season,seasonNumber,chef,placement)


challs <- read.csv(paste0(directory,"Top Chef - Challenge wins.csv")) %>%
  filter(series == "US" & challengeType %in% c("Elimination","Sudden Death Elimination","Quickfire Elimination"))


challwin <- challs %>%
  left_join(winners) %>%
  filter(placement == "1.0") %>%
  group_by(season,seasonNumber,chef) %>%
  mutate(firstep=min(episode)) %>%
  filter(episode == firstep) %>%
  select(season,seasonNumber,chef,episode,challengeType,outcome)


#
## Keep just first episode and whoever was shown the most
# shownmost <- confs %>% ungroup() %>%
#   left_join(placement %>%
#               select(season,seasonNumber,chef,placement,name) %>%
#               mutate(placement=as.numeric(placement))) %>%
#   # For each episode, which chef was shown the most?
#   group_by(series,season,seasonNumber,episode) %>%
#   mutate(shownthemost = ifelse(
#     max(percentdifffromequalinEp)==percentdifffromequalinEp
#     ,"Shown the most","Not shown the most")) %>%
#   # first episode that a chef was in
#   ungroup() %>% group_by(series,season,seasonNumber,chef) %>%
#   mutate(keep = ifelse(min(episode)==episode,"Keep","Drop")) %>%
#   filter(shownthemost == "Shown the most" &  keep == "Keep") %>% ungroup() %>%
#   select(season,seasonNumber,name,episode,placement,count,equalInEpPercent
#          ,percentdifffromequalinEp,shownthemost,ElimWinner,atJTElim
#          ,OutWithdrewElim) %>%
#   arrange(seasonNumber)
#
# ## Combine
# combined <- winners %>%
#   bind_rows(shownmost) %>%
#   mutate(y = dense_rank(desc(seasonNumber))
#          ,`Elimination challenge outcome` = case_when(ElimWinner == 1 ~ "Won"
#                             ,OutWithdrewElim == 1 ~ "Eliminated"
#                             ,atJTElim == 1 ~ "At judges' table"
#                             ,TRUE ~ "Safe")
#          ,`Category of chef` = ifelse(placement == 1,"Season winner"
#                                 ,"Most confessionals in 1st episode")
#          ,xValue=ifelse(placement == 1, 0, NA))
# numberofseasons <- length(unique(combined$seasonNumber))
#
# combined %>%
#   ggplot(aes(x= percentdifffromequalinEp,y=y
#              ,shape=`Elimination challenge outcome`
#              ,color=`Category of chef`,fill=`Category of chef`))+
#   geom_rect(aes(xmin=0,xmax=0,ymin=1,ymax=numberofseasons),color="gray47") +
#   geom_point() +
#   scale_y_continuous(breaks=seq(1,length(unique(combined$seasonNumber)),1)
#                      ,labels=paste0("S ",rev(unique(combined$seasonNumber)))) +
#   scale_x_continuous(lim=c(-.5,1.6),breaks = seq(-.5,1.5,.5)
#                      ,labels = seq(-.5,1.5,.5)) +
#   theme_minimal() +
#   geom_rect(aes(xmin=percentdifffromequalinEp,xmax=xValue,ymin=y,ymax=y))
#
#
#

