
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
  select(season,seasonNumber,name,episode,placement,count
         ,percentdifffromequalinEp,shownthemost,ElimWinner,atJTElim
         ,OutWithdrewElim) %>%
  arrange(seasonNumber)

## Keep just first episode and whoever was shown the most
shownmost <- confs %>% ungroup() %>%
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
  filter(shownthemost == "Shown the most" &  keep == "Keep") %>% ungroup() %>%
  select(season,seasonNumber,name,episode,placement,count
         ,percentdifffromequalinEp,shownthemost,ElimWinner,atJTElim
         ,OutWithdrewElim) %>%
  arrange(seasonNumber)

## Combine
combined <- winners %>%
  bind_rows(shownmost) %>%
  mutate(y = dense_rank(desc(seasonNumber))
         ,`Elimination challenge outcome` = case_when(ElimWinner == 1 ~ "Won"
                            ,OutWithdrewElim == 1 ~ "Eliminated"
                            ,atJTElim == 1 ~ "At judges' table"
                            ,TRUE ~ "Safe")
         ,`Category of chef` = ifelse(placement == 1,"Season winner"
                                ,"Most confessionals in 1st episode")
         ,xValue=ifelse(placement == 1, 0, NA))
numberofseasons <- length(unique(combined$seasonNumber))

combined %>%
  ggplot(aes(x= percentdifffromequalinEp,y=y
             ,shape=`Elimination challenge outcome`
             ,color=`Category of chef`,fill=`Category of chef`))+
  geom_rect(aes(xmin=0,xmax=0,ymin=1,ymax=numberofseasons),color="gray47") +
  geom_point() +
  scale_y_continuous(breaks=seq(1,length(unique(combined$seasonNumber)),1)
                     ,labels=paste0("S ",rev(unique(combined$seasonNumber)))) +
  scale_x_continuous(lim=c(-.5,1.6),breaks = seq(-.5,1.5,.5)
                     ,labels = seq(-.5,1.5,.5)) +
  theme_minimal() +
  geom_rect(aes(xmin=percentdifffromequalinEp,xmax=xValue,ymin=y,ymax=y))




