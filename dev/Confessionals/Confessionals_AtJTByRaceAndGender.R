## Do those who win elimination have the most confessionals?

rm(list=ls())
library(tidyverse)
library(gt)
library(ggplot2)

directory <- "/Users/carlylevitz/Documents/Data/"

## bring in the data
## Combine the confessional data with race and gender
## (the data is already combined w/ elim challenge outcome)
confsByEpi <- read.csv(paste0(directory
                              ,"/topChef/Top Chef - Confessionals by episode.csv")
                       ,header=TRUE)

details <- read.csv(paste0(directory,"topChef/Top Chef - Chef details.csv")
                      ,header=TRUE) %>%
  select(series,season,seasonNumber,chef,placement,gender,personOfColor)

details$chef[details$chef == "Kevin D'Andrea"] <- "Kévin D'Andrea"
details$chef[details$chef == "Cesar Murillo"] <- "César Murillo"
details$chef[details$chef == "Begona Rodrigo" ] <- "Begoña Rodrigo"

combined <- confsByEpi %>%
  left_join(details) %>%
  select(!c(quotes,phone.call,photos)) %>%
  mutate(personOfColor = ifelse(is.na(personOfColor),"white","POC")
         ,identity = paste(personOfColor,gender,sep=" "))

## how many episodes do I have data for in each season?
confsByEpi %>%
  filter(count > 0 & !(is.na(count))) %>%
  select(series,season,seasonNumber,episode) %>%
  distinct() %>%
  group_by(series,season,seasonNumber) %>%
  summarise(numberofepisodeswithdata = n()) %>%
  arrange(seasonNumber)

#########################################################################
## Some exploratory stats
  ## average difference from equal edit for those @JT by gender-racial identity

  combined %>%
  group_by(series,identity,atJTElim) %>%
  summarise(mean=mean(percentdifffromequalinEp,na.rm=TRUE)
            ,median = median(percentdifffromequalinEp,na.rm=TRUE)) %>%
  arrange(atJTElim,median)

  ## what predicts average edit if they're at JT?
  ## use logistic regression because it's between -1 and 1
  atJT <- combined %>% filter(atJTElim == 1)
  reg <- glm(atJT$percentdifffromequalinEp ~ atJT$gender + atJT$personOfColor +
        atJT$gender*atJT$personOfColor        )
  summary(reg)

  reg <- glm(atJT$percentdifffromequalinEp ~ atJT$identity)
  summary(reg)


