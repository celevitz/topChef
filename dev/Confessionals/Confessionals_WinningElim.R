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

#########################################################################
## Some exploratory stats
  ## average difference from equal edit for those @JT by gender-racial identity

  combined %>%
  group_by(series,identity,atJTElim) %>%
  summarise(mean=mean(percentdifffromequalinEp,na.rm=TRUE)
            ,median = median(percentdifffromequalinEp,na.rm=TRUE))

