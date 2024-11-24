## Bring in data from Excel file

rm(list=ls())

library(openxlsx)
library(usethis)
library(rmarkdown)
library(goodpractice)
library(stringi)
library(tidyr)
library(tidyverse)

directory <- "/Users/carlylevitz/Documents/Data/"

placement <- as_tibble(read.xlsx(paste(directory,"TopChefData.xlsx",sep=""),sheet=1)) %>%
  select(chef,season,seasonNumber,series,placement,personOfColor,gender) %>%
  filter(series == "US") %>%
  mutate(personOfColor = ifelse(is.na(personOfColor),"white",personOfColor))

placement %>%
  group_by(seasonNumber,season,personOfColor) %>%
  summarise(n=n()) %>%
  pivot_wider(values_from = n,names_from = personOfColor) %>%
  mutate(percent = POC/(POC+white)*100) %>%
  print(n=21)

confs <- as_tibble(read.xlsx(paste(directory,"TopChefData.xlsx",sep=""),sheet=9)) %>%
  left_join(placement) %>%
  filter(inCompetition == TRUE) %>%
  mutate(aftersecondallstars = ifelse(seasonNumber>=18,"after","17orbefore"))



reg <- lm(confs$count ~ confs$episode+confs$placement+confs$gender+confs$personOfColor+confs$aftersecondallstars)
summary(reg)





