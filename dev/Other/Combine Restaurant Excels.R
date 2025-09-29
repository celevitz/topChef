rm(list=ls())

library(openxlsx)
library(tidyverse)

directory <- "/Users/carlylevitz/Documents/Data/topChef/Restaurants/"

## Combine my and Kendra's chef info
Kchefs <- as_tibble(read.xlsx(paste(directory
                                         ,"Find Top Chef Data.xlsx",sep="")
                               ,sheet=1))
Cchefs <- as_tibble(read.xlsx(paste(directory
                      ,"Pack Your Knives Top Chef Restaurants List.xlsx",sep="")
                              ,sheet=2))

combinedchefs <- Cchefs %>%
  full_join(Kchefs %>%
              select(id,name,instagram))


combinedchefs %>%
  arrange(name,id)  %>%
  distinct() %>%
  view()

## Who doesn't yet have an ID?
  combinedchefs %>% filter(is.na(id))

