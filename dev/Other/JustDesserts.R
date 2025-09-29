rm(list=ls())

library(openxlsx)
library(tidyverse)
library(gt)

directory <- "/Users/carlylevitz/Documents/Data/"

chefdetails <- as_tibble(read.xlsx(paste(directory
                                     ,"TopChefData.xlsx",sep=""),sheet=1)) %>%
  filter(series == "Just Desserts")

challdescr <- as_tibble(read.xlsx(paste(directory
                                       ,"TopChefData.xlsx",sep=""),sheet=3))

challwins <- as_tibble(read.xlsx(paste(directory
                                     ,"TopChefData.xlsx",sep=""),sheet=2)) %>%
  filter(series == "Just Desserts" & inCompetition == "TRUE") %>%
  select(!c(series,dish,notes,rating)) %>%
  mutate(outcome = case_when(outcome %in% c("WIN","WINNER") ~ "WIN"
                           ,outcome %in% c("OUT","WITHDREW","RUNNER-UP") ~ "OUT"
                           ,outcome %in% c("HIGH","LOW") ~ outcome
                           ,TRUE ~ "IN")) %>%
  left_join(challdescr %>%
            select(season,seasonNumber,episode,challengeType,outcomeType)) %>%
  left_join(chefdetails %>% select(chef,placement))


## Challenge stats by person
  challstats <- challwins %>%
    group_by(chef,placement,challengeType,outcome) %>%
    summarise(n=n()) %>%
    pivot_wider(names_from=challengeType,values_from=n) %>%
    pivot_wider(names_from=outcome
                ,values_from=c("Elimination","Quickfire")) %>%
    select(!Quickfire_OUT)


  challstats %>%
    gt() %>%
    tab_source_note(source_note = "Created by Carly Levitz for Pack Your Knives") %>%
    tab_options(data_row.padding = px(1),
                column_labels.padding = px(1),
                row_group.padding = px(1)) %>%
    tab_spanner(label = "Elimination",
      columns = c(Elimination_WIN,Elimination_HIGH,Elimination_IN
                  ,Elimination_LOW,Elimination_OUT ) ) %>%
    tab_spanner(label = "Quickfire",
                columns = c(Quickfire_WIN,Quickfire_HIGH,Quickfire_IN
                            ,Quickfire_LOW ) )





