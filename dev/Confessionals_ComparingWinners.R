
rm(list=ls())
library(tidyverse)
library(gt)

directory <- "/Users/carlylevitz/Documents/Data/topChef/"

confs <- read.csv(paste0(directory,"Top Chef - Confessionals in a season.csv")
                  ,header=TRUE)

placement <- read.csv(paste0(directory,"Top Chef - Chef details.csv")
                      ,header=TRUE)

temp <- confs %>%
  select(!c(series,totalconfs,totalchefsepisodes,phonecallsorphotos) )%>%
  filter(seasonNumber %in% c(1,2,21,22)) %>%
  left_join(placement %>%
              select(season,seasonNumber,chef,placement)) %>%
  mutate(placement=as.numeric(placement)
         ,expectedpercentofconfs = paste0(round(expectedpercentofconfs,3)*100
                                          ,"%")
         ,observedpercent = paste0(round(observedpercent,3)*100,"%")
         ,difffromexpected = paste0(round(difffromexpected,3)*100,"%")
                                    ) %>%
  rename(`season #` = seasonNumber
         ,`# of first confessionals`=firstconfs,`total confessionals`=chefconfs
         ,`expected %`=expectedpercentofconfs,`observed %`=observedpercent
         ,`difference from expected`=difffromexpected) %>%
  arrange(placement,`season #`)

temp %>%
  gt() %>%
  #cols_hide(columns=c(placement)) %>%
  tab_row_group(label = "13th or lower",rows = placement >=13) %>%
  tab_row_group(label = "10th through 12th",rows = placement %in%
                  c(10,11,12)) %>%
  tab_row_group(label = "7th through 9th",rows = placement %in% c(7,8,9)) %>%
  tab_row_group(label = "4th through 6th place",rows =placement %in%
                  c(4,5,6)) %>%
  tab_row_group(label = "2nd or 3rd place",rows = placement %in% c(2,3)) %>%
  tab_row_group(label = "Winners",rows = placement == 1) %>%
  tab_options(data_row.padding = px(1),
              column_labels.padding = px(1),
              row_group.padding = px(1))  %>%
  tab_style(style = cell_text(align = "right"),locations = cells_source_notes()) %>%
  tab_style(style = cell_text(align = "left",weight="bold")
            ,locations = cells_title(groups="title")) %>%
  tab_style(style = cell_text(align = "left",weight="bold")
            ,locations = cells_row_groups() ) %>%
  tab_style(style = cell_text(align = "left")
            ,locations = cells_title(groups="subtitle")) %>%
  tab_style(style = cell_text(align = "center")
            ,locations = cells_body(columns=!c(season,chef))) %>%
  tab_style(style = cell_text(align = "center",weight="bold")
            ,locations = cells_column_labels(columns=!c(season,chef)))
  tab_style(style = cell_text(align = "left",weight="bold")
            ,locations = cells_column_labels(columns=!c(season,chef)))
  tab_style(style = cell_text(align = "center",weight="bold")
            ,locations = cells_column_spanners())
