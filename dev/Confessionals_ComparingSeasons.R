
rm(list=ls())
library(tidyverse)
library(gt)

directory <- "/Users/carlylevitz/Documents/Data/topChef/"

confs <- read.csv(paste0(directory,"Top Chef - Confessionals in a season.csv")
                  ,header=TRUE)

placement <- read.csv(paste0(directory,"Top Chef - Chef details.csv")
                      ,header=TRUE)
placement$chef[placement$chef == "Kevin D'Andrea"] <- "Kévin D'Andrea"
placement$chef[placement$chef == "Cesar Murillo"] <- "César Murillo"

temp <- confs %>%
  select(!c(series,totalconfs,totalchefsepisodes,phonecallsorphotos) )%>%
  filter(seasonNumber %in% c(1,2,16,21,22)) %>%
  left_join(placement %>%
              select(season,seasonNumber,chef,placement,name)) %>%
  mutate(placement=as.numeric(placement)
         ,expectedpercentofconfs = paste0(round(expectedpercentofconfs,3)*100
                                          ,"%")
         ,observedpercent = paste0(round(observedpercent,3)*100,"%")
         ,difffromexpected = round(difffromexpected,3)*100
         ,`season #` = paste0("Season ",seasonNumber)
         ,chef=name
                                    ) %>%
  filter(!(is.na(placement))) %>%
  rename(`# of first confessionals`=firstconfs,`total confessionals`=chefconfs
         ,`expected %`=expectedpercentofconfs,`observed %`=observedpercent
         ,`difference from expected`=difffromexpected) %>%
  arrange(placement,desc(`difference from expected`),seasonNumber) %>%
  ## drop the unneeded variables, rearrange
  select(!c(season,seasonNumber,name))
## add average for each grouping of placements


############################################################################
## Comparing 6th place and better.
confessionalstattable <- temp %>%
  ## keep just people in 6th place or better
  filter(placement <= 6) %>%
  gt() %>%
  cols_hide(columns=c(placement)) %>%
  tab_row_group(label = "6th place",rows = placement %in% c(6)) %>%
  tab_row_group(label = "5th place",rows = placement %in% c(5)) %>%
  tab_row_group(label = "4th place",rows = placement %in% c(4)) %>%
  tab_row_group(label = "3rd place",rows = placement %in% c(3)) %>%
  tab_row_group(label = "2nd place",rows = placement %in% c(2)) %>%
  tab_row_group(label = "Winners",rows = placement == 1) %>%
  tab_options(data_row.padding = px(1),
              column_labels.padding = px(1),
              row_group.padding = px(1))  %>%
  cols_width(chef ~ px(175)
             ,`# of first confessionals` ~ px(120)
             ,`total confessionals` ~ px(120)
             ,`difference from expected` ~ px(150)
             ,`season #` ~ px(100)
             , everything() ~ px(110) ) %>%
  tab_style(style = cell_text(align = "right")
            ,locations = cells_source_notes()) %>%
  tab_style(style = cell_text(align = "left",weight="bold")
            ,locations = cells_title(groups="title")) %>%
  tab_style(style = cell_text(align = "left",weight="bold")
            ,locations = cells_row_groups() ) %>%
  tab_style(style = cell_text(align = "left")
            ,locations = cells_title(groups="subtitle")) %>%
  tab_style(style = cell_text(align = "center")
            ,locations = cells_body(columns=!c(chef,`season #`))) %>%
  tab_style(style = cell_text(align = "center",weight="bold")
            ,locations = cells_column_labels(columns=!c(chef,`season #`))) %>%
  tab_style(style = cell_text(align = "center",weight="bold")
            ,locations = cells_column_labels(columns=!c(chef,`season #`))) %>%
  tab_style(style = cell_text(align = "center",weight="bold")
            ,locations = cells_column_spanners()) %>%
  data_color(method="numeric",
             columns=`difference from expected`,
             palette=c("#c85200","#ffbc69","gray80", "#a3cce9","#1170AA"),
             domain=c(min(temp$`difference from expected`)
                      ,max(temp$`difference from expected`))) %>%
  summary_rows(
    groups = TRUE, columns = vars(`expected %`,`difference from expected`),
    fns = list(Average = ~mean(.)),
    formatter = fmt_number, decimals = 1
  ) %>%
  tab_header(
    title = paste0("Top Chef Confessional Information for Chefs Placing 6th or Better")
    ,subtitle = "Expected % of confessionals takes into account how many episodes they were in and how many chefs were in that episode. Observed % of confessionals is the percent of all confessionals in the season so far. Data collected manually by Carly Levitz."
  )

gtsave(confessionalstattable
       ,filename = paste(directory,"Confessionals_6thplaceorbetter.png",sep=""))
