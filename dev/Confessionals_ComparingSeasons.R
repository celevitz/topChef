
rm(list=ls())
library(tidyverse)
library(gt)
library(ggplot2)

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
## Comparing 6th place and better: Table
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


############################################################################
## Comparing 6th place and better: Visual compared to expected
temparranged <- temp %>%
  ungroup() %>%
  mutate(group = case_when(placement == 1 ~ "1st place"
                           ,placement %in% c(2,3) ~ "2nd or 3rd place"
                           ,placement %in% c(4,5) ~ "4th or 5th place"
                           ,placement %in% c(6,7,8,9) ~ "6th to 9th place"
                           ,placement >= 10 ~ "10th place or lower")
         ,`season #` = case_when(`season #` == "Season 1" ~ "Season 01"
                                 ,`season #` == "Season 2" ~ "Season 02"
                                 ,TRUE ~ `season #`))

# temparranged <- temparranged[order(temparranged$`difference from expected`),]
#
# temparranged %>%
#   ggplot(aes(x=`difference from expected`,y=placement
#              ,color=placement,shape=`season #`)) +
#   geom_point() +
#   facet_wrap(~`season #`)

temparranged %>%
  ggplot(aes(x=`difference from expected`,y=placement)) +
  geom_rect(xmin=-.01,xmax=0.01,ymin=1,ymax=16.1,color="gray90") +
  geom_point(aes(color=`season #`,shape=`season #`) ,size = 7) +
  scale_color_manual(values = c("#D81B60","#1E88E5","#55B9A7","#FFC107"
                                ,"#004D40")) +
  ylab("placement (lower is better)") +
  xlab("difference from expected % of confessionals") +
  labs(title = "Top Chef Confessionals: Difference from Expected % of Confessionals"
          ,subtitle = "Data collected manually by Carly Levitz") +
  theme_minimal() +
  theme(axis.line = element_line(color="black")
        ,axis.ticks = element_line(color="black")
        ,axis.text = element_text(color="black",size=20)
        ,axis.title = element_text(color="black",size=20)
        ,title = element_text(color="black",size=22)
        ,subtitle = element_text(color="black",size=20)
        ,panel.grid = element_blank()
        ,legend.title = element_text(size=18)
        ,legend.text = element_text(size=16)
        )

dev.print(png, file = paste0(directory,"ConfessionalsByPlacement.png")
          , width = 900, height = 900)
dev.off()


temparranged %>%
  ggplot(aes(x=`difference from expected`,y=placement)) +
  geom_rect(xmin=-.01,xmax=0.01,ymin=1,ymax=16.1,color="gray90") +
  geom_point(aes(color=`season #`) ) +
  scale_color_manual(values = c("#D81B60","#1E88E5","#55B9A7","#FFC107"
                                ,"#004D40")) +
  ylab("placement (lower is better)") +
  facet_grid(cols = vars(`season #`))





