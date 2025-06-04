rm(list=ls())
library(tidyverse)
library(openxlsx)
library(ggplot2)
library(gt)

directory <- "/Users/carlylevitz/Documents/Data/topChef/"
chefdetails <- read.csv(paste0(directory,"Top Chef - Chef details.csv"))  %>%
  filter(series == "US" )

## First, gender ratio at start
starting <- chefdetails %>%
  select(season,seasonNumber,chef,gender) %>%
  filter(!(is.na(gender))) %>%
  group_by(season,seasonNumber,gender) %>%
  summarise(n=n())%>%
  # how many are in the comp at that time?
  ungroup() %>%
  group_by(season,seasonNumber) %>%
  mutate(total = sum(n)
         ,timing="A. All chefs, including those who didn't make it out of Qualifiers or LCK")

## Gender ratio of only those who were in the official competition
officialcomp <- chefdetails %>%
  mutate(placement=as.numeric(placement)) %>%
  filter(!(is.na(gender)) &
           # While Season 22 is ongoing, I need to include them since their
           # placement is NA for those still in the comp
           (!(is.na(placement)) | seasonNumber == 22)
         ) %>%
  select(season,seasonNumber,chef,gender) %>%
  group_by(season,seasonNumber,gender) %>%
  summarise(n=n()) %>%
  # how many are in the comp at that time?
  ungroup() %>%
  group_by(season,seasonNumber) %>%
  mutate(total = sum(n)
         ,timing="B. Chefs with an official placement in their season")

## Gender ratio of top 8 chefs
eight <- chefdetails %>%
  mutate(placement=as.numeric(placement)) %>%
  filter(!(is.na(gender)) &
           # While Season 22 is ongoing, I need to include them since their
           # placement is NA for those still in the comp
           (placement <=8 |  (seasonNumber == 22 & is.na(placement)) )
        ) %>%
  select(season,seasonNumber,chef,gender) %>%
  group_by(season,seasonNumber,gender) %>%
  summarise(n=n()) %>%
  mutate(total=8
         ,timing = "C. Final 8 chefs")

## Gender ratio of top 4 chefs
four <- chefdetails %>%
  mutate(placement=as.numeric(placement)) %>%
  filter(!(is.na(gender)) &
           # While Season 22 is ongoing, I need to include them since their
           # placement is NA for those still in the comp
           (placement <=4 | (seasonNumber == 22 & is.na(placement)) )
  ) %>%
  select(season,seasonNumber,chef,gender) %>%
  group_by(season,seasonNumber,gender) %>%
  summarise(n=n()) %>%
  mutate(total=4
         ,timing = "D. Final 4 chefs")

## Bring the data together
alldata <- starting %>%
  bind_rows(officialcomp) %>%
  bind_rows(eight) %>%
  bind_rows(four) %>%
  # Get percent of group that are women
  mutate(percent = round(n/total,3)) %>%
  filter(gender == "Female") %>%
  # get ratio of group that are women
  mutate(ratio = round(n/(total-n),3))%>%
  select(!gender) %>%
  # which ones have more women and which have less?
  mutate(Gender_balance = case_when(ratio < 1 ~ "A. Fewer women than men"
                             ,ratio == 1 ~ "B. Same number of women and men"
                             ,ratio > 1 ~ "C. More women than men"
                             ,TRUE ~ "needs to be categorized"))

##################################
## Visualize it

alldata %>%
  ggplot(aes(x=seasonNumber,y=ratio,color=Gender_balance,shape=Gender_balance))+
  geom_point() +
  scale_y_continuous(limits=c(0,3)
                     ,"Ratio of women to men") +
  scale_x_continuous("Season #") +
  facet_wrap(~timing)

##################################
# Table format
genderratiotable <- alldata %>%
  ungroup() %>%
  select(season,seasonNumber,ratio,timing) %>%
  pivot_wider(names_from=timing,values_from=ratio) %>%
  arrange(seasonNumber) %>%
  rename(`season #`=seasonNumber) %>%
  gt() %>%
  tab_source_note(source_note = "Created by Carly Levitz for Pack Your Knives") %>%
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
            ,locations = cells_body(columns=!season)) %>%
  tab_style(style = cell_text(align = "center",weight="bold")
            ,locations = cells_column_labels(columns=!season)) %>%
  tab_style(style = cell_text(align = "left",weight="bold")
            ,locations = cells_column_labels(columns=season)) %>%
  # Color the cells based on their value
  # Category A
    tab_style(locations=cells_body(columns=`A. All chefs, including those who didn't make it out of Qualifiers or LCK`
                                   ,rows = `A. All chefs, including those who didn't make it out of Qualifiers or LCK` <.7)
              ,style = cell_fill("#c85200")) %>%
    tab_style(locations=cells_body(columns=`A. All chefs, including those who didn't make it out of Qualifiers or LCK`
                                   ,rows = `A. All chefs, including those who didn't make it out of Qualifiers or LCK` <.7)
              ,style = cell_text("white")) %>%
    tab_style(locations=cells_body(columns=`A. All chefs, including those who didn't make it out of Qualifiers or LCK`
                                   ,rows = `A. All chefs, including those who didn't make it out of Qualifiers or LCK` >=.7 &
                                     `A. All chefs, including those who didn't make it out of Qualifiers or LCK` < 1)
              ,style = cell_fill("#ffbc69")) %>%
    tab_style(locations=cells_body(columns=`A. All chefs, including those who didn't make it out of Qualifiers or LCK`
                                 ,rows = `A. All chefs, including those who didn't make it out of Qualifiers or LCK` == 1)
              ,style = cell_fill("gray90")) %>%
    tab_style(locations=cells_body(columns=`A. All chefs, including those who didn't make it out of Qualifiers or LCK`
                                   ,rows = `A. All chefs, including those who didn't make it out of Qualifiers or LCK` > 1 &
                                     `A. All chefs, including those who didn't make it out of Qualifiers or LCK` < 2)
              ,style = cell_fill("#a3cce9")) %>%
    tab_style(locations=cells_body(columns=`A. All chefs, including those who didn't make it out of Qualifiers or LCK`
                                   ,rows = `A. All chefs, including those who didn't make it out of Qualifiers or LCK` >=2)
              ,style = cell_fill("#1170AA")) %>%
  # Category B
  tab_style(locations=cells_body(columns=`B. Chefs with an official placement in their season`
                                 ,rows = `B. Chefs with an official placement in their season` <.7)
            ,style = cell_fill("#c85200")) %>%
  tab_style(locations=cells_body(columns=`B. Chefs with an official placement in their season`
                                 ,rows = `B. Chefs with an official placement in their season` <.7)
            ,style = cell_text("white")) %>%
  tab_style(locations=cells_body(columns=`B. Chefs with an official placement in their season`
                                 ,rows = `B. Chefs with an official placement in their season` >=.7 &
                                   `B. Chefs with an official placement in their season` < 1)
            ,style = cell_fill("#ffbc69")) %>%
  tab_style(locations=cells_body(columns=`B. Chefs with an official placement in their season`
                                 ,rows = `B. Chefs with an official placement in their season` == 1)
            ,style = cell_fill("gray90")) %>%
  tab_style(locations=cells_body(columns=`B. Chefs with an official placement in their season`
                                 ,rows = `B. Chefs with an official placement in their season` > 1 &
                                   `B. Chefs with an official placement in their season` < 2)
            ,style = cell_fill("#a3cce9")) %>%
  tab_style(locations=cells_body(columns=`B. Chefs with an official placement in their season`
                                 ,rows = `B. Chefs with an official placement in their season` >=2)
            ,style = cell_fill("#1170AA")) %>%
  # Category C
  tab_style(locations=cells_body(columns=`C. Final 8 chefs`
                                 ,rows = `C. Final 8 chefs` <.7)
            ,style = cell_fill("#c85200")) %>%
  tab_style(locations=cells_body(columns=`C. Final 8 chefs`
                                 ,rows = `C. Final 8 chefs` <.7)
            ,style = cell_text("white")) %>%
  tab_style(locations=cells_body(columns=`C. Final 8 chefs`
                                 ,rows = `C. Final 8 chefs` >=.7 &
                                          `C. Final 8 chefs` < 1)
            ,style = cell_fill("#ffbc69")) %>%
  tab_style(locations=cells_body(columns=`C. Final 8 chefs`
                                 ,rows = `C. Final 8 chefs` == 1)
            ,style = cell_fill("gray90")) %>%
  tab_style(locations=cells_body(columns=`C. Final 8 chefs`
                                 ,rows = `C. Final 8 chefs` > 1 &
                                         `C. Final 8 chefs` < 2)
            ,style = cell_fill("#a3cce9")) %>%
  tab_style(locations=cells_body(columns=`C. Final 8 chefs`
                                 ,rows = `C. Final 8 chefs` >=2)
            ,style = cell_fill("#1170AA")) %>%
  # Category D
  tab_style(locations=cells_body(columns=`D. Final 4 chefs`
                                 ,rows = `D. Final 4 chefs` <.7)
            ,style = cell_fill("#c85200")) %>%
  tab_style(locations=cells_body(columns=`D. Final 4 chefs`
                                 ,rows = `D. Final 4 chefs` <.7)
            ,style = cell_text("white")) %>%
  tab_style(locations=cells_body(columns=`D. Final 4 chefs`
                                 ,rows = `D. Final 4 chefs` >=.7 &
                                   `D. Final 4 chefs` < 1)
            ,style = cell_fill("#ffbc69")) %>%
  tab_style(locations=cells_body(columns=`D. Final 4 chefs`
                                 ,rows = `D. Final 4 chefs` == 1)
            ,style = cell_fill("gray90")) %>%
  tab_style(locations=cells_body(columns=`D. Final 4 chefs`
                                 ,rows = `D. Final 4 chefs` > 1 &
                                   `D. Final 4 chefs` < 2)
            ,style = cell_fill("#a3cce9")) %>%
  tab_style(locations=cells_body(columns=`D. Final 4 chefs`
                                 ,rows = `D. Final 4 chefs` >=2)
            ,style = cell_fill("#1170AA")) %>%
  tab_options(
    row_group.background.color = "gray95",
    table.font.color = "#323232",
    table_body.hlines.color = "#323232",
    table_body.border.top.color = "#323232",
    heading.border.bottom.color = "#323232",
    row_group.border.top.color = "#323232",
    column_labels.border.bottom.color = "#323232",
    row_group.border.bottom.color = "transparent"
    ,table.border.top.style = "transparent"
    ,table.border.bottom.style = "transparent"
  ) %>%
  opt_all_caps() %>%
  cols_width(season ~ px(165)
             ,`season #` ~ px(85)
             ,`A. All chefs, including those who didn't make it out of Qualifiers or LCK` ~ px(180)
             ,`C. Final 8 chefs` ~ px(90)
             ,`D. Final 4 chefs` ~ px(90)
             , everything() ~ px(125) )  %>%
  tab_header(
    title = "Ratio of women to men at different times in Top Chef seasons"
    ,subtitle = "Ratios above 1 indicate there were more women than men. Ratios under 1 indicate there were fewer men than women. Category A will be the same as Category B for seasons that didn't have Qualifiers or didn't have people competing in Last Chance Kitchen (LCK) who didn't make it into the main competition. There have not been nonbinary chefs on Top Chef, so this analysis just looks at two genders."
  )


gtsave(genderratiotable
       ,filename = paste(directory,"Gender ratio at different times.png"
                         ,sep=""))




