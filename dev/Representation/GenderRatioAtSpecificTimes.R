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
           (!(is.na(placement)) | seasonNumber == 22) &
           !(chef %in% c("Ying Gao","Sam Olayinka"))
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
           (placement <=8 |  (seasonNumber == 22 & is.na(placement)) ) &
           !(chef %in% c("Ying Gao","Sam Olayinka"))
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
           (placement <=4 | (seasonNumber == 22 & is.na(placement)) ) &
           !(chef %in% c("Ying Gao","Sam Olayinka"))
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

facetviz <- alldata %>%
  mutate(timing = ifelse(timing %in% "A. All chefs, including those who didn't make it out of Qualifiers or LCK"
                         ,"A. All chefs, including those who didn't\nmake it out of Qualifiers or LCK"
                         ,timing)
         ,timing = ifelse(timing %in% "B. Chefs with an official placement in their season"
                          ,"B. Chefs with an official\nplacement in their season"
                          ,timing)) %>%
  ggplot(aes(x=seasonNumber,y=ratio
             ,color=Gender_balance,shape=Gender_balance,fill=Gender_balance))+
  geom_point(size=1) +scale_x_continuous("Season #") + facet_wrap(~timing) +
  scale_y_continuous(limits=c(0,3.1),"Ratio of women to men") +
  guides(shape=guide_legend(title="Gender balance",override.aes=list(size=1))
        ,fill=guide_legend(title="Gender balance")
        ,color=guide_legend(title="Gender balance")) +
  ggtitle("Figure 1: Ratio of women to men at different times of Top Chef seasons"
          ,subtitle = "Values under 1 indicate fewer women than men; values over 1 indicate more women than men") +
  labs(caption="Created by Carly Levitz for Pack Your Knives")+
  scale_color_manual(values=c("#c85200","gray60","#1170AA")) +
  scale_fill_manual(values=c("#c85200",NA,"#1170AA")) +
  scale_shape_manual(values=c(25,16,24) ) +
  theme(#panel.grid = element_blank()
    panel.background = element_rect(fill="white")
    ,plot.background = element_rect(fill="white")
    ,plot.title.position="plot"
    ,plot.caption = element_text(hjust=0,size=5)
    ,plot.caption.position="plot"
    ,strip.background =element_rect(fill="gray90")
    ,strip.text = element_text(colour = 'black',size=8)
    ,plot.title = element_text(size=12)
    ,plot.subtitle = element_text(size=10)
    ,legend.text = element_text(size=7)
    ,legend.title = element_text(size=8)
    ,axis.line = element_line(color="black")
    ,axis.ticks = element_line(color="black")
    ,axis.text = element_text(color="black"))
ggsave(paste0(directory,"Gender ratio at different times - facet scatter.png")
       ,facetviz,width = 8,height = 4,dpi = 1200 )

##################################
# Table format
genderratiotableData <- alldata %>%
  ungroup() %>%
  select(season,seasonNumber,ratio,timing) %>%
  pivot_wider(names_from=timing,values_from=ratio) %>%
  arrange(seasonNumber) %>%
  rename(`season #`=seasonNumber) %>%
  ## Category of season
  mutate(category = case_when(
    # B < C < D = got better over time
    # B > C > D = got worse over time
    # B = C = D
    `B. Chefs with an official placement in their season` > `C. Final 8 chefs`  &
       `C. Final 8 chefs`  > `D. Final 4 chefs` ~ "Worsened over time"
    ,`B. Chefs with an official placement in their season` < `C. Final 8 chefs`  &
      `C. Final 8 chefs`  < `D. Final 4 chefs` ~ "Improved over time"
    ,`B. Chefs with an official placement in their season` == `C. Final 8 chefs`  &
      `C. Final 8 chefs`  == `D. Final 4 chefs` ~ "Stayed the same"
    # B > C, C = D: worsened, flattened
    ,`B. Chefs with an official placement in their season` > `C. Final 8 chefs`  &
      `C. Final 8 chefs`  == `D. Final 4 chefs` ~ "Worsened, then stayed the same"
    # B < C, D < B: Got better, worsened more than at start
    ,`B. Chefs with an official placement in their season` < `C. Final 8 chefs`  &
      `B. Chefs with an official placement in their season` > `D. Final 4 chefs` ~ "Improved, ended up worse than at start"
    # B > C, D > B, worsened, got better than at the start
    ,`B. Chefs with an official placement in their season` > `C. Final 8 chefs`  &
      `B. Chefs with an official placement in their season` < `D. Final 4 chefs` ~ "Worsened, got better than at the start"
    # B = C, C < D: same, then improved
    ,`B. Chefs with an official placement in their season` = `C. Final 8 chefs`  &
      `C. Final 8 chefs` < `D. Final 4 chefs` ~ "Same, then improved"
    # B < C, C = D: improved, flattened
    ,`B. Chefs with an official placement in their season` < `C. Final 8 chefs`  &
      `C. Final 8 chefs` == `D. Final 4 chefs` ~ "Improved, then stayed the same"
    ,TRUE ~ "Mixed"
  ))

genderratiotable <- genderratiotableData %>%
  gt() %>%
  cols_hide(columns=category) %>%
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
    tab_style(locations=cells_body(columns=`A. All chefs, including those who didn't make it out of Qualifiers or LCK`
                                   ,rows = `A. All chefs, including those who didn't make it out of Qualifiers or LCK` >=2)
              ,style = cell_text("white")) %>%
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
  tab_style(locations=cells_body(columns=`B. Chefs with an official placement in their season`
                                 ,rows = `B. Chefs with an official placement in their season` >=2)
            ,style = cell_text("white")) %>%
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
  tab_style(locations=cells_body(columns=`C. Final 8 chefs`
                                 ,rows = `C. Final 8 chefs` >=2)
            ,style = cell_text("white")) %>%
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
  tab_style(locations=cells_body(columns=`D. Final 4 chefs`
                                 ,rows = `D. Final 4 chefs` >=2)
            ,style = cell_text("white")) %>%
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
    title = "Table 1: Ratio of women to men at different times in Top Chef seasons"
    ,subtitle = "Ratios above 1 indicate there were more women than men. Ratios under 1 indicate there were fewer men than women. Category A will be the same as Category B for seasons that didn't have Qualifiers or didn't have people competing in Last Chance Kitchen (LCK) who didn't make it into the main competition. There have not been nonbinary chefs on Top Chef, so this analysis just looks at two genders."
  )


gtsave(genderratiotable
       ,filename = paste(directory,"Gender ratio at different times.png"
                         ,sep=""))


## Gender ratio at final 4 compared to winners' gender
final4 <- alldata %>%
  filter(timing == "D. Final 4 chefs") %>%
  select(season,seasonNumber,ratio) %>%
  left_join(chefdetails %>%
              filter(placement == "1.0") %>%
              select(season,seasonNumber,gender) %>%
              rename(WinnerGender=gender)
            ) %>%
  # drop season 22 since they don't have a winner yet
  filter(!(is.na(WinnerGender)))

final4summary <- final4 %>%
  group_by(ratio,WinnerGender) %>%
  summarise(n=n()) %>%
  ungroup() %>% group_by(ratio) %>%
  mutate(total = sum(n)
         ,percent = n/total
         ,ratio = as.character(ratio)) %>%
  select(!n) %>%
  pivot_wider(values_from=percent,names_from=WinnerGender) %>%
  mutate(Female = ifelse(is.na(Female),0,Female)) %>%
  pivot_longer(!c(ratio,total),names_to="Winner's gender"
               ,values_to = "percent") %>%
  mutate(ratio = paste0("Ratio of ",ratio,"\n(",total," seasons)",sep=""))


final4pie <- final4summary %>%
  ggplot(aes(x=1,y=percent,fill=`Winner's gender`)) +
  geom_bar(stat="identity",width=2) +
  coord_polar(theta="y") +
  facet_wrap(~ratio) +
  scale_fill_manual(values=c("#1170AA","#ffbc69"))+
  ggtitle("Figure 2. Ratio of women to men at the final four of Top Chef seasons and the gender\nof winners in those seasons"
          ,subtitle = "Values under 1 indicate fewer women than men; values over 1 indicate more women than men") +
  labs(caption="Created by Carly Levitz for Pack Your Knives")+
  theme(
    axis.ticks = element_blank()
    ,axis.title = element_blank()
    ,axis.text = element_blank()
    ,panel.grid = element_blank()
  )

ggsave(paste0(directory
              ,"Gender ratio at F4 and winner gender.png")
       ,final4pie,width = 8,height = 4,dpi = 1200 )
















