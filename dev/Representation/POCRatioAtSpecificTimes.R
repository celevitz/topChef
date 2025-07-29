rm(list=ls())
library(tidyverse)
library(openxlsx)
library(ggplot2)
library(gt)
library(nlme)
library(AICcmodavg)

directory <- "/Users/carlylevitz/Documents/Data/topChef/"
chefdetails <- read.csv(paste0(directory,"Top Chef - Chef details.csv"))  %>%
  filter(series == "US" ) %>%
  mutate(personOfColor = ifelse(is.na(personOfColor),"white",personOfColor))

guestjudges <- read.csv(paste0(directory,"Top Chef - Guest judges.csv"))  %>%
  filter(series == "US" ) %>%
  mutate(personOfColor = ifelse(is.na(personOfColor),"white",personOfColor)
         ,timing="E. Guest judges") %>%
  group_by(season,seasonNumber) %>%
  mutate(total = n()) %>%
  group_by(season,seasonNumber,total,personOfColor) %>%
  summarise(n=n()) %>%
  mutate(pocGJ = n/total) %>% ungroup() %>%
  filter(personOfColor == "POC") %>%
  select(seasonNumber,pocGJ)

## Proportion of restaurants in the state owned by a minority
## https://restaurant.org/getmedia/a618a883-6705-4018-8b1b-9c74d8d99d23/nra-data-brief-restaurant-owner-demographics-april-2025.pdf
state <- guestjudges %>% ungroup() %>%
  select(seasonNumber) %>%
  distinct() %>%
  mutate(minorityowned =case_when(seasonNumber %in% c(1,2,13,17) ~ .65
                                  ,seasonNumber %in% c(3) ~ .6
                                  ,seasonNumber %in% c(4) ~ .51
                                  ,seasonNumber %in% c(5,8) ~ .48
                                  ,seasonNumber %in% c(6) ~ .56
                                  ,seasonNumber %in% c(7) ~ .57
                                  ,seasonNumber %in% c(9,19) ~ .68
                                  ,seasonNumber %in% c(10) ~ .41
                                  ,seasonNumber %in% c(11) ~ .52
                                  ,seasonNumber %in% c(12) ~ .31
                                  ,seasonNumber %in% c(14) ~ .43
                                  ,seasonNumber %in% c(15) ~ .31
                                  ,seasonNumber %in% c(16) ~ .29
                                  ,seasonNumber %in% c(18) ~ .34
                                  ,seasonNumber %in% c(21) ~ .19
                                  ,TRUE ~ NA
                                ))

## First, POC ratio at start
starting <- chefdetails %>%
  select(season,seasonNumber,chef,personOfColor) %>%
  filter(!(is.na(personOfColor))) %>%
  group_by(season,seasonNumber,personOfColor) %>%
  summarise(n=n())%>%
  # how many are in the comp at that time?
  ungroup() %>%
  group_by(season,seasonNumber) %>%
  mutate(total = sum(n)
         ,timing="A. All chefs, including those who didn't make it out of Qualifiers or LCK")

startingOwners<- chefdetails %>%
  select(season,seasonNumber,chef,personOfColor,occupation_category) %>%
  filter(!(is.na(personOfColor)) &
         occupation_category == "Executive Chef and Owner/Partner/Founder") %>%
  group_by(season,seasonNumber,personOfColor) %>%
  summarise(n=n())%>%
  # how many are in the comp at that time?
  ungroup() %>%
  group_by(season,seasonNumber) %>%
  mutate(total = sum(n)
         ,timing="A. All chefs who were owners")

## POC ratio of only those who were in the official competition
officialcomp <- chefdetails %>%
  mutate(placement=as.numeric(placement)) %>%
  filter(!(is.na(personOfColor)) &
           # While Season 22 is ongoing, I need to include them since their
           # placement is NA for those still in the comp
           (!(is.na(placement)) | seasonNumber == 22) &
           !(chef %in% c("Ying Gao","Sam Olayinka"))
         ) %>%
  select(season,seasonNumber,chef,personOfColor) %>%
  group_by(season,seasonNumber,personOfColor) %>%
  summarise(n=n()) %>%
  # how many are in the comp at that time?
  ungroup() %>%
  group_by(season,seasonNumber) %>%
  mutate(total = sum(n)
         ,timing="B. Chefs with an official placement in their season")

## POC ratio of top 8 chefs
eight <- chefdetails %>%
  mutate(placement=as.numeric(placement)) %>%
  filter(!(is.na(personOfColor)) &
           # While Season 22 is ongoing, I need to include them since their
           # placement is NA for those still in the comp
           (placement <=8 |  (seasonNumber == 22 & is.na(placement)) ) &
           !(chef %in% c("Ying Gao","Sam Olayinka"))
        ) %>%
  select(season,seasonNumber,chef,personOfColor) %>%
  group_by(season,seasonNumber,personOfColor) %>%
  summarise(n=n()) %>%
  mutate(total=8
         ,timing = "C. Final 8 chefs")

## POC ratio of top 4 chefs
four <- chefdetails %>%
  mutate(placement=as.numeric(placement)) %>%
  filter(!(is.na(personOfColor)) &
           # While Season 22 is ongoing, I need to include them since their
           # placement is NA for those still in the comp
           (placement <=4 | (seasonNumber == 22 & is.na(placement)) ) &
           !(chef %in% c("Ying Gao","Sam Olayinka"))
  ) %>%
  select(season,seasonNumber,chef,personOfColor) %>%
  group_by(season,seasonNumber,personOfColor) %>%
  summarise(n=n()) %>%
  mutate(total=4
         ,timing = "D. Final 4 chefs")

## Bring the data together
alldata <- starting %>%
  bind_rows(officialcomp) %>%
  bind_rows(eight) %>%
  bind_rows(four) %>%
  full_join(state) %>%
  full_join(guestjudges) %>%
  # Get percent of group that are people of color
  mutate(percent = round(n/total,3)) %>%
  filter(personOfColor == "POC") %>%
  # get ratio of group that are POC
  mutate(ratio = round(n/(total-n),3))%>%
  select(!personOfColor) %>%
  # which ones have more POC and which have less?
  mutate(POC_balance = case_when(ratio < 1 ~ "A. Fewer people of color than white people"
                             ,ratio == 1 ~ "B. Same number of people color as white people"
                             ,ratio > 1 ~ "C. More people of color than white people"
                             ,TRUE ~ "needs to be categorized"))

####################################################
## I want to figure out what the time trends are
## My hypothesis is that there's a switch at each of the All Stars seasons
## So I went with an interrupted time series: https://rpubs.com/chrissyhroberts/1006858
## The figure 0 data can be used for a time trend visual, regardless of time series
  # Prep the data
  figure0data <- alldata %>%
    filter(timing %in% c("A. All chefs, including those who didn't make it out of Qualifiers or LCK"
                         ,"B. Chefs with an official placement in their season"
                         )) %>%
    select(season,seasonNumber,percent,timing) %>%
    arrange(desc(seasonNumber)) %>%
    mutate(timing = ifelse(grepl("A. All chefs",timing),"All chefs cast"
                           ,"Chefs with official placement")
           ,intervention = ifelse(seasonNumber <8,0,1)
           ,postinterventiontime = ifelse(seasonNumber>=8,seasonNumber-7,0)
           ,intervention2 = ifelse(seasonNumber > 17,1,0)
       ,postinterventiontime2 = ifelse(seasonNumber > 17,seasonNumber-17,0))%>%
  arrange(seasonNumber)

  # keep just the data for all chefs cast
  df <- figure0data %>% filter(timing == "All chefs cast")

  # Interrupted time series
    model.a <- gls(percent ~ seasonNumber + intervention +
                             postinterventiontime + intervention2 + postinterventiontime2
        ,data=df,method="ML")
    summary(model.a)

    # Estimated modeled values
    df<-df %>% left_join(
      data.frame(predictions = predictSE.gls (model.a, df, se.fit=T)$fit
                 ,se = predictSE.gls (model.a, df, se.fit=T)$se
                 ,seasonNumber = seq(1,max(df$seasonNumber),1)))

  # create counterfactual for if the trend from intervention 1 continued
    df2 <- df %>% filter(seasonNumber <= 17 & seasonNumber >=8)
    model.b <- gls(percent ~ seasonNumber
                   ,data=df2,method="ML")

      # estimated model values of counterfactual
      df <- df %>% left_join(
        data.frame(predictionsCF = predictSE.gls (model.b, df, se.fit=T)$fit
                   ,seCF = predictSE.gls (model.b, df, se.fit=T)$se
                   ,seasonNumber = seq(1,max(df$seasonNumber),1)) %>%
          filter(seasonNumber >= 17))

  # Interrupted time series plot
    # first "intervention" of the first all stars isn't significant
      figure3 <- ggplot(df,aes(seasonNumber,percent))+
        geom_segment(x=7.9,yend=max(df$seasonNumber),y=0)+
        geom_segment(x=17.1,yend=max(df$seasonNumber),y=0)+
        # counter factual
        geom_ribbon(aes(ymin = predictionsCF - (1.96*seCF)
                        , ymax = predictionsCF + (1.96*seCF)),fill = "lightblue"
                    ,alpha=.8)+
        geom_line(aes(seasonNumber,predictionsCF),color="blue",lty=2)+
        # interrupted time series
        geom_ribbon(aes(ymin = predictions - (1.96*se)
                        , ymax = predictions + (1.96*se)), fill = "lightgreen"
                    ,alpha=.8)+
        geom_line(aes(seasonNumber,predictions),color="forestgreen",lty=1)+
        geom_point(alpha=0.3) +
        labs(title="Figure 3. Racial diversity on Top Chef seasons"
            ,subtitle = "Modeled interrupted time series with a 95% confidence interval (green), and a counterfactual (blue with dotted line)\nCircles are observed percent of the cast that are people of color"
            ,caption="Analysis by Carly Levitz for Pack Your Knives") +
        scale_y_continuous(lim=c(0,1),breaks=seq(0,1,.2)
                           ,labels = paste0(seq(0,100,20),"%")
                         ,name="Proportion of cast that are people of color")+
        scale_x_continuous(breaks=seq(1,22,1),labels=seq(1,22,1)
                           ,name = "Season number") +
        theme(panel.background = element_rect(fill="white")
              ,plot.background = element_rect(fill="white")
              ,plot.title.position="plot",plot.caption = element_text(hjust=0,size=5)
              ,plot.caption.position="plot",plot.title = element_text(size=12)
              ,plot.subtitle = element_text(size=10)
              ,strip.text = element_text(colour = 'black',size=8)
              ,legend.text = element_text(size=7),legend.title = element_text(size=8)
              ,axis.line = element_line(color="black")
              ,axis.ticks = element_line(color="black")
              ,axis.text = element_text(color="black",size=8)
              ,panel.grid = element_blank())
      ggsave(paste0(directory,"Interrupted time series.png")
             ,figure3,width = 10,height = 5,dpi = 1200 )


  # Simple time trend plot
    model.c <- lm(figure0data$percent[figure0data$timing == "All chefs cast" ] ~
                figure0data$seasonNumber[figure0data$timing == "All chefs cast"])
    summary(model.c)

    figure0data <- figure0data %>% left_join(
        data.frame(predictions = predictSE.gls (model.c, df, se.fit=T)$fit
                   ,se = predictSE.gls (model.c, df, se.fit=T)$se
                   ,seasonNumber = seq(1,max(df$seasonNumber),1))
      )

    figure2 <- figure0data %>% filter(timing == "All chefs cast") %>%
      ggplot(aes(x=seasonNumber,y=percent )) +
      geom_ribbon(aes(ymin = predictions - (1.96*se)
                      , ymax = predictions + (1.96*se)), fill = "lightgreen"
                  ,alpha=.8)+
      geom_line(aes(seasonNumber,predictions),color="black",lty=1)+
      geom_point(alpha=0.5) +
      labs(title = "Figure 2. Relationship between season number and proportion of Top Chef cast that are people of color"
           ,subtitle = "Linear regression model with 95% confidence interval"
           ,caption="Analysis by Carly Levitz for Pack Your Knives")+
      scale_y_continuous(lim=c(0,1),breaks=seq(0,1,.2)
                         ,labels = paste0(seq(0,100,20),"%")
                         ,name="Proportion of cast that are people of color")+
      scale_x_continuous(breaks=seq(1,22,1),labels=seq(1,22,1)
                         ,name = "Season number") +
      theme(panel.background = element_rect(fill="white")
            ,plot.background = element_rect(fill="white")
            ,plot.title.position="plot",plot.caption = element_text(hjust=0,size=5)
            ,plot.caption.position="plot",plot.title = element_text(size=12)
            ,plot.subtitle = element_text(size=10)
            ,strip.text = element_text(colour = 'black',size=8)
            ,legend.text = element_text(size=7),legend.title = element_text(size=8)
            ,axis.line = element_line(color="black")
            ,axis.ticks = element_line(color="black")
            ,axis.text = element_text(color="black",size=8)
            ,panel.grid = element_blank())
    ggsave(paste0(directory,"Linear regression.png")
           ,figure2,width = 10,height = 5,dpi = 1200 )

    figure1 <- figure0data %>%
      rename(Cast = timing) %>%
      ggplot(aes(x=seasonNumber,y=percent,color=Cast)) +
      geom_line() +
      labs(title = "Figure 1. Proportion of Top Chef cast that are people of color"
           ,subtitle = "Comparing the full casts with the chefs who made it past qualifying challenges and Last Chance Kitchen"
           ,caption="Analysis by Carly Levitz for Pack Your Knives")+
      scale_y_continuous(lim=c(0,1),breaks=seq(0,1,.2)
                         ,labels = paste0(seq(0,100,20),"%")
                         ,name="Proportion of cast that are people of color")+
      scale_x_continuous(breaks=seq(1,22,1),labels=seq(1,22,1)
                         ,name = "Season number") +
      scale_color_manual(values=c("#1170AA","#a3cce9")) +
      theme(panel.background = element_rect(fill="white")
            ,plot.background = element_rect(fill="white")
            ,plot.title.position="plot",plot.caption = element_text(hjust=0,size=5)
            ,plot.caption.position="plot",plot.title = element_text(size=12)
            ,plot.subtitle = element_text(size=10)
            ,strip.text = element_text(colour = 'black',size=8)
            ,legend.text = element_text(size=7),legend.title = element_text(size=8)
            ,axis.line = element_line(color="black")
            ,axis.ticks = element_line(color="black")
            ,axis.text = element_text(color="black",size=8)
            ,panel.grid = element_blank())
    ggsave(paste0(directory,"General time trend.png")
           ,figure1,width = 10,height = 5,dpi = 1200 )

  # Descriptive statistics
    figure0data %>% group_by(timing) %>%
      summarise(mean=mean(percent),min=min(percent),max=max(percent))

    # for season 17 and prior
    figure0data %>%
      mutate(timing2 = ifelse(seasonNumber<=17,"01 to 17","18 onward")) %>%
      group_by(timing,timing2) %>%
      summarise(mean=mean(percent),min=min(percent),max=max(percent))


  # confirming non stat relationship from seasons 1 to 17
    model.d <- lm(figure0data$percent[figure0data$timing == "All chefs cast" &
                                        figure0data$seasonNumber <=17] ~
              figure0data$seasonNumber[figure0data$timing == "All chefs cast"&
                                               figure0data$seasonNumber <=17])
    summary(model.d)

#####################################################
## Compare context: % of restaurants owned by minorities
## Use the state in which the season was filmed
## THen maybe just go to the owners in each season?
    figure4data <- alldata %>%
      filter(timing %in% c("A. All chefs, including those who didn't make it out of Qualifiers or LCK") )%>%
      select(season,seasonNumber,percent,minorityowned) %>%
      distinct() %>%
      # bring on the % of business owners that are people of color
      full_join(startingOwners %>%
                  filter(personOfColor == "POC") %>%
                  mutate(percentOwners = n/total) %>%
                  select(seasonNumber,percentOwners)) %>%
        # some of the seasons have owners that are all white (season 4 and 10)
        # season 1 and 5 don't have any owners
        mutate(percentOwners=ifelse(seasonNumber %in% c(4,10),0
                                    ,percentOwners)) %>%
      mutate(category = case_when(minorityowned > percent ~ "Cast less representative than state\nrestaurant owner average"
                                  ,minorityowned < percent ~ "Cast more representative than state\nrestaurant owner average"
                                  ,minorityowned == percent ~ "Cast same as state restaurant owner average"
                                  ,TRUE ~ "Out of USA")
             ,category2 = case_when(minorityowned > percentOwners ~ "Cast (owners) less representative than state\nrestaurant owner average"
                                    ,minorityowned < percentOwners ~ "Cast (owners) more representative than state\nrestaurant owner average"
                                    ,minorityowned == percentOwners ~ "Cast (owners) same as state restaurant owner average"
                                    ,is.na(percentOwners) ~ "No owners"
                                    ,TRUE ~ "Out of USA")
             ,maincategory = case_when(minorityowned < percent & minorityowned < percentOwners ~ "Full cast and just owners both more\ndiverse than state restaurant owner average"
                                       ,minorityowned < percent & minorityowned > percentOwners ~ "Full cast more and just owners less\ndiverse than state restaurant owner average"
                                       ,minorityowned > percent & minorityowned < percentOwners ~ "Full cast less and just owners more\ndiverse than state restaurant owner average"
                                       ,minorityowned > percent & minorityowned > percentOwners ~ "Full cast and just owners less\ndiverse than state restaurant owner average"
                                       ,minorityowned > percent & is.na(percentOwners) ~ "Full cast less diverse than state restaurant owner\naverage (no owners on cast)"
                                       ,is.na(minorityowned) ~ "Not filmed in USA"
                                       ,TRUE ~ "CATEGORIZATION NEEDED"
                                       )) %>%
      select(!c(category,category2)) %>%
      pivot_longer(!c(season,seasonNumber,maincategory),names_to="Legend"
                   ,values_to="values") %>%
      mutate(Legend = case_when(Legend == "minorityowned" ~ "state's restaurant owners in 2025"
                               ,Legend == "percent" ~ "season's cast"
                               ,Legend == "percentOwners" ~ "season's cast that were owners"))

    figure4 <- figure4data %>%
      ggplot(aes(x=values,y=seasonNumber,color=Legend,shape=Legend,fill=Legend)) +
      facet_wrap(maincategory~.) + geom_point(size=2.5) +
      scale_y_continuous(breaks=seq(1,22,1),labels=paste0("Season ",seq(1,22,1))
                         ,name=NULL)+
      scale_x_continuous(breaks=seq(0,1,.1),labels=c("0%","","20%","","40%","","60%","","80%","","100%")
                         ,name = "% of group that is a minority or people of color")+
      ggtitle("Figure 4: Comparing Top Chef seasons' casts to the demographics of restaurant owners"
              ,subtitle = "Demographics of restaurant owners by state comes from the National Restaurant's Association's April 2025 report.") +
      labs(caption="Created by Carly Levitz for Pack Your Knives")+
      scale_color_manual(values=c("#1170AA","#a3cce9","#c85200")) +
      scale_fill_manual(values=c("#1170AA",NA,"#c85200")) +
      scale_shape_manual(values=c(24,17,1) ) +
      theme(panel.background = element_rect(fill="white")
            ,plot.background = element_rect(fill="white")
            ,plot.title.position="plot",plot.caption = element_text(hjust=0,size=7)
            ,plot.caption.position="plot",plot.title = element_text(size=13)
            ,plot.subtitle = element_text(size=11)
            ,strip.text = element_text(colour = 'black',size=7)
            ,legend.text = element_text(size=7),legend.title = element_text(size=8)
            ,axis.line = element_line(color="black")
            ,axis.ticks = element_line(color="black")
            ,axis.text = element_text(color="black",size=7)
            ,panel.grid.major.y = element_line(color="gray95",linewidth=.2)
            ,panel.grid.major.x = element_line(color="gray95",linewidth=.2)
            ,legend.position="top"
            ,legend.margin=margin(c(.1,5,.1,5)))
    ggsave(paste0(directory,"Comparing casts to restaurant owners.png")
           ,figure4,width = 8,height = 7,dpi = 1200 )


























####################################################
## starting %, state %, GJ %

#
# ##################################
# ## Visualize it
#
# facetviz <- alldata %>%
#   mutate(timing = ifelse(timing %in% "A. All chefs, including those who didn't make it out of Qualifiers or LCK"
#                          ,"A. All chefs, including those who didn't\nmake it out of Qualifiers or LCK"
#                          ,timing)
#          ,timing = ifelse(timing %in% "B. Chefs with an official placement in their season"
#                           ,"B. Chefs with an official\nplacement in their season"
#                           ,timing)) %>%
#   ggplot(aes(x=seasonNumber,y=ratio
#              ,color=POC_balance,shape=POC_balance,fill=POC_balance))+
#   geom_point(size=1) +scale_x_continuous("Season #") + facet_wrap(~timing) +
#   scale_y_continuous(limits=c(0,4.6),"Ratio of people of color to white people") +
#   guides(shape=guide_legend(title="Balance",override.aes=list(size=1))
#         ,fill=guide_legend(title="Balance")
#         ,color=guide_legend(title="Balance")) +
#   ggtitle("Figure 1: Ratio of people of color to white people at different times of Top Chef seasons"
#           ,subtitle = "Values under 1 indicate fewer people of color than white people; values over 1 indicate more people of color than white people") +
#   labs(caption="Created by Carly Levitz for Pack Your Knives")+
#   scale_color_manual(values=c("#c85200","gray60","#1170AA")) +
#   scale_fill_manual(values=c("#c85200",NA,"#1170AA")) +
#   scale_shape_manual(values=c(25,16,24) ) +
#   theme(#panel.grid = element_blank()
#     panel.background = element_rect(fill="white")
#     ,plot.background = element_rect(fill="white")
#     ,plot.title.position="plot"
#     ,plot.caption = element_text(hjust=0,size=5)
#     ,plot.caption.position="plot"
#     ,strip.background =element_rect(fill="gray90")
#     ,strip.text = element_text(colour = 'black',size=8)
#     ,plot.title = element_text(size=12)
#     ,plot.subtitle = element_text(size=10)
#     ,legend.text = element_text(size=7)
#     ,legend.title = element_text(size=8)
#     ,axis.line = element_line(color="black")
#     ,axis.ticks = element_line(color="black")
#     ,axis.text = element_text(color="black"))
# ggsave(paste0(directory,"RE ratio at different times - facet scatter.png")
#        ,facetviz,width = 8,height = 4,dpi = 1200 )
#
# ##################################
# # Table format
# REratiotableData <- alldata %>%
#   ungroup() %>%
#   select(season,seasonNumber,ratio,timing) %>%
#   pivot_wider(names_from=timing,values_from=ratio) %>%
#   arrange(seasonNumber) %>%
#   rename(`season #`=seasonNumber) %>%
#   ## Category of season
#   mutate(category = case_when(
#     # B < C < D = got better over time
#     # B > C > D = got worse over time
#     # B = C = D
#     `B. Chefs with an official placement in their season` > `C. Final 8 chefs`  &
#        `C. Final 8 chefs`  > `D. Final 4 chefs` ~ "Worsened over time"
#     ,`B. Chefs with an official placement in their season` < `C. Final 8 chefs`  &
#       `C. Final 8 chefs`  < `D. Final 4 chefs` ~ "Improved over time"
#     ,`B. Chefs with an official placement in their season` == `C. Final 8 chefs`  &
#       `C. Final 8 chefs`  == `D. Final 4 chefs` ~ "Stayed the same"
#     # B > C, C = D: worsened, flattened
#     ,`B. Chefs with an official placement in their season` > `C. Final 8 chefs`  &
#       `C. Final 8 chefs`  == `D. Final 4 chefs` ~ "Worsened, then stayed the same"
#     # B < C, D < B: Got better, worsened more than at start
#     ,`B. Chefs with an official placement in their season` < `C. Final 8 chefs`  &
#       `B. Chefs with an official placement in their season` > `D. Final 4 chefs` ~ "Improved, ended up worse than at start"
#     # B > C, D > B, worsened, got better than at the start
#     ,`B. Chefs with an official placement in their season` > `C. Final 8 chefs`  &
#       `B. Chefs with an official placement in their season` < `D. Final 4 chefs` ~ "Worsened, got better than at the start"
#     # B = C, C < D: same, then improved
#     ,`B. Chefs with an official placement in their season` = `C. Final 8 chefs`  &
#       `C. Final 8 chefs` < `D. Final 4 chefs` ~ "Same, then improved"
#     # B < C, C = D: improved, flattened
#     ,`B. Chefs with an official placement in their season` < `C. Final 8 chefs`  &
#       `C. Final 8 chefs` == `D. Final 4 chefs` ~ "Improved, then stayed the same"
#     ,TRUE ~ "Mixed"
#   ))
#
# REratiotable <- REratiotableData %>%
#   gt() %>%
#   cols_hide(columns=category) %>%
#   tab_source_note(source_note = "Created by Carly Levitz for Pack Your Knives") %>%
#   tab_options(data_row.padding = px(1),
#               column_labels.padding = px(1),
#               row_group.padding = px(1))  %>%
#   tab_style(style = cell_text(align = "right"),locations = cells_source_notes()) %>%
#   tab_style(style = cell_text(align = "left",weight="bold")
#             ,locations = cells_title(groups="title")) %>%
#   tab_style(style = cell_text(align = "left",weight="bold")
#             ,locations = cells_row_groups() ) %>%
#   tab_style(style = cell_text(align = "left")
#             ,locations = cells_title(groups="subtitle")) %>%
#   tab_style(style = cell_text(align = "center")
#             ,locations = cells_body(columns=!season)) %>%
#   tab_style(style = cell_text(align = "center",weight="bold")
#             ,locations = cells_column_labels(columns=!season)) %>%
#   tab_style(style = cell_text(align = "left",weight="bold")
#             ,locations = cells_column_labels(columns=season)) %>%
#   # Color the cells based on their value
#   # Category A
#     tab_style(locations=cells_body(columns=`A. All chefs, including those who didn't make it out of Qualifiers or LCK`
#                                    ,rows = `A. All chefs, including those who didn't make it out of Qualifiers or LCK` <.7)
#               ,style = cell_fill("#c85200")) %>%
#     tab_style(locations=cells_body(columns=`A. All chefs, including those who didn't make it out of Qualifiers or LCK`
#                                    ,rows = `A. All chefs, including those who didn't make it out of Qualifiers or LCK` <.7)
#               ,style = cell_text("white")) %>%
#     tab_style(locations=cells_body(columns=`A. All chefs, including those who didn't make it out of Qualifiers or LCK`
#                                    ,rows = `A. All chefs, including those who didn't make it out of Qualifiers or LCK` >=.7 &
#                                      `A. All chefs, including those who didn't make it out of Qualifiers or LCK` < 1)
#               ,style = cell_fill("#ffbc69")) %>%
#     tab_style(locations=cells_body(columns=`A. All chefs, including those who didn't make it out of Qualifiers or LCK`
#                                  ,rows = `A. All chefs, including those who didn't make it out of Qualifiers or LCK` == 1)
#               ,style = cell_fill("gray90")) %>%
#     tab_style(locations=cells_body(columns=`A. All chefs, including those who didn't make it out of Qualifiers or LCK`
#                                    ,rows = `A. All chefs, including those who didn't make it out of Qualifiers or LCK` > 1 &
#                                      `A. All chefs, including those who didn't make it out of Qualifiers or LCK` < 2)
#               ,style = cell_fill("#a3cce9")) %>%
#     tab_style(locations=cells_body(columns=`A. All chefs, including those who didn't make it out of Qualifiers or LCK`
#                                    ,rows = `A. All chefs, including those who didn't make it out of Qualifiers or LCK` >=2)
#               ,style = cell_fill("#1170AA")) %>%
#     tab_style(locations=cells_body(columns=`A. All chefs, including those who didn't make it out of Qualifiers or LCK`
#                                    ,rows = `A. All chefs, including those who didn't make it out of Qualifiers or LCK` >=2)
#               ,style = cell_text("white")) %>%
#   # Category B
#   tab_style(locations=cells_body(columns=`B. Chefs with an official placement in their season`
#                                  ,rows = `B. Chefs with an official placement in their season` <.7)
#             ,style = cell_fill("#c85200")) %>%
#   tab_style(locations=cells_body(columns=`B. Chefs with an official placement in their season`
#                                  ,rows = `B. Chefs with an official placement in their season` <.7)
#             ,style = cell_text("white")) %>%
#   tab_style(locations=cells_body(columns=`B. Chefs with an official placement in their season`
#                                  ,rows = `B. Chefs with an official placement in their season` >=.7 &
#                                    `B. Chefs with an official placement in their season` < 1)
#             ,style = cell_fill("#ffbc69")) %>%
#   tab_style(locations=cells_body(columns=`B. Chefs with an official placement in their season`
#                                  ,rows = `B. Chefs with an official placement in their season` == 1)
#             ,style = cell_fill("gray90")) %>%
#   tab_style(locations=cells_body(columns=`B. Chefs with an official placement in their season`
#                                  ,rows = `B. Chefs with an official placement in their season` > 1 &
#                                    `B. Chefs with an official placement in their season` < 2)
#             ,style = cell_fill("#a3cce9")) %>%
#   tab_style(locations=cells_body(columns=`B. Chefs with an official placement in their season`
#                                  ,rows = `B. Chefs with an official placement in their season` >=2)
#             ,style = cell_fill("#1170AA")) %>%
#   tab_style(locations=cells_body(columns=`B. Chefs with an official placement in their season`
#                                  ,rows = `B. Chefs with an official placement in their season` >=2)
#             ,style = cell_text("white")) %>%
#   # Category C
#   tab_style(locations=cells_body(columns=`C. Final 8 chefs`
#                                  ,rows = `C. Final 8 chefs` <.7)
#             ,style = cell_fill("#c85200")) %>%
#   tab_style(locations=cells_body(columns=`C. Final 8 chefs`
#                                  ,rows = `C. Final 8 chefs` <.7)
#             ,style = cell_text("white")) %>%
#   tab_style(locations=cells_body(columns=`C. Final 8 chefs`
#                                  ,rows = `C. Final 8 chefs` >=.7 &
#                                           `C. Final 8 chefs` < 1)
#             ,style = cell_fill("#ffbc69")) %>%
#   tab_style(locations=cells_body(columns=`C. Final 8 chefs`
#                                  ,rows = `C. Final 8 chefs` == 1)
#             ,style = cell_fill("gray90")) %>%
#   tab_style(locations=cells_body(columns=`C. Final 8 chefs`
#                                  ,rows = `C. Final 8 chefs` > 1 &
#                                          `C. Final 8 chefs` < 2)
#             ,style = cell_fill("#a3cce9")) %>%
#   tab_style(locations=cells_body(columns=`C. Final 8 chefs`
#                                  ,rows = `C. Final 8 chefs` >=2)
#             ,style = cell_fill("#1170AA")) %>%
#   tab_style(locations=cells_body(columns=`C. Final 8 chefs`
#                                  ,rows = `C. Final 8 chefs` >=2)
#             ,style = cell_text("white")) %>%
#   # Category D
#   tab_style(locations=cells_body(columns=`D. Final 4 chefs`
#                                  ,rows = `D. Final 4 chefs` <.7)
#             ,style = cell_fill("#c85200")) %>%
#   tab_style(locations=cells_body(columns=`D. Final 4 chefs`
#                                  ,rows = `D. Final 4 chefs` <.7)
#             ,style = cell_text("white")) %>%
#   tab_style(locations=cells_body(columns=`D. Final 4 chefs`
#                                  ,rows = `D. Final 4 chefs` >=.7 &
#                                    `D. Final 4 chefs` < 1)
#             ,style = cell_fill("#ffbc69")) %>%
#   tab_style(locations=cells_body(columns=`D. Final 4 chefs`
#                                  ,rows = `D. Final 4 chefs` == 1)
#             ,style = cell_fill("gray90")) %>%
#   tab_style(locations=cells_body(columns=`D. Final 4 chefs`
#                                  ,rows = `D. Final 4 chefs` > 1 &
#                                    `D. Final 4 chefs` < 2)
#             ,style = cell_fill("#a3cce9")) %>%
#   tab_style(locations=cells_body(columns=`D. Final 4 chefs`
#                                  ,rows = `D. Final 4 chefs` >=2)
#             ,style = cell_fill("#1170AA")) %>%
#   tab_style(locations=cells_body(columns=`D. Final 4 chefs`
#                                  ,rows = `D. Final 4 chefs` >=2)
#             ,style = cell_text("white")) %>%
#   tab_options(
#     row_group.background.color = "gray95",
#     table.font.color = "#323232",
#     table_body.hlines.color = "#323232",
#     table_body.border.top.color = "#323232",
#     heading.border.bottom.color = "#323232",
#     row_group.border.top.color = "#323232",
#     column_labels.border.bottom.color = "#323232",
#     row_group.border.bottom.color = "transparent"
#     ,table.border.top.style = "transparent"
#     ,table.border.bottom.style = "transparent"
#   ) %>%
#   opt_all_caps() %>%
#   cols_width(season ~ px(165)
#              ,`season #` ~ px(85)
#              ,`A. All chefs, including those who didn't make it out of Qualifiers or LCK` ~ px(180)
#              ,`C. Final 8 chefs` ~ px(90)
#              ,`D. Final 4 chefs` ~ px(90)
#              , everything() ~ px(125) )  %>%
#   tab_header(
#     title = "Table 1: Ratio of people of color to white people at different times in Top Chef seasons"
#     ,subtitle = "Ratios above 1 indicate there were more people of color than white people. Ratios under 1 indicate there were fewer people of color than white people. Category A will be the same as Category B for seasons that didn't have Qualifiers or didn't have people competing in Last Chance Kitchen (LCK) who didn't make it into the main competition."
#   )
#
#
# gtsave(REratiotable
#        ,filename = paste(directory,"RE ratio at different times.png"
#                          ,sep=""))
#
#
# ## Gender ratio at final 4 compared to winners' gender
# final4 <- alldata %>%
#   filter(timing == "D. Final 4 chefs") %>%
#   select(season,seasonNumber,ratio) %>%
#   left_join(chefdetails %>%
#               filter(placement == "1.0") %>%
#               select(season,seasonNumber,personOfColor) %>%
#               rename(WinnerRE=personOfColor)
#             ) %>%
#   # drop season 22 since they don't have a winner yet
#   filter(!(is.na(WinnerRE)))
#
# final4summary <- final4 %>%
#   group_by(ratio,WinnerRE) %>%
#   summarise(n=n()) %>%
#   ungroup() %>% group_by(ratio) %>%
#   mutate(total = sum(n)
#          ,percent = n/total
#          ,ratio = as.character(ratio)) %>%
#   select(!n) %>%
#   pivot_wider(values_from=percent,names_from=WinnerRE) %>%
#   mutate(white = ifelse(is.na(white),0,white)) %>%
#   pivot_longer(!c(ratio,total),names_to="Winner's race"
#                ,values_to = "percent") %>%
#   mutate(ratio = paste0("Ratio of ",ratio,"\n(",total," seasons)",sep=""))
#
#
# final4pie <- final4summary %>%
#   ggplot(aes(x=1,y=percent,fill=`Winner's race`)) +
#   geom_bar(stat="identity",width=2) +
#   coord_polar(theta="y") +
#   facet_wrap(~ratio) +
#   scale_fill_manual(values=c("#1170AA","#ffbc69"))+
#   ggtitle("Figure 2. Ratio of people of color to white people at the final four of Top Chef seasons and the gender\nof winners in those seasons"
#           ,subtitle = "Ratios above 1 indicate there were more people of color than white people. Ratios under 1 indicate there were fewer people of color than white people.") +
#   labs(caption="Created by Carly Levitz for Pack Your Knives")+
#   theme(
#     axis.ticks = element_blank()
#     ,axis.title = element_blank()
#     ,axis.text = element_blank()
#     ,panel.grid = element_blank()
#   )
#
# ggsave(paste0(directory
#               ,"RE ratio at F4 and winner gender.png")
#        ,final4pie,width = 8,height = 4,dpi = 1200 )
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
