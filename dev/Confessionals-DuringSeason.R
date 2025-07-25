## Bring in data from Excel file

rm(list=ls())
library(tidyverse)
library(gt)
library(ggplot2)
library(openxlsx)
library(RColorBrewer)

directory <- "/Users/carlylevitz/Documents/Data/"

currentep <- 13

accent <- brewer.pal(n = 9, name = "PuBuGn")[9]


## bring in the data
  confs <- read.csv(paste0(directory
                           ,"/topChef/Top Chef - Confessionals in a season.csv")
            ,header=TRUE)
  confsByEpi <- read.csv(paste0(directory
                            ,"/topChef/Top Chef - Confessionals by episode.csv")
                         ,header=TRUE)

  placement <- read.csv(paste0(directory,"topChef/Top Chef - Chef details.csv")
                        ,header=TRUE)
  placement <- placement %>%
    select(series,season,seasonNumber,chef,placement)


## Season specific confessionals
  s22epi <- confsByEpi %>%
    ungroup() %>%
    filter(seasonNumber == 22 & series == "US") %>%
    select(episode,chef,count,first,percentofEpsConfs,equalInEpPercent
           ,edit,winner) %>%
    mutate(labelpercent = ifelse(!(is.na(first))
                                 # first confessional of episode
                            ,paste0("*",round(percentofEpsConfs*100,1),"%")
                            ,paste0(round(percentofEpsConfs*100,1),"%"))
           # won elim challenge
           ,labelpercent = ifelse(!(is.na(winner))
                                  ,paste0(labelpercent,"#"),labelpercent)
           ) %>%
    left_join(placement %>%
                filter(series == "US" & seasonNumber == 22) %>%
                mutate(placement = as.numeric(placement))) %>%
    group_by(chef) %>%
    mutate(chefstotal = sum(count,na.rm=T)) %>%
    ungroup() %>% group_by(episode) %>%
    mutate(episodeottal = sum(count,na.rm=T)
           # fill in the ones that don't have a placement
           #,placement = case_when(is.na(placement) & chef == )
           ,chef = paste0(chef," (n=",chefstotal,")")
           )


  s22epi <- s22epi[order(s22epi$placement,s22epi$chefstotal
                         ,decreasing = c(T,F)),]
  s22epi$chef <- factor(s22epi$chef,levels=unique(s22epi$chef))
  s22epi$edit <- factor(s22epi$edit, levels = c("Under-shown","Shown as expected"
                                                ,"Over-shown"))




## Confessional edit by episode
  s22epi %>%
    ggplot(aes(x=episode,y=chef,label=labelpercent,fill=edit)) +
    geom_tile(aes(fill=edit)) +
    geom_tile(data=s22epi %>% filter(!(is.na(first)))
              ,aes(x=episode
                   ,y=chef
                   ,color="black")) +
    geom_text(aes(x=episode,y=chef,label=labelpercent)
              ,color="white",size=8) +
    scale_fill_manual(values=c("#c85200","grey75","#1170AA")) +
    scale_color_manual(values=c("black")) +
    scale_x_continuous(lim=c(0.5,currentep+.5),breaks=c(seq(1,currentep,1))
                       ,labels=c(paste0("Ep. ",seq(1,currentep,1)))
                       ,position="top"
                       ,"") +
    labs(title="% of total confessionals in each episode belonging to each chef"
         ,subtitle="Each chef's confessional total is compared to the number if the episode's confessionals were evenly distributed"
         ,caption = "* = first confessional of episode. # = won elimination challenge. Created by Carly Levitz for Pack Your Knives") +
    guides(color="none") +
    theme_minimal() +
    theme(panel.grid = element_blank()
          ,axis.ticks = element_blank()
          ,axis.text.y = element_text(color="black",size = 20)
          ,axis.text.x = element_text(color="black",size = 16)
          ,axis.title = element_text(color="black",size = 20,face="bold")
          ,plot.title = element_text(color="black",size = 30,face="bold")
          ,plot.subtitle = element_text(color="black",size=20)
          ,plot.caption = element_text(color="black",size=20,hjust=.5)
          ,panel.background = element_rect(color="white", fill="white")
          ,plot.background  = element_rect(color="white", fill="white")
          ,plot.margin = margin(t=25,r=10,l=10,b=10)
          ,legend.key.size = unit(1, 'cm')
          ,legend.key.height = unit(1, 'cm')
          ,legend.key.width = unit(1, 'cm')
          ,legend.title = element_text(size=18)
          ,legend.text = element_text(size=16)
          )

  dev.print(png, file = paste0(directory,"topChef/S22ConfessionalsByEpisode.png")
            , width = 1600, height = 900)
  dev.off()

## Average edit
## Number of first confessionals
  s22chefs <- confs %>%
    filter(seasonNumber == 22) %>%
    ungroup() %>%
    select(chef,firstconfs,chefconfs,expectedpercentofconfs,observedpercent) %>%
    # remove the chefs that didn't make it out of LCK
    filter(!(chef %in% c("Sam Olayinka","Ying Gao"))) %>%
    mutate(`Percentage point difference` = observedpercent-expectedpercentofconfs
      ,edit = case_when(observedpercent-expectedpercentofconfs == 0 ~ "Shown as expected"
                            ,observedpercent-expectedpercentofconfs > 0 ~ "Over-shown"
                            ,observedpercent-expectedpercentofconfs < 0 ~ "Under-shown"
                            ,TRUE ~ "missing")) %>%
    arrange(desc(observedpercent)) %>%
    rename(`# of times with first confessional of episode`=firstconfs
           ,`Total confessionals`=chefconfs
           ,`Expected % of confessionals`=expectedpercentofconfs
           ,`Observed % of confessionals`=observedpercent) %>%
    mutate(`Expected % of confessionals`=paste0(round(`Expected % of confessionals`*100,1),"%")
           ,`Observed % of confessionals`=paste0(round(`Observed % of confessionals`*100,1),"%")
           ,`Percentage point difference`=round(`Percentage point difference`*100,1)
           )

  confessionalstattable <- s22chefs %>%
    gt() %>%
    #cols_hide(columns=c(OverallRank,inCompetition)) %>%
    tab_source_note(source_note = "Created by Carly Levitz for Pack Your Knives") %>%
    #tab_row_group(label = "Eliminated",rows = inCompetition==0) %>%
    #tab_row_group(label = "In Last Chance Kitchen",rows = inCompetition==1) %>%
    #tab_row_group(label = "In the competition",rows = inCompetition==2) %>%
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
              ,locations = cells_body(columns=!chef)) %>%
    tab_style(style = cell_text(align = "center",weight="bold")
              ,locations = cells_column_labels(columns=!chef)) %>%
    tab_style(style = cell_text(align = "left",weight="bold")
              ,locations = cells_column_labels(columns=chef)) %>%
    tab_style(style = cell_text(align = "center",weight="bold")
              ,locations = cells_column_spanners()) %>%
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
    cols_width(chef ~ px(160)
               ,`# of times with first confessional of episode` ~ px(160)
               , everything() ~ px(130) )  %>%
    tab_header(
      title = paste0("Top Chef Destination Canada confessional data through episode ",currentep)
      ,subtitle = "Expected % of confessionals takes into account how many episodes they were in and how many chefs were in that episode.\nObserved % of confessionals is the percent of all confessionals in the season so far."
    ) %>%
    data_color(method="numeric",
               columns=`# of times with first confessional of episode`,
               palette=c("#a3cce9"#,"#5fa2ce"
                         ,"#1170AA","#141B41"
                         ),
               domain=c(0,max(s22chefs$`# of times with first confessional of episode`))) %>%
    data_color(method = "factor"
               ,columns = edit
               ,palette=c("#1170AA","gray80")) %>%
    data_color(method="numeric",
               columns=`Percentage point difference`,
               palette=c("#c85200","#ffbc69","gray80", "#a3cce9","#1170AA"),
               domain=c(min(s22chefs$`Percentage point difference`)
                        ,max(s22chefs$`Percentage point difference`)))


  gtsave(confessionalstattable
         ,filename = paste(directory,"topChef/S22E",currentep
                           ,"ConfessionalStats.png",sep=""))


###########################################################################
## Confessionals for those at judges table or not
  confsJTall <- confsByEpi %>%
    left_join(judgestable %>%
                # need to update the chefs' names who have accents in them
                mutate(chef = case_when(chef == "Kevin D'Andrea" ~ "Kévin D'Andrea"
                                        ,chef == "Cesar Murillo" ~ "César Murillo"
                                        ,chef == "Begona Rodrigo" ~ "Begoña Rodrigo"
                                        ,TRUE ~ chef))
              ) %>%
    # in competition only
    filter(inCompetition == TRUE)

  confsJT <- confsJTall %>%
    ungroup() %>%
    group_by(season,seasonNumber,series,episode,totalconfsinep,atJT) %>%
    summarise(avg = round(mean(count,na.rm=T),1)) %>%
    pivot_wider(names_from=atJT,values_from=avg) %>%
    mutate(`Who has a higher average number of confessionals` =
             case_when(yes > no ~ "Those at JT"
                       ,yes == no ~ "Equal"
                       ,yes < no ~ "Those not at JT")) %>%
    ungroup() %>%
    rename(`Not at judges' table`=no,`At judges' table`=yes)

  confsatJTtable <- confsJT %>%
    filter(seasonNumber == 22) %>%
    gt() %>%
    cols_hide(columns=c(season,seasonNumber,series,totalconfsinep
                      ,`Who has a higher average number of confessionals`)) %>%
    tab_source_note(source_note = "Created by Carly Levitz for Pack Your Knives") %>%
    tab_options(data_row.padding = px(1),
                column_labels.padding = px(1),
                row_group.padding = px(1))  %>%
    tab_style(style = cell_text(align = "right"),locations = cells_source_notes()) %>%
    tab_style(style = cell_text(align = "left",weight="bold")
              ,locations = cells_title(groups="title")) %>%
    tab_style(style = cell_text(align = "center",weight="bold")
              ,locations = cells_row_groups() ) %>%
    tab_style(style = cell_text(align = "left")
              ,locations = cells_title(groups="subtitle")) %>%
    tab_style(style = cell_text(align = "center")
              ,locations = cells_body()) %>%
    tab_style(style = cell_text(align = "center",weight="bold")
              ,locations = cells_column_labels()) %>%
    tab_style(style = cell_text(align = "center",weight="bold")
              ,locations = cells_column_labels()) %>%
    tab_style(style = cell_text(align = "center",weight="bold")
              ,locations = cells_column_spanners()) %>%
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
    cols_width(#chef ~ px(160)
               #,`# of times with first confessional of episode` ~ px(160)
               #,
               everything() ~ px(175) )  %>%
    tab_header(
      title = "Top Chef Destination Canada: Average confessional counts by whether they were at judges' table for the elimination challenge"
      ,subtitle = "Being at judges' table means you were there because you were at the top or the bottom. In Episode 8, all chefs were either at the top or the the bottom."
    ) %>%
    data_color(method="numeric",
               columns=!episode,
               palette=c("#a3cce9","#5fa2ce"
                         ,"#1170AA","#141B41"
               ),
               domain=c(3.4,9))


  gtsave(confsatJTtable
         ,filename = paste(directory,"topChef/S22_ConfsAtJT.png",sep=""))


########################################################################
## What's related to winning the season?
  # confplacement <- confsByEpi %>%
  #   left_join(placement %>%
  #               mutate(placement = as.numeric(placement))) %>%
  #   # Drop the episodes with three or fewer chefs
  #   filter(chefsinepisode > 3) %>%
  #   # Create a variable for if they are the winner of the season
  #   mutate(seasonWinner = ifelse(placement == 1,1,0))

  confplacement <- confs %>%
    left_join(placement %>%
                mutate(placement = as.numeric(placement))) %>%
    mutate(seasonWinner = ifelse(placement == 1, 1, 0))

  ## Logistic regression on season winner
  reg <- glm(confplacement$seasonWinner ~ confplacement$firstconfs +
        confplacement$difffromexpected + confplacement$phonecallsorphotos)






