## Carly Levitz
## Date written: 2023-04-10
## Date updated: 2023-05-20
## Purpose: Analyze Top Chef data for one season through the most recent episode


#####
## Set up
rm(list=ls())
devtools::install_github("celevitz/topChef")
library(tidyverse); library(openxlsx); library(gt); library(topChef)

directory <- "/Users/carlylevitz/Documents/Data/"
savedirectory <- "/Users/carlylevitz/Documents/Data/TCSeason20/Episode-specific/"

chefdetails <- topChef::chefdetails
challengewins <- topChef::challengewins

### Set up key information manually
  # season number
    currentseason <- 20
  # most recent episode
    airedepisode <- 11
  # Episode description
    episodedescription <- "In the Quickfire, the chefs had to share one pot of water and feature
      a steamed element. Andrew Wong guest-judged. Buddha won. The elimination challenge was a
      team challenge: Ali & Tom, Sara & Amar, and Buddha & Gabri. Each team was responsible for
      making three Wellingtons. Buddha and Gabri won - meaning Buddha 'won' the episode and Gabri
      won his first elimination challenge! Amar & Sara lost and went to compete in Last Chance Kitchen."
  ## Manually list the chefs who are still in Last Chance Kitchen (LCK) at this point
  ## These are the chefs that have been eliminated. I am not sharing which ones have also
  ## now been eliminated in LCK
    stillinlck <- c("Sylwia Stachyra","Dale MacKay","Charbel Hayek","Nicole Gomes","Victoire Gouloubi","Sara B.","Amar S.")

# Copy the episode description with clean line breaks here; this will become ALT text on twitter
#Top Chef World All Stars: Challenge statistics. Heat map of the contestants of Season 20 of Top Chef & the number of times they have won or been in the top or bottom of both quickfire & elimination challenges. Being in the bottom of an elimination challenge includes when the person was eliminated. Organized by in competition, in LCK, & out.
#In the Quickfire, the chefs had to share one pot of water and feature a steamed element. Andrew Wong guest-judged. Buddha won. The elimination challenge was a team challenge: Ali & Tom, Sara & Amar, and Buddha & Gabri. Each team was responsible for making three Wellingtons. Buddha and Gabri won - meaning Buddha 'won' the episode and Gabri won his first elimination challenge! Amar & Sara lost and went to compete in Last Chance Kitchen.
    #Remaining chefs: Buddha (3 elimination wins, 3 quickfire wins), Ali (2 elimination wins, 1 quickfire win), Gabri (1 elimination win, 0 quickfire wins), and Tom (1 elimination win and 1 quickfire win)
#Data from: github.com/celevitz/topChef

##############################################################################
## Set up data
##############################################################################
currentstats <-
  ## Who is still in the competition?
  chefdetails %>% filter(sznnumber == currentseason) %>%
  mutate(in.competition=ifelse(is.na(placement),2,0)
         # call out LCK people
         ,in.competition=ifelse(chef %in% stillinlck,1,in.competition)
  ) %>%
  select(chef,in.competition,placement) %>%
  ## merge on the challenge wins
  left_join(challengewins %>%
              filter(sznnumber == currentseason) %>%
              select(!c(in.competition,rating,episode))) %>%
  ## get counts of wins, highs, and lows
  ## and, if they were out, include that as being on the bottom
  mutate(outcome=ifelse(outcome == "OUT","LOW",outcome)) %>%
  group_by(chef,challenge_type,outcome,in.competition) %>%
  mutate(tempcount=1,count=sum(tempcount)) %>%
  select(!c(tempcount,szn,sznnumber)) %>%
  distinct() %>%
  ## reshape the data
  mutate(outcome=ifelse(is.na(outcome),"IN",outcome)) %>%
  filter(outcome != "IN") %>%
  pivot_wider(names_from=challenge_type,values_from=count) %>%
  ungroup() %>%
  pivot_wider(names_from=outcome,values_from=c(Elimination,Quickfire)) %>%
  ## format things for gt()
  mutate(Elimination_WIN=ifelse(is.na(Elimination_WIN),0,Elimination_WIN),
         Elimination_HIGH=ifelse(is.na(Elimination_HIGH),0,Elimination_HIGH),
         Elimination_LOW=ifelse(is.na(Elimination_LOW),0,Elimination_LOW),
         Quickfire_WIN=ifelse(is.na(Quickfire_WIN),0,Quickfire_WIN),
         Quickfire_HIGH=ifelse(is.na(Quickfire_HIGH),0,Quickfire_HIGH),
         Quickfire_LOW=ifelse(is.na(Quickfire_LOW),0,Quickfire_LOW)) %>%
  arrange(desc(in.competition),placement,desc(Elimination_WIN),desc(Elimination_HIGH)
          ,desc(Quickfire_WIN),desc(Quickfire_HIGH),Quickfire_LOW) %>%
  relocate(Elimination_WIN,Elimination_HIGH,Elimination_LOW
           ,Quickfire_WIN,Quickfire_HIGH,Quickfire_LOW
           ,.after = last_col()) %>%
  rename(`Elimination_LOW*`=Elimination_LOW) %>%
  mutate(placement=ifelse(is.na(placement)," ",placement))

##############################################################################
## Visualize the results
##############################################################################
viztable <-
  currentstats %>%
  rename(Chef = chef) %>%
  gt() %>%
  tab_spanner_delim(delim = "_") %>%
  cols_hide(columns=c(in.competition,series)) %>%
  tab_source_note(source_note = "*Data notes: LOW also includes when the chefs were eliminated (e.g., Dale). /// Data: github.com/celevitz/topChef /// Twitter@carlylevitz") %>%
  tab_row_group(label = "Eliminated",rows = in.competition==0) %>%
  tab_row_group(label = "In Last Chance Kitchen",rows = in.competition==1) %>%
  tab_row_group(label = "In the competition",rows = in.competition==2) %>%
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
            ,locations = cells_body(columns=!Chef)) %>%
  tab_style(style = cell_text(align = "center",weight="bold")
            ,locations = cells_column_labels(columns=!Chef)) %>%
  tab_style(style = cell_text(align = "left",weight="bold")
            ,locations = cells_column_labels(columns=Chef)) %>%
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
  cols_width(Chef ~ px(250), placement ~ px(100), everything() ~ px(50) )  %>%
  tab_header(
    title = paste0("Top Chef World All Stars Episode ",airedepisode)
    ,subtitle = episodedescription
  ) %>%
  data_color(method="numeric",
             columns=c(Quickfire_WIN,Elimination_WIN,Quickfire_HIGH,Elimination_HIGH),
             palette=c("transparent", "#a3cce9","#5fa2ce","#1170AA","#141B41"),
             domain=c(0,4)) %>%
  data_color(method="numeric",
             columns=c(Quickfire_LOW,`Elimination_LOW*`),
             palette=c("transparent","#ffbc69","#fc7d0b","#c85200","#c85200","#984447","#984447"),
             domain=c(0,6))

gtsave(viztable,filename = paste(savedirectory,"S20E",airedepisode,"Summary.png",sep=""))


