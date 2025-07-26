## When was each chef's first elimination low for an individual challenge?

rm(list=ls())
library(tidyverse)
library(openxlsx)
library(topChef)
library(ggplot2)
library(devtools)
library(gt)

directory <- "/Users/carlylevitz/Documents/Data/topChef/"
chefs <- read.csv(paste0(directory,"Top Chef - Chef details.csv")) %>%
  select(chef,name,placement,season,seasonNumber,series) %>%
  filter(series == "US")
challengedetails <- read.csv(paste0(directory
                                  ,"Top Chef - Challenge descriptions.csv"))%>%
  filter(series == "US")

challengewins <- read.csv(paste0(directory,"Top Chef - Challenge wins.csv"))%>%
  filter(series == "US" & inCompetition == "TRUE") %>%
  select(!c(inCompetition,immune,rating))

allchallenges <- challengewins %>%
  full_join(challengedetails %>%
              select(season,seasonNumber,series,episode,challengeType
                     ,outcomeType))

maxepisodebyseason <- allchallenges %>%
  group_by(series,season,seasonNumber) %>%
  summarise(lastepisode = max(episode)) %>%
  # change S22 to be the number of episodes that it will be, not where the data
  # curerntly say it is
  mutate(lastepisode = ifelse(seasonNumber == 22, 14,lastepisode))

## keep just the challenges of interest: elimination-type challs,
## Look at lows and outs, but consider them the same, since we're looking for
## the first individual challenge they were on the bottom
challenges <- allchallenges %>%
  filter(outcomeType == "Individual" &
           !(challengeType %in% c("Qualifying challenge","Qualifying Challenge"
                                  ,"Quickfire")) &
         outcome %in% c("LOW","OUT","RUNNER-UP","DISQUALIFIED")
         ) %>%
  # get the episode # by chef
  ungroup() %>% group_by(season,seasonNumber,series,chef) %>%
  summarise(firstepisodeLow = min(episode)) %>%
  # since maaaaaybe no chefs were at the bottom ever, just to be sure
  # that all chefs are included, add back on the chefs & their placement
  full_join(chefs) %>%
  # make the placement a number
  # if it becomes NA that means that they didn't make it out of LCK or
  # into the main comp from a qualifying challenge
  mutate(placement = as.numeric(placement)) %>%
  # how does this compare to the number of episodes in the season?
  full_join(maxepisodebyseason) %>%
  mutate(proportionintoseason = firstepisodeLow/lastepisode
         ,seasonNumberAsString = ifelse(seasonNumber < 10
                                        ,paste0("0",as.character(seasonNumber))
                                        ,as.character(seasonNumber))
         ,category = ifelse(is.na(proportionintoseason),"Never had an individual elimination challenge low","Had an individual elimination challenge low"))

# Stats
  summary(challenges$proportionintoseason)

# Regression
  reg <- lm(challenges$placement ~ challenges$proportionintoseason +
              challenges$season)
  summary(reg)

# Plot
  # This won't show people who were eliminated on team challenges prior to
  # having an individual challenge
  # There are five winners who were never low on an individual challenge:
  #   Harold, Michael, Richard (S8), Paul, and Kristen
  # It's only been 2nd placers whose first individual low was in the last
  # episode of the season (the finale, where they were runner-up)
  #   Richard (S4), Evelyn, Bryan (S6), Kevin (S6), Shota
  challenges %>%
    ggplot(aes(x=seasonNumber,y=proportionintoseason)) +
    geom_jitter(aes(color=placement))

  challenges %>%
    filter(placement <= 3 #| seasonNumber == 22
           )%>%
    ggplot(aes(x=seasonNumber,y=proportionintoseason)) +
    geom_text(aes(label=chef,color=placement),size=2)

  detailedgraphbyplacement <- challenges %>%
    ggplot(aes(x=placement,y=proportionintoseason)) +
    geom_jitter(alpha=.1,aes(color=seasonNumberAsString)) +
    geom_smooth(method='lm', formula= y~x) +
    geom_text(aes(label=chef),size=.5)

  ggsave(paste0(directory,"Placement-FirstElimLow.png")
         ,detailedgraphbyplacement,width = 6,height = 4,dpi = 1200 )

## Winners: table
  winnerstable <-
  challenges %>%
    ungroup() %>%
    filter(placement == 1) %>%
    relocate(name,.before=season) %>%
    mutate(proportionintoseason=round(proportionintoseason,3)) %>%
    rename(`First episode in which they had an IEL` = firstepisodeLow
           ,`Proportion into season in which they first had an IEL` = proportionintoseason
           ,`Season #`=seasonNumber) %>%
    arrange(category,desc(`Proportion into season in which they first had an IEL`)) %>%
    gt() %>%
    cols_hide(columns=c(chef,seasonNumberAsString,series,placement,lastepisode
                        ,category)) %>%
    tab_source_note(source_note = "Created by Carly Levitz for Pack Your Knives") %>%
    tab_row_group(label = "Had an individual elimination challenge low"
                  ,rows = category == "Had an individual elimination challenge low") %>%
    tab_row_group(label = "Never had an individual elimination challenge low"
                  ,rows = category == "Never had an individual elimination challenge low") %>%
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
              ,locations = cells_body(columns=!c(name,season))) %>%
    tab_style(style = cell_text(align = "center",weight="bold")
              ,locations = cells_column_labels(columns=!c(name,season))) %>%
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
    cols_width(name ~ px(165)
             ,`Proportion into season in which they first had an IEL` ~ px(170)
             ,season ~ px(155)
             , everything() ~ px(115) )  %>%
    tab_header(
      title = "Top Chef Winners: How long into their season did they have an individual elimination challenge low (IEL)?"
      ,subtitle = "Low means they were on the bottom of the individual elimination challenge, or they were eliminated. Five winners never had an IEL, though they all were on the bottom of team challenges once (Harold was on the bottom twice). Kristen was the only winner who was eliminated, but it was in a team elimination challenge."
    ) %>%
    data_color(method="numeric",
               columns=`Proportion into season in which they first had an IEL`,
               palette=c("#c85200","#ffbc69", "#a3cce9","#5fa2ce"
                         ,"#1170AA","#141B41"),
               domain=c(0,1))

  gtsave(winnerstable
         ,filename = paste(directory,"WinnersFirstIndivElimChallLow.png",sep=""))

# Plot Season 22
  s22 <- challenges %>%
    ungroup() %>%
    filter(seasonNumber == 22 & !(name %in% c("Sam Olayinka","Ying Gao"))) %>%
    relocate(name,.before=season) %>%
    select(!c(season,seasonNumber,seasonNumberAsString,series,name,lastepisode)) %>%
    mutate(proportionintoseason=round(proportionintoseason,3)) %>%
    arrange(desc(proportionintoseason)) %>%
    mutate(y=case_when(chef %in% c("Vincenzo Loseto","Henry Lu","Paula Endara"
                                   ,"Mimi Weissenborn") ~ -1
                      ,chef %in% c("Zubair Mohajir","Anya El-Wattar") ~ -2
                      ,chef %in% "Bailey Sullivan" ~ 2
                      ,chef %in% "Katianna Hong" ~ 1.5
                      ,chef %in% "Corwin Hemming" ~ 1
                      ,TRUE ~ 1 )
           ,x=ifelse(is.na(proportionintoseason),1,proportionintoseason)
           ,yForLabel=case_when(category %in% "Never had an individual elimination challenge low" ~ y
                                ,y>0 ~ y+.3
                                ,y<0 ~ y-.3)
           ) %>%
    separate_wider_delim(chef, delim = " ", names = c("chef", "lastname"))


  s22propintoseason <- s22 %>%
    ggplot(aes(x=x,y=y,label=chef)) +
    geom_rect(aes(ymin=0,ymax=y,xmin=x-.000000001,xmax=x+.000000001
                  ,color=category),linewidth=1) +
    geom_label(aes(x=x,y=y,color=category),cex=2.5) +
    geom_segment(aes(x=0,xend=.8,y=0,yend=0)) +
    # axis ticks
    annotate(geom="text",label="Episode 1",x=.071,y=1.3,cex=2.3) +
    annotate(geom="text",label="Episode 3",x=.214,y=1.3,cex=2.3) +
    annotate(geom="text",label="Episode 4",x=.286,y=1.3,cex=2.3) +
    annotate(geom="text",label="Episode 7",x=.5,y=1.3,cex=2.3) +
    annotate(geom="text",label="Episode 9",x=.643,y=1.3,cex=2.3) +
    annotate(geom="text",label="Episode 10",x=.714,y=1.3,cex=2.3) +
    # title and such
    annotate(geom="text",label="Each chef's first episode with an individual elimination challenge low"
              ,x=0,y=2.5,hjust="left",color="black",cex=5) +
    # annotate(geom="text",label="This is assuming a 14-episode season"
    #          ,x=0,y=2,hjust="left",color="black",cex=3) +
    annotate(geom="text",label="Never:\nBailey\nKatianna\nCorwin"
             ,x=.85,y=1.5,hjust=.5,color="black",cex=3) +
    annotate(geom="text",label="Created by Carly Levitz for Pack Your Knives"
             ,x=.9,y=-1.8,hjust=1,color="black",cex=2) +
    scale_color_manual(values=c("#1170AA","white")) +
    scale_y_continuous(lim=c(-2.5,3.5)) +
    theme(legend.position = "none"
          ,panel.grid = element_blank()
          ,plot.background = element_rect(color="white",fill="white")
          ,panel.background = element_rect(color="white",fill="white")
          ,axis.title=element_blank()
          ,axis.ticks = element_blank()
          ,axis.text = element_blank())


  ggsave(paste0(directory,"S22_ILE.png")
         ,s22propintoseason,width = 7,height = 4,dpi = 1200 )

