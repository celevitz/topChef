rm(list=ls())
library(tidyverse)
library(openxlsx)
library(topChef)
library(ggplot2)
library(devtools)
library(gt)
#devtools::install_github("celevitz/topChef")

directory <- "/Users/carlylevitz/Documents/Data/topChef/"

challengewins <- read.csv(paste0(directory,"Top Chef - Challenge wins.csv"))%>%
  filter(series == "US")

chefdetails <- read.csv(paste0(directory,"Top Chef - Chef details.csv"))  %>%
  filter(series == "US" )

# Current episode #
    currentep <- 14

    chefsinlck <- NA
    eliminatedchefs <- c("Corwin Hemming","Zubair Mohajir","Mimi Weissenborn"
                         ,"Anya El-Wattar","Kat Turner","Henry Lu"
                         ,"Paula Endara","Vincenzo Loseto","Katianna Hong"
                         ,"Lana Lagomarsini","Massimo Piedimonte"
                         ,"Cesar Murillo"
                         )

## Index
## Write it out here, because it calls on the Top Chef package and that's
    # not working right now


    weightedindex <- function(seriesname,seasonnumberofchoice,numberofelimchalls
                              ,numberofquickfires) {
      # 1. Set up the data
      placementdata <- chefdetails[,c("chef","series","season"
                                               ,"seasonNumber","placement")]
      placementdata <- placementdata[placementdata$series == seriesname &
                                       placementdata$seasonNumber == seasonnumberofchoice,]


      # 1a. Outcomes of the challenges
      challengewins <-
        challengewins[,names(challengewins)[!(names(challengewins) %in% "rating")] ]
      challengewins <- challengewins[challengewins$series == seriesname &
                                       challengewins$seasonNumber == seasonnumberofchoice,]

      # 1ai. combine types of challenges
      # because there could be both a SDQ & an elimination in an episode, and I
      # use episode as a row ID, I need to make sure that we keep those challenge
      # results separate from the other elimination challenges in that episode
      challengewins$episodeascharacter <- as.character(challengewins$episode)
      challengewins$episodeascharacter[nchar(challengewins$episodeascharacter)==1] <-
        paste0("0",challengewins$episodeascharacter[nchar(challengewins$episodeascharacter)==1])

      challengewins$challID <- paste0(challengewins$episodeascharacter,"a_",challengewins$challengeType)
      challengewins$challID[challengewins$challengeType == "Quickfire Elimination"] <-
        paste0(challengewins$episodeascharacter[challengewins$challengeType == "Quickfire Elimination"] ,"b_qe")
      challengewins$challID[challengewins$challengeType == "Sudden Death Quickfire"] <-
        paste0(challengewins$episodeascharacter[challengewins$challengeType == "Sudden Death Quickfire"] ,"b_sdq")
      challengewins$challID[challengewins$challengeType == "Quickfire"] <-
        paste0(challengewins$episodeascharacter[challengewins$challengeType == "Quickfire"] ,"a_qf")
      challengewins$challID[challengewins$challengeType == "Elimination"] <-
        paste0(challengewins$episodeascharacter[challengewins$challengeType == "Elimination"] ,"c_elim")

      challengewins$challengeType[challengewins$challengeType %in%
                                    c("Quickfire Elimination"
                                      ,"Sudden Death Quickfire")] <- "Elimination"

      # 1aii. Exclude the uncommon challenge types
      challengewins <- challengewins[!(challengewins$challengeType %in%
                                         c("Battle of the Sous Chefs"
                                           ,"Qualifying Challenge")),]

      # 1aiii. clean up outcomes: consolidate
      challengewins$outcome[challengewins$outcome %in% c("High","HiGH")] <-
        "HIGH"
      challengewins$outcome[grepl("LOW",challengewins$outcome)] <- "LOW"
      challengewins$outcome[challengewins$outcome %in%
                              c("DISQUALIFIED","RUNNER-UP","WITHDREW") |
                              grepl("OUT",challengewins$outcome) ] <- "OUT"
      challengewins$outcome[challengewins$outcome %in% c("DIDN'T COMPETE") |
                              grepl("N/A",challengewins$outcome) |
                              grepl("QUALIFIED",challengewins$outcome) ] <- "IN"
      challengewins$outcome[challengewins$outcome %in% c("WINNER")] <- "WIN"

      # 1b. need to consecutively number each challenge of each challenge type
      challnum_temp <- unique(challengewins[, c("season","seasonNumber"
                                                ,"challengeType","episode"
                                                ,"episodeascharacter","challID")])
      challnum_temp <- challnum_temp[order(challnum_temp$seasonNumber
                                           ,challnum_temp$episode
                                           ,challnum_temp$challID
                                           ,desc(challnum_temp$challengeType)),]

      # not all challenges that we're looking at start in episode 1
      # and, some episode 1s don't have a quickfire
      # order of eliminations
      elimorder <- unique(challnum_temp[challnum_temp$challengeType == "Elimination",c("challengeType","episode","challID")])
      row.names(elimorder) <- NULL
      elimorder$count <- row.names(elimorder)
      elimorder$challengeType <- "Elimination"
      elimorder$seasonNumber <- seasonnumberofchoice

      # order of quickfires
      qforder <- unique(challnum_temp[challnum_temp$challengeType == "Quickfire",c("challengeType","episode","challID")])
      row.names(qforder) <- NULL
      qforder$count <- row.names(qforder)
      qforder$challengeType <- "Quickfire"
      qforder$seasonNumber <- seasonnumberofchoice

      challnum <- rbind(elimorder,qforder)

      # if an elimination and quickfire don't happen in ep 1
      # need to add a row of quickfire
      # if the first episode of a quickfire is after the first episode of an elim,
      # then make the count 0 until it is the episode of the first quickfire
      firstelim_ep <- min(challnum$episode[challnum$challengeType == "Elimination"])
      firstqf_ep <- min(challnum$episode[challnum$challengeType == "Quickfire"])

      for (tempcount in seq(firstelim_ep, firstqf_ep,1)) {
        if (tempcount < firstqf_ep) {
          newrow <- data.frame(cbind("Quickfire",tempcount
                                     ,paste0("0",as.character(tempcount),"a_qf")
                                     ,0,seasonnumberofchoice
          ) )
          names(newrow) <- names(challnum)
          challnum <- rbind(challnum,newrow )
        }
      }

      # 1bi. keep just the challenges that meet the criteria
      # update: in case the numbers are vastly different (e.g., 10 elim challs & 0 Qfs)
      # need to instead of having: an OR statement about keeping things if the
      # count is less than or = to the number of elim challs select OR
      # count is less than or = to the # of qf challs
      # instead going to append the datasets together

      challnum$count <- as.numeric(challnum$count)

      challkeep <- rbind(challnum[challnum$count <= numberofelimchalls &
                                    challnum$challengeType == "Elimination" &
                                    !(is.na(challnum$count)),]
                         ,challnum[(challnum$count <= numberofquickfires &
                                      challnum$challengeType == "Quickfire" &
                                      !(is.na(challnum$count))) ,] )

      ## 2. get the number of wins, losses, highs, etc. by chef
      statsbynumberofchalls <- merge(challengewins,placementdata,by=c("series"
                                                                      ,"season","seasonNumber","chef"))
      statsbynumberofchalls <- merge(statsbynumberofchalls,challkeep,by=
                                       c("seasonNumber","challengeType"
                                         ,"episode","challID"))

      # 2a. keep just the episodes that are at or the same # of challenges
      # that have happened
      statsbynumberofchalls <- statsbynumberofchalls[!(is.na(
        statsbynumberofchalls$count)),]

      # 2b. drop when people were not in the competition
      statsbynumberofchalls <- statsbynumberofchalls[statsbynumberofchalls$
                                                       inCompetition == "TRUE",]

      # 2c. NEW PART OF CODE: flag just the chefs who were in the most recent
      # challenge you're looking at (whether it's elimination or QF)
      tempDataToFlagEliminatedPlayers <- challkeep %>%
        arrange(challID)
      challengeflag <- tempDataToFlagEliminatedPlayers[max(nrow(
        tempDataToFlagEliminatedPlayers))
        ,"challID"]
      statsbynumberofchalls$stillincomp <- 0
      statsbynumberofchalls$stillincomp[statsbynumberofchalls$challID ==
                                          challengeflag] <- 1
      statsbynumberofchalls <- statsbynumberofchalls %>%
        group_by(seasonNumber,series,season,chef) %>%
        mutate(stillincomp = max(stillincomp))

      # 2d. drop unnecessary variables
      statsbynumberofchalls <- statsbynumberofchalls[,c("season","seasonNumber"
                                                        ,"series","challengeType"
                                                        ,"chef","outcome"
                                                        ,"placement"
                                                        # NEW CODE
                                                        ,"stillincomp")]

      # 3. create summary stats by chef
      # get counts of wins, highs, and lows
      # don't combine outs & lows because we want to count those differently
      # in the index
      # for the currently airing season, they don't yet have a placement;
      # they'll get dropped in this aggregate function if they are empty;
      # fill them in for now
      statsbynumberofchalls$placement[is.na(statsbynumberofchalls$placement)] <-
        1.5
      statsbynumberofchalls$tempcount <- 1
      statsbynumberofchalls <- aggregate(statsbynumberofchalls$tempcount
                                         ,by=list(statsbynumberofchalls$chef
                                                  ,statsbynumberofchalls$season
                                                  ,statsbynumberofchalls$seasonNumber
                                                  ,statsbynumberofchalls$series
                                                  ,statsbynumberofchalls$challengeType
                                                  ,statsbynumberofchalls$outcome
                                                  ,statsbynumberofchalls$placement
                                                  # NEW CODE
                                                  ,statsbynumberofchalls$stillincomp)
                                         ,FUN=sum)
      names(statsbynumberofchalls) <- c("chef","season","seasonNumber","series"
                                        ,"challengeType","outcome","placement"
                                        # NEW CODE
                                        ,"stillincomp"
                                        ,"count")

      # 3a. reshape the data
      # we don't need the "IN"/safe counts because we are holding constant
      # the number of challenges done
      statsbynumberofchalls <-
        statsbynumberofchalls[statsbynumberofchalls$outcome != "IN",]
      statsbynumberofchalls <- reshape(statsbynumberofchalls,
                                       timevar = "challengeType",
                                       idvar = c("chef","season","seasonNumber"
                                                 ,"series","outcome"
                                                 ,"placement"
                                                 # NEW CODE
                                                 ,"stillincomp"),
                                       direction = "wide")
      statsbynumberofchalls <- reshape(statsbynumberofchalls,
                                       timevar = "outcome",
                                       idvar = c("chef","season","seasonNumber"
                                                 ,"series","placement"
                                                 # NEW CODE
                                                 ,"stillincomp"
                                       ),
                                       direction = "wide")
      names(statsbynumberofchalls) <- gsub("count.",""
                                           ,names(statsbynumberofchalls))
      statsbynumberofchalls$Quickfire.OUT <- NULL

      # 3b. if NA, replace with 0
      for (var in c(names(statsbynumberofchalls)[!(names(statsbynumberofchalls)
                                                   %in% c("chef","season","seasonNumber","series"
                                                          ,"placement"))])) {
        statsbynumberofchalls[is.na(statsbynumberofchalls[,var]),var] <- 0
      }

      # 3c. How many times are they at jduges table?
      statsbynumberofchalls$judgestableQF <-statsbynumberofchalls$Quickfire.WIN+
        statsbynumberofchalls$Quickfire.HIGH+statsbynumberofchalls$Quickfire.LOW
      statsbynumberofchalls$judgestableElim <-
        statsbynumberofchalls$Elimination.WIN+
        statsbynumberofchalls$Elimination.HIGH+
        statsbynumberofchalls$Elimination.LOW +
        statsbynumberofchalls$Elimination.OUT
      statsbynumberofchalls$judgestableElimPercent <-
        statsbynumberofchalls$judgestableElim/numberofelimchalls

      # 4. get the index
      statsbynumberofchalls$indexWeight <-
        statsbynumberofchalls$Elimination.WIN*7+
        statsbynumberofchalls$Elimination.HIGH*3 -
        statsbynumberofchalls$Elimination.LOW*3-
        statsbynumberofchalls$Elimination.OUT*7 +
        statsbynumberofchalls$Quickfire.WIN*4+
        statsbynumberofchalls$Quickfire.HIGH*2-
        statsbynumberofchalls$Quickfire.LOW*2

      # NEW CODE
      # Rank of all chefs
      # Rank of chefs in the competition at the time

        statsbynumberofchalls <- statsbynumberofchalls[order(
          statsbynumberofchalls$indexWeight
          ,statsbynumberofchalls$Elimination.WIN
          ,statsbynumberofchalls$Elimination.HIGH
          ,statsbynumberofchalls$Quickfire.WIN
          ,statsbynumberofchalls$Quickfire.HIGH
          ,decreasing=TRUE),]
        row.names(statsbynumberofchalls) <- NULL
        statsbynumberofchalls$OverallRank <- as.numeric(row.names(statsbynumberofchalls))

        statsbynumberofchalls <- statsbynumberofchalls[order(
          statsbynumberofchalls$stillincomp
          ,statsbynumberofchalls$indexWeight
          ,statsbynumberofchalls$Elimination.WIN
          ,statsbynumberofchalls$Elimination.HIGH
          ,statsbynumberofchalls$Quickfire.WIN
          ,statsbynumberofchalls$Quickfire.HIGH
          ,decreasing=TRUE),]
        row.names(statsbynumberofchalls) <- NULL
        statsbynumberofchalls$RankOfThoseStillIn <- as.numeric(row.names(statsbynumberofchalls))
        statsbynumberofchalls$RankOfThoseStillIn[statsbynumberofchalls$stillincomp == 0] <- NA


      statsbynumberofchalls
    }

#############################################################################
## Rank by episode
    s22allColumnsTemp <- weightedindex("US",22,1,1) %>% mutate(episode = 1) %>%
      bind_rows(weightedindex("US",22,2,2)  %>% mutate(episode = 2) ) %>%
      bind_rows(weightedindex("US",22,3,3)  %>% mutate(episode = 3)  ) %>%
      bind_rows(weightedindex("US",22,4,4)  %>% mutate(episode = 4)  ) %>%
      bind_rows(weightedindex("US",22,5,4)  %>% mutate(episode = 5)  ) %>%
      bind_rows(weightedindex("US",22,6,5)  %>% mutate(episode = 6)  ) %>%
      bind_rows(weightedindex("US",22,7,6)  %>% mutate(episode = 7)  ) %>%
      bind_rows(weightedindex("US",22,8,6)  %>% mutate(episode = 8)  ) %>%
      bind_rows(weightedindex("US",22,9,7)  %>% mutate(episode = 9)  ) %>%
      bind_rows(weightedindex("US",22,10,8)  %>% mutate(episode = 10)  ) %>%
      bind_rows(weightedindex("US",22,11,9)  %>% mutate(episode = 11)  ) %>%
      bind_rows(weightedindex("US",22,12,9)  %>% mutate(episode = 12)  ) %>%
      bind_rows(weightedindex("US",22,13,10)  %>% mutate(episode = 13)  ) %>%
      bind_rows(weightedindex("US",22,14,10)  %>% mutate(episode = 14)  )

    s22allColumns <- s22allColumnsTemp %>%
      select(!placement) %>%
      mutate(yvalue=15-OverallRank+1)

    s22 <- s22allColumns %>%
      select(chef,episode,indexWeight,OverallRank,yvalue) %>%
      mutate(cheflabel=ifelse(episode == max(episode),chef,NA))

  # Wide version for gt()
    s22wide <- s22allColumns %>%
      select(chef,episode,indexWeight) %>%
      mutate(episode = paste0("Ep. ",episode)
             ,inCompetition = case_when(chef %in% chefsinlck ~ 1
                                        ,chef %in% eliminatedchefs ~ 0
                                        ,TRUE ~ 2)) %>%
      pivot_wider(names_from = episode,values_from=indexWeight)

    currentrankforsorting <- s22 %>%
      filter(episode == currentep) %>%
      select(chef,OverallRank)

    s22wide <- s22wide %>%
      left_join(currentrankforsorting)

    s22wide <- s22wide[order(s22wide$OverallRank),]

#############################################################################
## Compare to all seasons
    allseasons <- s22allColumnsTemp %>%
      filter(episode == 14) %>% select(!episode)
    for (sn in 1:21) {
      allseasons <- allseasons %>%
        bind_rows(weightedindex("US",sn,14,10)  )
    }

   # Most Elimination wins
    allseasons %>%
      select(chef,season,seasonNumber,Elimination.WIN) %>%
      arrange(desc(Elimination.WIN)) %>%
     filter(Elimination.WIN >=3)

  # Most Quickfire wins
    allseasons %>%
      select(chef,season,seasonNumber,Quickfire.WIN) %>%
      arrange(desc(Quickfire.WIN)) %>%
      filter(Quickfire.WIN >=3)

  # Elimination, quickfire - at the top, including wins
  allseasons %>%
    mutate(top = Elimination.WIN + Elimination.HIGH) %>%
    select(chef,season,seasonNumber,top) %>%
    arrange(desc(top)) %>%
    filter(top >=7)

  allseasons %>%
    mutate(top = Quickfire.WIN + Quickfire.HIGH) %>%
    select(chef,season,seasonNumber,top) %>%
    arrange(desc(top)) %>%
    filter(top >=4)

  # Top three, most lows
  allseasons %>%
    filter(placement <=3 & !(is.na(placement))) %>%
    select(chef,season,seasonNumber,placement,Elimination.LOW) %>%
    arrange(desc(Elimination.LOW))



#############################################################################
## Visualizations
# Graph
rankgraph <- s22 %>%
  mutate(StillIn = ifelse(chef %in% eliminatedchefs,"Out","In")
         ,alphavalue = ifelse(chef %in% eliminatedchefs,.8,1)) %>%
  ggplot(aes(x=episode,y=yvalue)) +
  geom_line(aes(color=chef,linetype=StillIn,alpha=alphavalue)) +
  geom_point(aes(color=chef,alpha=alphavalue)) +
  scale_y_continuous(breaks=seq(1,15,1),labels=rev(seq(1,15,1))
                     ,limits=c(.5,15.5)
                     ,"Rank (lower values are better)") +
  scale_x_continuous(breaks=seq(1,max(s22$episode),1)
                     ,labels=paste0("Ep. ",seq(1,max(s22$episode),1))
                     ,limits=c(.9,max(s22$episode)+3.5)
                     ,"") +
  geom_text(aes(label=cheflabel,y=yvalue,x=episode+2,color=chef),size=3) +
  ggtitle("Rank of chefs' scores each episode of Top Chef Season 22"
          ,subtitle = "Created by Carly Levitz for Pack Your Knives"  ) +
  theme_minimal() +
  theme(legend.position="none"
        ,panel.grid = element_blank()
        ,plot.background = element_rect(color="white")
        ,axis.text.x=element_text(size=6))
ggsave(paste0(directory,"S22E",currentep,"Ranking.png")
       ,rankgraph,width = 6,height = 4,dpi = 1200 )

# Table
scoretable <- s22wide %>%
  gt() %>%
  cols_hide(columns=c(OverallRank,inCompetition)) %>%
  tab_source_note(source_note = "Created by Carly Levitz for Pack Your Knives") %>%
  tab_row_group(label = "Eliminated",rows = inCompetition==0) %>%
  tab_row_group(label = "In Last Chance Kitchen",rows = inCompetition==1) %>%
  tab_row_group(label = "In the competition",rows = inCompetition==2) %>%
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
  cols_width(chef ~ px(165), everything() ~ px(50) )  %>%
  tab_header(
    title = paste0("Top Chef Destination Canada Episode ",currentep)
    ,subtitle = "Scores by episode"
  ) %>%
  data_color(method="numeric",
             columns=!chef,
             palette=c("#c85200","#ffbc69", "#a3cce9","#5fa2ce"
                       ,"#1170AA","#141B41"),
             domain=c(min(s22$indexWeight),max(s22$indexWeight)))


gtsave(scoretable
       ,filename = paste(directory,"S22E",currentep,"Summary.png",sep=""))













