## Last Chance Kitchen
## There are three chefs who started in LCK and came back into the main comp:
##    Lee Anne (S15), Brother (S16), and Soo (S21).
## In S20, the "quickfire" of Dale and Begona in ep 6 is consolidated to be
##  part of LCK, instead of the main competition. I consider it to be one
##  challenge
## George P. (S12) not considered a LCK return because he was voted in, and
##    didn't win a challenge to re-enter the main competition
## I struggled with things when they were split across two episodes but aired
##    on the same day.
##    Charleston LCK finale could probably have been condensed to be just one
##      challenge, with Jamie and Casey being eliminated, instead of two
##      challenges, with Brooke and Casey winning the first one.
##    Ditto re: Colorado
##    Ditto re All Stars LA mid-season -- eps 5 and 6
##    Ditto re S19 eps 4 and 5

rm(list=ls())
library(tidyverse)
library(gt)

directory <- "/Users/carlylevitz/Documents/Data/topChef/"

# Bring in data
chefs <- read.csv(paste0(directory,"Top Chef - Chef details.csv"))
challengedescriptions <- read.csv(paste0(directory
                                     ,"Top Chef - Challenge descriptions.csv"))
challengewins <- read.csv(paste0(directory,"Top Chef - Challenge wins.csv"))

############################################################################
## Using the new LCK data
  lckchallenges <- challengewins %>%
    filter(series == "US LCK") %>%
    # for merging, need to change the series to be US, instead of LCK
    mutate(series = "US") %>%
    left_join(chefs %>% select(chef,series,season,seasonNumber,placement
                               ,gender,personOfColor)) %>%
    # which ones made it back into the main series?
    ungroup() %>% group_by(season,seasonNumber,series,chef) %>%
    mutate(cameback=ifelse(outcome %in% "WINNER","yes","no")) %>%
    # gender makeup
    group_by(season,seasonNumber,series,episode) %>%
    mutate(genderNumber = ifelse(gender == "Male",1,2)
           ,gendermakeupNumeric = mean(genderNumber,na.rm=T)
           ,gendermakeup = case_when(gendermakeupNumeric == 1 ~ "All men"
                                     ,gendermakeupNumeric == 2 ~ "All women"
                                     ,TRUE ~ "Mixed")
           ,winnerGender = case_when(genderNumber == 1 &
                                       outcome == "WIN" ~ 1
                                     ,genderNumber == 2 &
                                       outcome == "WIN" ~ 2
                                     ,TRUE ~ NA)
           ,winnerGender = case_when(gendermakeup == "All men" ~
                                                              "All male winners"
                             ,gendermakeup == "All women" ~ "All female winners"
                                    ,mean(winnerGender,na.rm=T) ==1 ~
                                                              "All male winners"
                                   ,mean(winnerGender,na.rm=T) ==2 ~
                                                            "All female winners"
                                   ,TRUE ~ "Mixed winner"    )   )

## How many people won LCK?
## Doesn't include George P. in Boston
  returningchefs <- lckchallenges %>%
    filter(cameback == "yes") %>%
    ungroup () %>%
    select(season,seasonNumber,series,chef) %>%
    distinct() %>%
    arrange(seasonNumber, chef) %>%
    mutate(cameback = "yes")

  # Stats for each season
  seasonstats <-
      # How many episodes of LCK?
      lckchallenges %>%
        ungroup() %>%
        select(season,seasonNumber,series,episode) %>%
        distinct() %>%
        group_by(season,seasonNumber,series) %>%
        summarise(numberEpisodesOfLCK = n()) %>%
      left_join(
        # How many chefs competed?
        lckchallenges %>%
          ungroup() %>%
          select(season,seasonNumber,series,chef) %>%
          distinct() %>%
          group_by(season,seasonNumber,series) %>%
          summarise(chefsThatCompeted = n())
      ) %>%
      left_join(
        # number of episodes in which someone came back
        lckchallenges %>%
          filter(cameback == "yes") %>%
          ungroup () %>%
          select(season,seasonNumber,series,episode) %>%
          distinct() %>%
          group_by(season,seasonNumber,series) %>%
          summarise(numberEpisodesSomeoneReturned = n())
      ) %>%
      left_join(
        # How many chefs returned to main comp
        returningchefs %>%
          group_by(season,seasonNumber,series) %>%
          summarise(nChefsThatReEntered=n())
      ) %>%
    arrange(seasonNumber)


## How many challenges did each chef win?
  challengeswon <-   lckchallenges %>%
    filter(inCompetition == TRUE & outcome %in% c("WIN","WINNER")) %>%
    ungroup() %>%
    group_by(season,seasonNumber,series,chef) %>%
    summarise(nWon=n())

## How many challenges did they compete in?
  challengescompetedin <- lckchallenges %>%
    filter(inCompetition == TRUE ) %>%
    ungroup() %>%
    group_by(season,seasonNumber,series,chef) %>%
    summarise(nCompetedIn=n())

## How many episodes of the main competition did they miss?
  missedepisodesTemp <- returningchefs %>%
    left_join(challengewins) %>%
    # if they were in an episode at all, make it so they are counted for that episode
    group_by(season,seasonNumber,chef,episode) %>%
    summarise(temp_inEpisode = max(ifelse(inCompetition == TRUE,1,0),na.rm=T)
           ,inCompetition = ifelse(temp_inEpisode == 1, TRUE, FALSE)
           # in which episode are they out?
           ,out = max(case_when(outcome == "OUT" ~ 1
                                ,TRUE ~ 0 ))
           # flag in competition/out
         ,tempvalue = paste0(as.character(inCompetition),as.character(out)))

  missedepisodes <-
      ## First episode in
      missedepisodesTemp %>%
        filter(tempvalue == "TRUE0") %>%
        ## for each chef
        ungroup() %>% group_by(season,seasonNumber,chef) %>%
        summarise(firstep = min(episode)) %>%
    full_join(
      ## episode out
      missedepisodesTemp %>%
        filter(tempvalue == "TRUE1") %>%
        ## for each chef
        ungroup() %>% group_by(season,seasonNumber,chef) %>%
        summarise(epout = min(episode))
    ) %>%
    full_join(
      missedepisodesTemp
    ) %>%
    ## episode back in
      ungroup() %>% group_by(season,seasonNumber,chef) %>%
      mutate(epbackin = min(ifelse(epout<episode & tempvalue == "TRUE0"
                               ,episode,NA),na.rm=T)
             ,epbackin = ifelse(chef == "Louis M.",16,epbackin)
        # final episode in
             ,finalepin = min(ifelse(epbackin<episode & tempvalue == "TRUE1"
                                     ,episode,NA),na.rm=T)
        ) %>%
    ## final episode of season
    ungroup() %>% group_by(season,seasonNumber) %>%
    mutate(finalepisodeofseason = max(episode)) %>%
    select(season,seasonNumber,chef,firstep,epout,epbackin
           ,finalepin,finalepisodeofseason) %>%
    distinct() %>%
    ## fix a few things
    mutate(firstep = ifelse(chef == "Brother L.",6,firstep)
           ,epout = ifelse(chef == "Lee Anne W.",5,epout)
           ,epsMissed = epbackin-epout-1
           ,epbackin = ifelse(chef == "Lee Anne W.",5,epbackin)
           ,epbackin = ifelse(chef == "Brother L.",6,epbackin)
           ,epbackin = ifelse(chef == "Soo Ahn",6,epbackin)
           ,epsMissed = ifelse(chef == "Lee Anne W.",5,epsMissed)
           ,epsMissed = ifelse(chef == "Brother L.",6,epsMissed)
           ,epsMissed = ifelse(chef == "Soo Ahn",6,epsMissed)
           # winners and finalists
           ,finalepin = ifelse(chef %in% c("Brooke W.","Kristen K.","Joe F."
                                     ,"Amar S.","Sarah W.","Louis M.","Sara B.")
                               ,finalepisodeofseason,finalepin)
           ,finalepin = ifelse(chef %in% c("Lee Anne W.","Brother L.","Soo Ahn")
                               ,epout,finalepin)
           # Length of run post-LCK
           ,lengthofrunpostlck = finalepin-epbackin+1
           ,lengthofseasonpostlck = finalepisodeofseason-epbackin+1
           ,proportionofpostlckseasonin = round(lengthofrunpostlck/lengthofseasonpostlck,3)
           ) %>%
    # add on placement
    left_join(chefs %>% select(chef,series,season,seasonNumber,placement,name)) %>%
    # category of when they came back in
    mutate(whenbackin = ifelse(epbackin <10,"Mid-season","End of season")
           ,chef=name) %>%
    select(!name)

  winnerepisodeinfo <- missedepisodes %>%
    select(season,seasonNumber,series,whenbackin,chef,epsMissed
           ,lengthofrunpostlck,proportionofpostlckseasonin,whenbackin) %>%
    arrange(whenbackin,desc(proportionofpostlckseasonin)
            ,desc(lengthofrunpostlck),seasonNumber,chef) %>%
    mutate(lengthofrunpostlck = ifelse(is.infinite(lengthofrunpostlck)
                                       ,NA,lengthofrunpostlck)
           ,proportionofpostlckseasonin = ifelse(is.infinite(proportionofpostlckseasonin)
                                           ,NA,proportionofpostlckseasonin)) %>%
    relocate(whenbackin ,.after=last_col())

## Combine the data
  alldata <- challengeswon %>%
    full_join(challengescompetedin) %>%
    full_join(missedepisodes) %>%
    full_join(returningchefs ) %>%
    mutate(cameback = ifelse(is.na(cameback),"no",cameback)
           ,percentwon = nWon/nCompetedIn)

  winners <- missedepisodes %>%
    left_join(challengeswon) %>%
    left_join(challengescompetedin) %>%
    left_join(chefs %>% select(season,seasonNumber,chef,name)) %>%
    mutate(chef = name
           ,proportionwon = nWon/nCompetedIn
           ,placement = as.numeric(placement)) %>%
    select(!name)

  winners <- winners[,c("season","seasonNumber","series","chef"
                        ,"nCompetedIn","nWon","proportionwon"
                        ,"whenbackin","placement")]
  winners$proportionwon <- round(winners$proportionwon,3)


## Analysis
  ## By whether they came back
  alldata %>%
    group_by(cameback) %>%
    summarise(percentwon = mean(percentwon,na.rm=T)
              ,nWon = mean(nWon,na.rm=T)
              ,nCompetedIn = mean(nCompetedIn,na.rm=T)
              ,n=n())

  t.test(alldata$nCompetedIn[alldata$cameback == "no"]
         ,alldata$nCompetedIn[alldata$cameback == "yes"])
  t.test(alldata$nWon[alldata$cameback == "no"]
         ,alldata$nWon[alldata$cameback == "yes"])
  t.test(alldata$percentwon[alldata$cameback == "no"]
         ,alldata$percentwon[alldata$cameback == "yes"])

  ## Number of chefs who won different #s of LCK challenges
  nWonDistributionData <- alldata %>%
    group_by(nWon,cameback) %>%
    summarise(n=n()) %>%
    pivot_wider(names_from=cameback,values_from=n) %>%
    mutate(nWon = ifelse(is.na(nWon),0,nWon)
           ,no = ifelse(is.na(no),0,no)
           ,yes = ifelse(is.na(yes),0,yes)) %>%
    arrange(nWon) %>%
    # reshape it back to long b/c it's easier for ggplot
    pivot_longer(!nWon,names_to = "cameback",values_to = "nChefs") %>%
    mutate(cameback = ifelse(cameback == "no","Lost LCK","Returned to main competition"))

  ## Episodes missed of those who came back to main comp.
  alldata %>%
    group_by(epsMissed) %>%
    summarise(n=n())

  ## Number competed in
  alldata %>%
    group_by(cameback,nCompetedIn) %>%
    summarise(n=n()) %>%
    pivot_wider(names_from=cameback,values_from=n)

  ## Placement depending on when they were back in
  missedepisodes %>%
    ungroup() %>%
    mutate(placement = as.numeric(placement)) %>%
    group_by(whenbackin) %>%
    summarise(avgplacement = mean(placement,na.rm=T))


###########################################################################
# Graphics/tables
## Season stats
  seasonstatsTable <- seasonstats %>% ungroup() %>%
    rename(`Season #`=seasonNumber
           ,`# of LCK episodes`=numberEpisodesOfLCK
           ,`# of chefs who competed in LCK` = chefsThatCompeted
           ,`# of episodes in which someone returned` = numberEpisodesSomeoneReturned
           ,`# of chefs that returned to main competition` = nChefsThatReEntered) %>%
    gt() %>%
    cols_hide(columns=c(series)) %>%
    tab_source_note(source_note = "Created by Carly Levitz for Pack Your Knives") %>%
    tab_source_note(source_note = "There are three chefs who started in LCK and came back into the main competition: Lee Anne (S15), Brother (S16), and Soo (S21).\nIn S20, the quickfire of Dale and Begona in episode 6 is consolidated to be part of LCK, instead of the main competition. I consider it to be one challenge.\nGeorge P. (S12) not considered a LCK return because he was voted in, and did not win a challenge to re-enter the main competition.\nI struggled with things when they were split across two episodes but aired on the same day. I decided to categorize these as separate challenges:\nSeason 14 Charleston LCK finale, Season 15 Colorado LCK finale, Season 17 All Stars LA mid-season finale, and Season 19 Houston mid-season finale.") %>%
    tab_options(data_row.padding = px(1),
                column_labels.padding = px(1),
                row_group.padding = px(1))  %>%
    tab_style(style = cell_text(align = "left")
              ,locations = cells_source_notes()) %>%
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
    cols_width(season ~ px(160)
               ,`Season #` ~ px(90)
               ,`# of LCK episodes` ~ px(90)
               ,`# of chefs who competed in LCK` ~ px(120)
               , everything() ~ px(150) )  %>%
    tab_header(
      title = paste0("Top Chef Last Chance Kitchen: statistics by season")
      ,subtitle = "LCK = Last Chance Kitchen"
    )

  gtsave(seasonstatsTable
         ,filename = paste(directory,"LCKSeasonStats.png",sep=""))

## Number of challenges won
  numChallengesWonGraph <- nWonDistributionData %>%
    ggplot(aes(x=nWon,y=nChefs)) +
    geom_bar(stat="identity") +
    facet_wrap(~cameback) +
    geom_text(aes(label=nChefs,y=nChefs+4),cex=2.3)  +
    scale_y_continuous(breaks=seq(0,110,15),labels=seq(0,110,15)
                       ,limits=c(0,112)
                       ,"Number of chefs") +
    scale_x_continuous(breaks=seq(0,8,1)
                       #,labels=paste0("Ep. ",seq(1,max(s22$episode),1))
                       #,limits=c(.9,max(s22$episode)+3)
                       ,"Number of LCK challenges won") +
    ggtitle("Number of chefs who won different numbers of LCK challenges"
            ,subtitle = "Created by Carly Levitz for Pack Your Knives"
    ) +
    theme_minimal() +
    theme(panel.grid = element_blank()
          ,plot.background = element_rect(color="white")
          ,strip.background =element_rect(fill="#a3cce9")
          ,strip.text = element_text(colour = 'black')
          ,plot.subtitle = element_text(size=5)
    )
  ggsave(paste0(directory,"LCK_ChallengesWon_ByWhetherCameBack.png")
         ,numChallengesWonGraph,width = 6,height = 4,dpi = 1200 )

## Winners of LCK
  winnersTable <-
    winners %>% ungroup() %>%
    arrange(whenbackin,desc(nCompetedIn),desc(proportionwon),placement) %>%
    rename(`Season #` = seasonNumber
           ,`# LCK challenges competed in` = nCompetedIn
           ,`# LCK challenges won` = nWon
           ,`Proportion of LCK challenges won`=proportionwon
           ,`Final placement` = placement
           #,`Category of return to main competition` = whenbackin
           ) %>%
    gt() %>%
    cols_hide(columns=c(series,whenbackin)) %>%
    tab_source_note(source_note = "Created by Carly Levitz for Pack Your Knives") %>%
    tab_source_note(source_note = "There are three chefs who started in LCK and came back into the main competition: Lee Anne (S15), Brother (S16), and Soo (S21). Lee Anne and Brother were each only ever in one episode of the main competition. In LCK challenges of more than two chefs, chefs could be considered safe but not win.") %>%

    tab_row_group(label = "Returned to main competition episode 10 or later",rows = whenbackin=="End of season") %>%
    tab_row_group(label = "Returned to main competition before episode 10",rows = whenbackin=="Mid-season") %>%
    tab_options(data_row.padding = px(1),
                column_labels.padding = px(1),
                row_group.padding = px(1))  %>%
    tab_style(style = cell_text(align = "left")
              ,locations = cells_source_notes()) %>%
    tab_style(style = cell_text(align = "left",weight="bold")
              ,locations = cells_title(groups="title")) %>%
    tab_style(style = cell_text(align = "left",weight="bold")
              ,locations = cells_row_groups() ) %>%
    tab_style(style = cell_text(align = "left")
              ,locations = cells_title(groups="subtitle")) %>%
    tab_style(style = cell_text(align = "center")
              ,locations = cells_body(columns=!season) ) %>%
    tab_style(style = cell_text(align = "center",weight="bold")
              ,locations = cells_column_labels(columns=!season)) %>%
    tab_style(style = cell_text(align = "left",weight="bold")
              ,locations = cells_column_labels(columns=season)) %>%
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
    cols_width(season ~ px(160)
               ,`Season #` ~ px(80)
               ,`Final placement` ~ px(85)
               ,chef ~ px(130)
               #,`Episode back into main competition` ~ px(120)
               , everything() ~ px(120) )  %>%
    tab_header(
      title = paste0("Top Chef Last Chance Kitchen: Winners")
      ,subtitle = "LCK = Last Chance Kitchen"
    ) %>%
    data_color(method="numeric",
               columns=`Final placement`,
               palette=rev(c("#c85200","#ffbc69", "#a3cce9","#5fa2ce"
                         ,"#1170AA","#141B41")),
               domain=c(1,11))%>%
    data_color(method="numeric",
               columns=`Proportion of LCK challenges won`,
               palette=c("#c85200","#ffbc69", "#a3cce9","#5fa2ce"
                             ,"#1170AA","#141B41"),
               domain=c(0,1))

  gtsave(winnersTable
         ,filename = paste(directory,"LCKWinnerStats.png",sep=""))

  ## Winners and how much of the competition they missed/were part of
  winnermissedepisTable <-
    winnerepisodeinfo %>% ungroup() %>%
    rename(`Episodes missed of main competition`=epsMissed
           ,`Length of run in main competition post-LCK` = lengthofrunpostlck
           ,`Proportion of post-LCK episodes they were in`=proportionofpostlckseasonin
           ,`Season #`=seasonNumber) %>%
    gt() %>%
    cols_hide(columns=c(series,whenbackin)) %>%
    tab_source_note(source_note = "Created by Carly Levitz for Pack Your Knives") %>%
    tab_source_note(source_note = "There are three chefs who started in LCK and came back into the main competition: Lee Anne (S15), Brother (S16), and Soo (S21).") %>%
    tab_row_group(label = "Returned to main competition episode 10 or later",rows = whenbackin=="End of season") %>%
    tab_row_group(label = "Returned to main competition before episode 10",rows = whenbackin=="Mid-season") %>%
    tab_options(data_row.padding = px(1),
                column_labels.padding = px(1),
                row_group.padding = px(1))  %>%
    tab_style(style = cell_text(align = "left")
              ,locations = cells_source_notes()) %>%
    tab_style(style = cell_text(align = "left",weight="bold")
              ,locations = cells_title(groups="title")) %>%
    tab_style(style = cell_text(align = "left",weight="bold")
              ,locations = cells_row_groups() ) %>%
    tab_style(style = cell_text(align = "left")
              ,locations = cells_title(groups="subtitle")) %>%
    tab_style(style = cell_text(align = "center")
              ,locations = cells_body(columns=!season) ) %>%
    tab_style(style = cell_text(align = "center",weight="bold")
              ,locations = cells_column_labels(columns=!season)) %>%
    tab_style(style = cell_text(align = "left",weight="bold")
              ,locations = cells_column_labels(columns=season)) %>%
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
    cols_width(season ~ px(160)
               ,`Season #` ~ px(80)
               #,`Final placement` ~ px(85)
               ,chef ~ px(130)
               #,`Episode back into main competition` ~ px(120)
               , everything() ~ px(120) )  %>%
    tab_header(
      title = paste0("Top Chef Last Chance Kitchen: Episodes missed and length of run post-LCK")
      ,subtitle = "LCK = Last Chance Kitchen. Length of run post-LCK is the number of episodes of the main competition in which the chef appears. That is then compared to the number of episodes left in the competition at the time they returned from LCK."
    ) %>%
    data_color(method="numeric",
               columns=`Proportion of post-LCK episodes they were in`,
               palette=c("#c85200","#ffbc69", "#a3cce9","#5fa2ce"
                         ,"#1170AA","#141B41"),
               domain=c(0,1))

  gtsave(winnermissedepisTable
         ,filename = paste(directory,"LCKWinnerStats_Episodes.png",sep=""))



