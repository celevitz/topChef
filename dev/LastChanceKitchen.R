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
             ,epbackin = ifelse(chef == "Louis M.",16,epbackin)) %>%
    select(season,seasonNumber,chef,firstep,epout,epbackin) %>%
    distinct() %>%
    ## fix a few things
    mutate(firstep = ifelse(chef == "Brother L.",6,firstep)
           ,epout = ifelse(chef == "Lee Anne W.",5,epout)
           ,epsMissed = epbackin-epout-1)

## Combine the data
  alldata <- challengeswon %>%
    left_join(challengescompetedin) %>%
    left_join(missedepisodes) %>%
    left_join(returningchefs ) %>%
    mutate(cameback = ifelse(is.na(cameback),"no",cameback)
           ,percentwon = nWon/nCompetedIn)


## Analysis
  ## By whether they came back
  alldata %>%
    group_by(cameback) %>%
    summarise(percentwon = mean(percentwon,na.rm=T)
              ,nWon = mean(nWon,na.rm=T)
              ,nCompetedIn = mean(nCompetedIn,na.rm=T)
              ,n=n())

  #t.test(alldata$nCompetedIn[alldata$cameback == "no"]
  #       ,alldata$nCompetedIn[alldata$cameback == "yes"])
  t.test(alldata$nWon[alldata$cameback == "no"]
         ,alldata$nWon[alldata$cameback == "yes"])
  t.test(alldata$percentwon[alldata$cameback == "no"]
         ,alldata$percentwon[alldata$cameback == "yes"])

  ## Number of chefs who won different #s of LCK challenges
  alldata %>%
    group_by(nWon,cameback) %>%
    summarise(n=n()) %>%
    pivot_wider(names_from=cameback,values_from=n)

  ## Episodes missed of those who came back to main comp.
  alldata %>%
    group_by(epsMissed) %>%
    summarise(n=n())

  ## Number competed in
  alldata %>%
    group_by(cameback,nCompetedIn) %>%
    summarise(n=n()) %>%
    pivot_wider(names_from=cameback,values_from=n)







