rm(list=ls())
library(tidyverse)
library(devtools)
library(topChef)

lck <- topChef::challengedescriptions %>%
  filter(!(is.na(lastChanceKitchenWinnerEnters)) &
           series == "US") %>%
    # the San Francisco returning doesn't count; it's cuz Cynthia quit
    # exclude season 21 for now, since we don't know how far the two LCK folks will go
    filter(seasonNumber != 1 & seasonNumber != 22)

## Number of episodes someone entered
  nrow(lck)

## Number of episodes in which more than 1 person re-entered
  nrow(lck[grepl(",",lck$lastChanceKitchenWinnerEnters),])

## Number of seasons in which someone reentered
  nrow(lck %>% select(season) %>% distinct())

## Number of times a chef entered back into the competition each season
  lck %>%
    group_by(seasonNumber,season) %>%
    summarise(n=n())

  lck %>%
    group_by(season) %>%
    summarise(numberofreentries=n()) %>%
    ungroup() %>%
    group_by(numberofreentries) %>%
    summarise(numberofseasons=n())

## Amount of episodes people missed
  epi <- lck %>%
    select(season,seasonNumber,lastChanceKitchenWinnerEnters) %>%
    rename(chef=lastChanceKitchenWinnerEnters) %>%
    # separate out Claudette & Lee Anne
    mutate(chef = ifelse(chef == "Claudette Z.-W., Lee Anne W.","Claudette Z.-W.",chef)) %>%
    add_row(season="Colorado",seasonNumber=15,chef="Lee Anne W.") %>%
    # Separate out Soo & Kaleena
    mutate(chef = ifelse(chef == "Soo Ahn, Kaleena Bliss","Soo Ahn",chef)) %>%
    add_row(season="Wisconsin",seasonNumber=21,chef="Kaleena Bliss") %>%
    left_join(topChef::challengewins %>%
                filter(series == "US")) %>%
    # if they were in an episode at all, make it so they are counted for that episode
    group_by(season,seasonNumber,chef,episode) %>%
    mutate(temp_inEpisode = max(ifelse(inCompetition == TRUE,1,0),na.rm=T)
           ,inCompetition = ifelse(temp_inEpisode == 1, TRUE, FALSE)
        # in which episode are they out?
           ,out = max(case_when(outcome == "OUT" ~ 1
                                ,TRUE ~ 0 ))
        # flag in competition/out
          ,tempvalue = paste0(as.character(inCompetition),as.character(out) )
           ) %>%
    select(season,seasonNumber,chef,episode,tempvalue) %>%
    distinct() %>%
    # mark the different episodes
    ungroup() %>%
    group_by(season,seasonNumber,chef) %>%
    mutate(
      # first episode in
      firstep = case_when(
        seasonNumber  == 16 & chef == "Brother L." ~ 6
        ,seasonNumber == 12 & chef == "George P." ~ 1
        ,TRUE ~ min(ifelse(tempvalue == "TRUE0",episode,NA),na.rm=T)
      )
      # episode eliminated
      ,epelim = case_when(
        seasonNumber == 15 & chef == "Lee Anne W." ~ 5
        ,TRUE ~ min(ifelse(tempvalue == "TRUE1",episode,NA),na.rm=T)
      )
      # episode back in
      ,epbackin = case_when(
        seasonNumber == 16 & chef == "Brother L." ~ 6
        ,seasonNumber == 15 & chef == "Lee Anne W." ~ 5
        ,seasonNumber == 11 & chef == "Louis M." ~ 16
        ,seasonNumber == 21 & chef == "Soo Ahn" ~ NA
        ,TRUE ~ min(ifelse(tempvalue == "TRUE0" & episode >epelim,episode,NA),na.rm=T)
      )

      # episode back out
        ,epelimagain =min(ifelse(tempvalue == "TRUE1" & episode >= epbackin,episode,NA),na.rm=T)
        ,epelimagain = ifelse(is.infinite(epelimagain)
                              ,max(ifelse(tempvalue == "TRUE0" & episode >= epbackin,episode,NA),na.rm=T)
                              ,epelimagain)
        ,epelimagain=ifelse(seasonNumber == 21 & chef == "Soo Ahn" ,NA
                            ,epelimagain)
      # length of first run, LCK run, and 2nd run
      ,firstrun = epelim-firstep+1
      ,lckrun= case_when(
        chef == "Claudette Z.-W." ~ 4
        ,chef %in% c("Lee Anne W.","Brother L.","Soo Ahn") ~ 6
        ,TRUE ~ epbackin-epelim)
      ,secondrun = epelimagain-epbackin

    )

epi %>%
  select(season,seasonNumber,chef,firstrun,lckrun,secondrun) %>%
  distinct() %>%
  arrange(seasonNumber,chef) %>%
  print(n=30)

### For seasons where someone started in LCK, need to change their LCK run
### Lee Anne, Claudette, Brother, Soo

epi %>%
  select(season,seasonNumber,chef,firstrun) %>%
  distinct() %>%
  group_by(firstrun) %>%
  summarise(nchefs=n())

epi %>%
  select(season,seasonNumber,chef,lckrun) %>%
  distinct() %>%
  ungroup() %>%
  mutate(averagerun=mean(lckrun,na.rm=T)) %>%
  ungroup() %>% group_by(averagerun,lckrun) %>%
  summarise(nchefs=n())

epi %>%
  select(season,seasonNumber,chef,secondrun) %>%
  distinct() %>%
  ungroup() %>%
  mutate(averagerun=mean(secondrun,na.rm=T)) %>%
  ungroup() %>% group_by(averagerun,secondrun) %>%
  summarise(nchefs=n())

# length of second run in the main comp
# group by when they came back into the comp
  epi %>%
    left_join(topChef::chefdetails) %>%
    select(season,seasonNumber,chef,epbackin,secondrun,placement) %>%
    distinct() %>%
    mutate(epbackin_cat = if_else(epbackin <=8,"5 to 8","9 or later")) %>%
    group_by(epbackin_cat) %>%
    summarise(avg = mean(secondrun,na.rm=T)
              ,mdn = median(secondrun,na.rm=T)
              ,avgplacement=mean(placement,na.rm=T)
              ,mdnplacement=median(placement,na.rm=T))













