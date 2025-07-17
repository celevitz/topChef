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

confsRaw <- as_tibble(read.xlsx(paste(directory
                                     ,"TopChefData.xlsx",sep="")
                               ,sheet=9))

placement <- read.csv(paste0(directory,"topChef/Top Chef - Chef details.csv")
                      ,header=TRUE)
placement <- placement %>%
  select(series,season,seasonNumber,chef,placement)
challengewins <- read.csv(paste0(directory,"topChef/Top Chef - Challenge wins.csv")
                          ,header=TRUE)

# who showed up at the judges' table? just for elimination challenge.
# use max of the 1 and 0 in the case of multiple elim challs in an ep.
wonElimChall <- challengewins %>%
  mutate(ElimWinner=max(ifelse(challengeType == "Elimination" &
                             outcome %in% c("WIN","WINNER"),1,0))
         ,atJTElim = max(ifelse(challengeType == "Elimination" &
                              outcome %in% c("OUT","LOW","HIGH","WIN","WINNER"
                                             ,"RUNNER-UP"),1,0))
         ,OutWithdrewElim = max(ifelse(challengeType == "Elimination" &
                                   outcome %in% c("OUT","WITHDREW","RUNNER-UP"
                                              ,"DISQUALIFIED"),1,0))) %>%
  select(season,seasonNumber,series,episode,chef,ElimWinner,atJTElim
         ,OutWithdrewElim) %>%
  # remove duplicates (i.e., because there are multiple challenges per episode)
  distinct() %>%
  # need to update the chefs' names who have accents in them
  mutate(chef = case_when(chef == "Kevin D'Andrea" ~ "Kévin D'Andrea"
                          ,chef == "Cesar Murillo" ~ "César Murillo"
                          ,chef == "Begona Rodrigo" ~ "Begoña Rodrigo"
                          ,TRUE ~ chef))

## Episode-specific stats
  confsByEpi <- confsRaw %>%
    filter(inCompetition == "TRUE") %>%
    ## How many chefs are in the episode? How many total confessionals?
    group_by(season,seasonNumber,series,episode) %>%
    mutate(chefsinepisode = length(unique(ifelse(inCompetition == "TRUE"
                                                 ,chef,NA)))
           ,totalconfsinep = sum(count,na.rm=T)
           # average per episode and difference from it
           ,equalInEp = totalconfsinep/chefsinepisode
           ,equalInEpPercent = 1/chefsinepisode
           ,difffromequalinEp = count - equalInEp
           ,percentdifffromequalinEp = (count - equalInEp)/equalInEp
           ,percentofEpsConfs = count/totalconfsinep
           ,edit = case_when(percentofEpsConfs > equalInEpPercent ~ "Over-shown"
                   ,percentofEpsConfs < equalInEpPercent ~ "Under-shown"
                   ,percentofEpsConfs == equalInEpPercent  ~ "Shown as expected"
                   ,TRUE ~ "Not in episode")
           ) %>%
    ## Who won the elim chall in that ep?
    left_join(wonElimChall)


  write.csv(confsByEpi
          ,paste0(directory,"/topChef/Top Chef - Confessionals by episode.csv")
            ,row.names=FALSE)

## Chef specific stats
  confs <- confsRaw %>%
    ungroup() %>% group_by(season,seasonNumber,series) %>%
    mutate(totalconfs = sum(count,na.rm=T)
           ,totalchefsepisodes = sum(ifelse(inCompetition == "TRUE",1,0))) %>%
    # chef stats
    ungroup() %>% group_by(season,seasonNumber,series,chef
                           ,totalconfs,totalchefsepisodes) %>%
    ## How many episodes is the chef in?
    mutate(episodesIn = sum(ifelse(inCompetition == "TRUE",1,0))
           ## How many times are they the first confessional?
           ## keep just the ones that are first or before intro -not after intro
           ,first = ifelse(first == "after intro",NA,first)
           ,firstconfs = sum(ifelse(!(is.na(first)),1,0))
           ,phonecallsorphotos = sum(ifelse(!(is.na(phone.call)),1,0))
           ,chefconfs = sum(count,na.rm=T)
           ,expectedpercentofconfs = episodesIn/totalchefsepisodes
           ,observedpercent = chefconfs/totalconfs
           ,difffromexpected = observedpercent-expectedpercentofconfs
           ,percentdifffromexpected = (observedpercent-expectedpercentofconfs)/
             expectedpercentofconfs
           ) %>%
    select(season,seasonNumber,series,chef
           ,totalconfs,totalchefsepisodes
           ,firstconfs,phonecallsorphotos,chefconfs
           ,expectedpercentofconfs,observedpercent
           ,difffromexpected) %>%
    distinct()

  write.csv(confs
          ,paste0(directory,"/topChef/Top Chef - Confessionals in a season.csv")
            ,row.names=FALSE)


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






