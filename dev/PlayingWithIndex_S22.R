rm(list=ls())
library(tidyverse)
library(openxlsx)
library(topChef)
library(ggplot2)
library(devtools)
devtools::install_github("celevitz/topChef")

challengewins <- topChef::challengewins %>%
  filter(series == "US")

chefdetails <- topChef::chefdetails %>%
  filter(series == "US" )

# Prior to the Double Elimination at 12 chefs: 4 elims and 4 QFs
    episodenumber <- 4
    numberofelimchalls <- 4
    numberofquickfirechalls <- 4
    eliminatedchefs <- c("Anya El-Wattar","Zubair Mohajir"
                         ,"Mimi Weissenborn","Sam Olayinka"
                         ,"Ying Gao")

    ## Stats about S22
    s22challstats <- weightedindex("US",22,numberofelimchalls,numberofquickfirechalls) %>%
      mutate(eliminated = ifelse(chef %in% eliminatedchefs,"Out","In the competition")) %>%
      select(!c(season,seasonNumber,series,placement)) %>%
      arrange(eliminated,desc(indexWeight))

    s22challstats <- s22challstats[,c("chef","eliminated","Quickfire.WIN"
                                      ,"Quickfire.HIGH","Quickfire.LOW"
                                      ,"Elimination.WIN","Elimination.HIGH"
                                      ,"Elimination.LOW"
                                      ,"indexWeight")]

    ## compare ranks
    currentrank <- s22challstats %>%
      arrange(eliminated,desc(indexWeight)) %>%
      select(chef,eliminated,indexWeight) %>%
      mutate(rank = row.names(s22challstats)
             ,season = 22)
      # compare to Danny and Buddha

    s21 <- weightedindex("US",21,numberofelimchalls,numberofquickfirechalls)
    row.names(s21) <- NULL
    s21 <- s21 %>%
      arrange(desc(indexWeight)) %>%
      mutate(rank = row.names(s21)
             ,season = 21) %>%
      filter(chef == "Danny Garcia" | rank == 1) %>%
      select(chef,rank,season,indexWeight,placement)

    s20 <-  weightedindex("US",20,numberofelimchalls,numberofquickfirechalls)
    row.names(s20) <- NULL
    s20 <- s20 %>%
      arrange(desc(indexWeight)) %>%
      mutate(rank = row.names(s20)
             ,season = 20) %>%
      filter(chef == "Buddha" | rank == 1) %>%
      select(chef,rank,season,indexWeight,placement)

    s19 <- weightedindex("US",19,numberofelimchalls,numberofquickfirechalls)
    row.names(s19) <- NULL
    s19 <- s19 %>%
      arrange(desc(indexWeight)) %>%
      mutate(rank = row.names(s19)
             ,season = 19) %>%
      filter(chef == "Buddha" | rank == 1) %>%
      select(chef,rank,season,indexWeight,placement)

    s18 <- weightedindex("US",18,numberofelimchalls,numberofquickfirechalls)
    row.names(s18) <- NULL
    s18 <- s18 %>%
      arrange(desc(indexWeight)) %>%
      mutate(rank = row.names(s18)
             ,season = 18) %>%
      filter(chef == "Gabe E." | rank == 1) %>%
      select(chef,rank,season,indexWeight,placement)

    currentrank %>%
      bind_rows(s21 %>% mutate(eliminated = "In the competition")) %>%
      bind_rows(s20 %>% mutate(eliminated = "In the competition")) %>%
      bind_rows(s19 %>% mutate(eliminated = "In the competition")) %>%
      bind_rows(s18 %>% mutate(eliminated = "In the competition"))

## What's the spread of scores at this # of QFs and Elims?
    allseasons <- weightedindex("US",1
                                ,numberofelimchalls,numberofquickfirechalls)
    for (season in seq(2,22,1)) {
      allseasons <- rbind(allseasons
                          ,weightedindex("US",season,numberofelimchalls
                                         ,numberofquickfirechalls))

    }

    allseasons %>%
      group_by(season,seasonNumber) %>%
      summarise(standarddeviation = sd(indexWeight,na.rm=T)
                ,differencebtwMaxAndMin = max(indexWeight,na.rm=T) -
                                          min(indexWeight,na.rm=T)
                  ) %>%
      arrange(standarddeviation) %>%
      print(n=22)


