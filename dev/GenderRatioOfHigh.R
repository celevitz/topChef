library(topChef)
library(ggplot2)
library(tidyverse)

rm(list=ls())

## Prep the data
  ## For each season, what is the starting gender ratio?
  overallgenderratio <- topChef::chefdetails %>%
    group_by(series,season,seasonNumber,gender) %>%
    summarise(startingN=n()) %>%
    pivot_wider(names_from=gender,values_from=startingN) %>%
    rename(seasonWomen=Female,seasonMen=Male)

  ## for each episode & challenge, what is the gender ratio?
  episodegenderratio <- topChef::challengewins %>%
    left_join(topChef::chefdetails %>%
                select(name,chef,series,season,seasonNumber,gender)) %>%
    filter(inCompetition == TRUE & !(is.na(gender))) %>%
    group_by(series,season,seasonNumber,episode,challengeType,gender) %>%
    summarise(episodechallengeN=n()) %>%
    pivot_wider(names_from=gender,values_from=episodechallengeN) %>%
    rename(ChallengeWomen = Female, ChallengeMen = Male)

  ## what is the gender ratio of winners/highs vs not?
  outcomegenderratio <- topChef::challengewins %>%
    left_join(topChef::chefdetails %>%
                select(name,chef,series,season,seasonNumber,gender)) %>%
    filter(inCompetition == TRUE & !(is.na(gender)) &
             outcome %in% c("WIN","WINNER","HIGH")) %>%
    group_by(series,season,seasonNumber,episode,challengeType,gender) %>%
    summarise(outcomeN=n()) %>%
    pivot_wider(names_from=gender,values_from=outcomeN) %>%
    rename(WinHighWomen = Female,WinHighMen=Male)

  ## Flag the episodes with more than 5 chefs
  keepepisodes <- topChef::episodeinfo %>%
    mutate(keep=ifelse(nCompetitors > 5,"keep","drop")) %>%
    select(series,season,seasonNumber,episode,keep)


## Bring all the information together
## Analyze just the US series
  alldata <- keepepisodes %>%
    full_join(outcomegenderratio) %>%
    full_join(episodegenderratio) %>%
    full_join(overallgenderratio) %>%
    filter(keep == "keep" & series == "US" & !(is.na(challengeType))) %>%
    select(!keep)


  # for the NAs, make them 0
  for (varname in names(alldata)[6:11]) {
    alldata[is.na(alldata[,varname]),varname] <- 0
  }


## calculate the ratios
  alldataratios <- alldata %>%
    mutate(seasonRatio = seasonWomen/seasonMen
       ,challengeRatio = ChallengeWomen/ChallengeMen
       ,WinHighRatio = WinHighWomen/WinHighMen
       ,categoryofchallenge=case_when(ChallengeWomen == ChallengeMen ~ "b. equal"
                                  ,ChallengeWomen < ChallengeMen ~ "a. fewer women"
                                  ,ChallengeWomen > ChallengeMen ~ "c. more women")
       ,categoryofhighwin=case_when(WinHighWomen == WinHighMen ~ "b. equal"
                                      ,WinHighWomen < WinHighMen ~ "a. fewer women"
                                      ,WinHighWomen > WinHighMen ~ "c. more women"))


## Key information
  # gender ratio of all seasons
  summarybyseason <- overallgenderratio %>%
    filter(series == "US") %>%
    mutate(ratio = seasonWomen/seasonMen
           ,categoryofseason=case_when(seasonWomen == seasonMen ~ "equal"
                                       ,seasonWomen < seasonMen ~ "fewer women"
                                       ,seasonWomen > seasonMen ~ "more women"))
  summary(summarybyseason$ratio)
  table(summarybyseason$categoryofseason)
    # which seasons have more women?
    summarybyseason %>% filter(categoryofseason == "more women")

  # Number of challenges
  nrow(alldataratios)

  # Number of challenges with X number of women
  table(alldataratios$ChallengeWomen)
  summary(alldataratios$ChallengeWomen)

  # Number of challenges with X number of men
  table(alldataratios$ChallengeMen)
  summary(alldataratios$ChallengeMen)

  # how do the gender ratios of the challenges to the highs/wins compare?
  table(alldataratios$categoryofchallenge)
  table(alldataratios$categoryofhighwin)
  table(alldataratios$categoryofchallenge,alldataratios$categoryofhighwin)
  chisq.test(alldataratios$categoryofchallenge,alldataratios$categoryofhighwin)

  summary(alldataratios$challengeRatio)
  summary(alldataratios$WinHighRatio)

## for just when there are more than 2 wins/highs
## should I exclude Team challs?
  limited <- alldataratios %>%
    filter(WinHighWomen+WinHighMen > 2)


  chisq.test(limited$categoryofchallenge,limited$categoryofhighwin)

  limited$WinHighRatio[is.infinite(limited$WinHighRatio)] <- 3
  cor.test(limited$challengeRatio,limited$WinHighRatio
           ,method="pearson", use = "complete.obs")








