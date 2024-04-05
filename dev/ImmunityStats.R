rm(list=ls())
library(tidyverse)
library(openxlsx)
library(devtools)
devtools::install_github("celevitz/topChef")


resultsraw <- topChef::challengewins %>%
  filter(series == "US")

descriptionsraw <- topChef::challengedescriptions %>%
  filter(series == "US")

results <- resultsraw %>%
  left_join(descriptionsraw %>%
              select(season,seasonNumber,series
                     ,episode,challengeType,outcomeType))

###############################################################################
## Part 1: Check the underlying data: do they make sense?
# Flag for immunity
results <- results %>%
  group_by(season,seasonNumber,series,episode) %>%
  mutate(episodeimmunity = max(ifelse(immune == "TRUE",1,0)) )

# episodes with immunity in results file
episwithimm <-  results %>% ungroup() %>%
  group_by(season,seasonNumber,series,episode) %>%
  mutate(episodeimmunity = max(ifelse(immune == "TRUE",1,0)) ) %>%
  select(season,seasonNumber,series,episode,episodeimmunity) %>%
  distinct() %>%
  ungroup()

# now cross reference that with what challenges in the description file have immunity
advantagelistedasimm <- descriptionsraw %>%
  group_by(season,seasonNumber,series,episode) %>%
  mutate(episodeimmunity = max(ifelse(grepl("mmun",advantage),1,0))
         ,immuneasadv = ifelse(episodeimmunity == 1,"In descr file as imm given","not in desc file as imm given")) %>%
  select(season,seasonNumber,series,episode,immuneasadv)

immunityIssueList <- episwithimm %>%
  full_join(advantagelistedasimm) %>%
  # drop the ones that match
  filter(!(episodeimmunity == 1 & immuneasadv == "In descr file as imm given")) %>%
  filter(!(episodeimmunity == 0 & immuneasadv == "not in desc file as imm given"))




###############################################################################
## Part 2: Let's then understand what sort of episodes had immunity
## Descriptive stats

# Season stats
# number of episodes in which immunity was given
data.frame(
  results %>% ungroup() %>%
    select(season,seasonNumber,series,episode,episodeimmunity) %>%
    distinct() %>%
    group_by(season,seasonNumber,series) %>%
    summarise(episodeswithimmunity=sum(episodeimmunity)
              ,numberofepisodes = n()) %>%
    arrange(seasonNumber) %>%
    full_join(


      # how many "slots" of immunity were offered?
      results %>%
        filter(immune == "TRUE") %>%
        select(season,seasonNumber,series,episode,challengeType,chef) %>%
        distinct() %>%
        group_by(seasonNumber,season,series) %>%
        summarise(numberofoppstowinimmunity = n())
    ) %>%
    full_join(

      # how many UNIQUE people in each season won immunity?
      results %>%
        ungroup() %>%
        filter(immune == "TRUE") %>%
        select(season,seasonNumber,series,chef) %>%
        distinct() %>%
        group_by(seasonNumber,season,series) %>%
        summarise(numberofchefswonimmunity = n())
    ) %>%
    select(!series)
) # close data frame


# types of challenges in which immunity was given out
# check that each season is represented and that there aren't any normal elimination challenges in here
descriptionsraw %>%
  mutate(immunity = ifelse(grepl("mmun",advantage),1,0)) %>%
  filter(immunity == 1) %>%
  group_by(season,seasonNumber,series,challengeType,outcomeType) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from="outcomeType",values_from="n") %>%
  arrange(seasonNumber,challengeType)

# summary of the types of challenges in which immunity was given out
descriptionsraw %>%
  mutate(immunity = ifelse(grepl("mmun",advantage),1,0)) %>%
  filter(immunity == 1) %>%
  group_by(season,seasonNumber,series,challengeType,outcomeType) %>%
  summarise(n=n()) %>%
  group_by(challengeType,outcomeType) %>%
  summarise(numberofchallenges=sum(n))



# How many immunities did each chef win?
results %>%
  ungroup() %>%
  filter(immune == "TRUE") %>%
  select(season,seasonNumber,series,episode,chef) %>%
  group_by(seasonNumber,season,series,chef) %>%
  summarise(numberofepisodeswonimmunity=n()) %>%
  arrange(desc(numberofepisodeswonimmunity),seasonNumber,chef) %>%
  print(n=130)

# number of chefs who won immunity each number of times
results %>%
  ungroup() %>%
  filter(immune == "TRUE") %>%
  select(season,seasonNumber,series,episode,chef) %>%
  group_by(seasonNumber,season,series,chef) %>%
  summarise(numberofepisodeswonimmunity=n()) %>%
  ungroup() %>%
  group_by(numberofepisodeswonimmunity) %>%
  summarise(numberofchefs = n())

# # of chefs who won immunities in multiple seasons
results %>%
  ungroup() %>%
  filter(immune == "TRUE") %>%
  select(season,seasonNumber,series,chef) %>%
  distinct() %>%
  group_by(series,chef) %>%
  summarise(numberofseasonswonimmunity=n()) %>%
  arrange(desc(numberofseasonswonimmunity),series,chef) %>%
  filter(numberofseasonswonimmunity > 1)

# In what challenge types did immunity come into play?
# In Charleston (season 14), the immunity was for SDQ
results %>% ungroup() %>%
  filter(immune == "TRUE") %>%
  select(season,seasonNumber,series,episode,challengeType) %>%
  distinct() %>%
  group_by(seasonNumber,season,series,challengeType) %>%
  summarise(numberofchallengeswithimmunity=n()) %>%
  arrange(desc(numberofchallengeswithimmunity),seasonNumber,challengeType) %>%
  print(n=30)

results %>% ungroup() %>%
  filter(immune == "TRUE") %>%
  select(season,seasonNumber,series,episode,challengeType,outcomeType) %>%
  distinct() %>%
  group_by(challengeType,outcomeType) %>%
  summarise(numberofchallengeswithimmunity=n())




###############################################################################
## Part 3: performance

# how did people w/ immunity perform in the challenges in which they had
# the N here will equal the number of "slots" of immunity offered
thosewithimmunity <- results %>%
  filter(immune == "TRUE") %>%
  group_by(series,challengeType,outcomeType,outcome) %>%
  summarise(numberofchefs = n())



## % win with immunity compared to % win without immunity
## just the CHALLENGES for which immunity was a factor?

temp <- data.frame(results %>%
                     ungroup() %>%
                     group_by(season,seasonNumber,series,episode,challengeType) %>%
                     mutate(avgwin = case_when(outcome %in% c("WIN","WINNER")~ 1
                                               ,TRUE ~ 0 )
                            ,challengehadimm = max(ifelse(immune == TRUE,1,0)) ) %>%
                     filter(challengehadimm == 1)
)

t.test(temp$avgwin ~ as.character(temp$immune))
table(temp$avgwin,temp$immune)
chisq.test(temp$avgwin,temp$immune)

# How did they do in the FOLLOWING elimination challenge?

# quick and dirty, and not totally correct
# list of episodes in which chefs had immunity
whohasimmunity <- results %>%
  filter(immune == "TRUE") %>%
  select(season,seasonNumber,episode,challengeType,chef) %>%
  # tick that episode up by one
  # look at the results of that next elimination challenge
  # (this takes a simplistic view, cuz sometimes the next elim chall is SDQ)
  mutate(episode = episode+1
         ,challengeType = "Elimination") %>%
  # Look at just those challenges for those chefs
  left_join(results)

table(whohasimmunity$outcome)






# It's not as simple as just looking at immunity in the next episode, given that
# there was a case where the immunity affected a sudden death quickfire
# (Season 14, episodes 1 and 7)
# the raw description data is put in order of when the challenge occurred, I think.

rownumberofallchalls <- results %>%
  group_by(season,seasonNumber,series,episode,challengeType) %>%
  mutate(challengehadimm = max(ifelse(immune == TRUE,1,0))) %>%
  full_join(descriptionsraw %>%
              mutate(rownumber=as.numeric(row.names(descriptionsraw))) %>%
              select(season,seasonNumber,series,episode,challengeType
                     ,outcomeType,rownumber))

rownumberofjustelims <- rownumberofallchalls %>%
  filter(!(challengeType %in% c("Qualifying Challenge","Quickfire"))) %>%
  select(!c(inCompetition,immune,chef,outcome,rating)) %>%
  distinct()












