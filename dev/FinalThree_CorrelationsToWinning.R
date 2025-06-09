rm(list=ls())
library(tidyverse)

directory <- "/Users/carlylevitz/Documents/Data/topChef/"

rawdata <- read.csv(paste0(directory,"Top Chef - Challenge wins.csv"))%>%
  filter(series == "US") %>%
  select(!rating) %>%
  # add in the chefs' placement
  left_join(read.csv(paste0(directory,"Top Chef - Chef details.csv")) %>%
              select(series,season,seasonNumber,chef,placement) %>%
              # change placement data to be numeric. Don't want to include
              # people who didn't make it into the final competition
              mutate(placement = as.numeric(placement))) %>%
  # add on the number of competitors in that episode
  left_join(read.csv(paste0(directory,"Top Chef - Episode information.csv"))%>%
      select(!c(overallEpisodeNumber,episodeName,airDate))) %>%
  # consolidate challenge types, drop qualifying challenges
  filter(challengeType != "Qualifying Challenge") %>%
  mutate(CT = ifelse(challengeType %in% c("Quickfire Elimination"
                                          ,"Sudden Death Quickfire")
                     ,"Elimination",challengeType))

## Keep only the data with more than 3 competitors, since we want to see if
## there are things correlated with winning at that point in the season
challs <- rawdata %>%
  filter(nCompetitors > 3) %>%
  # stats for each person by challenge type
  group_by(series,season,seasonNumber,chef,CT,placement) %>%
  summarise(wins = sum(ifelse(outcome %in% c("WIN","WINNER"),1,0))
         ,highs = sum(ifelse(outcome %in% c("WIN","WINNER","HIGH"),1,0))
         ,lows = sum(ifelse(outcome %in% c("LOW","OUT","RUNNER-UP"),1,0))
         ,mostrecentwin = min(ifelse(outcome %in% c("WIN","WINNER")
                                     ,nCompetitors,NA),na.rm=T)) %>%
  pivot_wider(names_from=CT,values_from=c("wins","highs","lows"
                                          ,"mostrecentwin"))

final3 <- challs %>% filter(placement <=3)

reg <- lm(final3$placement ~ final3$wins_Elimination + final3$wins_Quickfire +
            final3$lows_Elimination + final3$lows_Quickfire +
            final3$mostrecentwin_Elimination + final3$season
          # quite a few of the chefs in the final 3s haven't won a QF
          #+ final3$mostrecentwin_Quickfire
          )
summary(reg)

# when you don't take into account the season, most recent elim win is
# leaning towards stat sig related, but isn't.
# when you take into account the season, most recent elim win is a liiiil
# more sig, but still not stat sig. Quickfire LOSSES becomes significant.
# But it's a negative estimate. So as you have MORE losses, your placement
#is better???

