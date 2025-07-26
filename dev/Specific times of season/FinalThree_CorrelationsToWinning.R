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
              mutate(placement = as.numeric(placement)
                     ,seasonWinner = ifelse(placement == 1, 1,0))) %>%
  # add on the number of competitors in that episode
  left_join(read.csv(paste0(directory,"Top Chef - Episode information.csv"))%>%
      select(!c(overallEpisodeNumber,episodeName,airDate))) %>%
  # consolidate challenge types, drop qualifying challenges
  filter(challengeType != "Qualifying Challenge") %>%
  mutate(CT = ifelse(challengeType %in% c("Quickfire Elimination"
                                          ,"Sudden Death Quickfire")
                     ,"Elimination",challengeType))


confsRaw <- read.csv(paste0(directory
                            ,"Top Chef - Confessionals by episode.csv"))%>%
  filter(inCompetition == "TRUE" &
           # keep just the seasons that I have full data for
           seasonNumber %in% c(1,2,21) &
           chefsinepisode > 3
  ) %>%
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
  ) %>% ungroup() %>%
  select(season,seasonNumber,series,chef
         ,firstconfs,chefconfs
         ,difffromexpected) %>%
  distinct()


## Keep only the data with more than 3 competitors, since we want to see if
## there are things correlated with winning at that point in the season
challs <- rawdata %>%
  filter(nCompetitors > 3) %>%
  # stats for each person by challenge type
  group_by(series,season,seasonNumber,chef,CT,placement,seasonWinner) %>%
  summarise(wins = sum(ifelse(outcome %in% c("WIN","WINNER"),1,0))
         ,highs = sum(ifelse(outcome %in% c("WIN","WINNER","HIGH"),1,0))
         ,lows = sum(ifelse(outcome %in% c("LOW","OUT","RUNNER-UP"),1,0))
         ,mostrecentwin = min(ifelse(outcome %in% c("WIN","WINNER")
                                     ,nCompetitors,NA),na.rm=T)) %>%
  pivot_wider(names_from=CT,values_from=c("wins","highs","lows"
                                          ,"mostrecentwin")) %>%
  left_join(confsRaw)

final3 <- challs %>% filter(placement <=3) %>%
  mutate(wins = wins_Elimination+wins_Quickfire
         ,losses = lows_Elimination+lows_Quickfire)
final3seasonswithconfs <- final3 %>%
  filter(seasonNumber %in% c(1,2,21))

final6 <- challs %>% filter(placement <=6) %>%
  mutate(wins = wins_Elimination+wins_Quickfire
         ,losses = lows_Elimination+lows_Quickfire)


## Logistic regression, all vars
reg <- glm(final3$seasonWinner ~ final3$wins_Elimination +final3$wins_Quickfire +
            final3$lows_Elimination + final3$lows_Quickfire +
            final3$mostrecentwin_Elimination #+ final3$season
          # quite a few of the chefs in the final 3s haven't won a QF
          #+ final3$mostrecentwin_Quickfire
          )
reg <- glm(seasonWinner ~ wins_Elimination +wins_Quickfire +
             lows_Elimination + lows_Quickfire +
             mostrecentwin_Elimination,data=final3,family="binomial")
summary(reg)
plot(allEffects(reg))
confint(reg)

## Logistic reg, combingin wins and losses
reg2 <- glm(final3$seasonWinner ~ final3$wins +final3$losses +
            final3$mostrecentwin_Elimination
)
summary(reg2)
confint(reg2)

## Add in the confessionals, but only for the seasons that I have it for
reg3 <- glm(final3seasonswithconfs$seasonWinner ~
             final3seasonswithconfs$wins_Elimination +
             final3seasonswithconfs$wins_Quickfire +
             final3seasonswithconfs$lows_Elimination +
             final3seasonswithconfs$lows_Quickfire +
             final3seasonswithconfs$mostrecentwin_Elimination +
             final3seasonswithconfs$firstconfs +
             final3seasonswithconfs$difffromexpected
)
summary(reg3)
confint(reg3)

## Then combine the wins and losses
reg4 <- glm(final3seasonswithconfs$seasonWinner ~ final3seasonswithconfs$wins +
             final3seasonswithconfs$losses +
             final3seasonswithconfs$mostrecentwin_Elimination +
             final3seasonswithconfs$firstconfs +
             final3seasonswithconfs$difffromexpected
)
summary(reg4)
confint(reg4)

##############
# combined <- as.data.frame(reg$coefficients)
# combined$variable <- row.names(combined)
#
# combined2 <- as.data.frame(confint(reg))
# combined2$variable <- row.names(combined2)
# names(combined2) <- c("lower","upper","variable")
#
# allcombined <- combined %>%
#   left_join(combined2) %>%
#   mutate(regression = "reg1")
#
#   for (eachreg in c(reg2,reg3,reg4)) {
#     holding <- as.data.frame(eachreg$coefficients)
#     holding$variable <- row.names(holding)
#
#     holding2 <- as.data.frame(confint(eachreg))
#     holding2$variable <- row.names(holding2)
#     names(holding2) <- c("lower","upper","variable")
#
#   }
#






  names(holding) <- c("variable","coefficient")

  left_join(data.frame(confint(reg4)))

##############


## Reg 1, but w/ final 6
reg1b <- lm(final6$seasonWinner ~ final6$wins_Elimination +final6$wins_Quickfire +
            final6$lows_Elimination + final6$lows_Quickfire # +
              # There are 15 ppl who hadn't won an elimination challenge
            #final6$mostrecentwin_Elimination #+ final3$season
          # quite a few of the chefs in the final 3s haven't won a QF
          #+ final3$mostrecentwin_Quickfire
)
summary(reg1b)

## Reg 2, but with final 6

reg2b <- lm(final6$seasonWinner ~ final6$wins +final6$losses #+
              #final6$mostrecentwin_Elimination
)
summary(reg2b)




