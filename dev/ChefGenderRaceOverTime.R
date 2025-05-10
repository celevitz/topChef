rm(list=ls())
library(tidyverse)
library(topChef)

directory <- "/Users/carlylevitz/Documents/Data/topChef/"
chefdetails <- read.csv(paste0(directory,"Top Chef - Chef details.csv")) %>%
  filter(series == "US" &
           !(placement %in% c("Eliminated in qualifying rounds"
         ,"Didn't make it out of LCK" )) ) %>%
  mutate(personOfColor = ifelse(is.na(personOfColor),"white",personOfColor)
         ,placement=as.numeric(placement))

## Ratio of gender at start of season compared to F8

startofseason <- chefdetails %>%
  group_by(series,season,seasonNumber,gender) %>%
  summarise(n=n())

f8 <- chefdetails %>%
  filter(placement <= 8 | is.na(placement)) %>%
  group_by(series,season,seasonNumber,gender) %>%
  summarise(n=n())

winnergender <- chefdetails %>%
  filter(placement == 1) %>%
  select(series,season,seasonNumber,gender) %>%
  rename(genderofwinner = gender)

ratiocomparison <- startofseason %>%
  mutate(timing = "SeasonStart") %>%
  bind_rows(f8 %>%
              mutate(timing = "final8")) %>%
  pivot_wider(names_from=gender,values_from=n) %>%
  mutate(ratio = Female/(Female+Male)) %>%
  select(!c(Female,Male)) %>%
  pivot_wider(names_from=timing,values_from=ratio) %>%
  left_join(winnergender) %>%
  mutate(compare = case_when(final8 > SeasonStart ~
                               "Greater ratio of women at F8 than at start"
     ,final8<SeasonStart  ~ "Smaller ratio of women at F8 than at start"
     ,final8==SeasonStart  ~ "Same gender ratio at F8 than at start")
     ,difference = final8-SeasonStart
     ,seasonNumberAsString = ifelse(seasonNumber <=9
                                    ,paste0("0",as.character(seasonNumber))
                                    ,as.character(seasonNumber))
     ,season = paste0("Season ",seasonNumberAsString," ",season)
     ,genderofwinner = ifelse(is.na(genderofwinner),"Current season"
                              ,paste0(genderofwinner," winner"))) %>%
  ungroup() %>%
  select(!c(series,seasonNumber,seasonNumberAsString))

ratiocomparison %>%
  arrange(final8,season) %>%
  print(n=22)

## The average % of women on season casts was 46.9%, ranging from 36.8% to 56.25%.
## Only two seasons have had more men than women at the start: Season 4 and Season 19. Season 4 had a female winner (Stephanie Izard) but Season 19 had a male winner (Buddha Lo).
## The season that started with the fewest women compared to men was Season 11 (New Orleans). Only 36.8% of the cast were women.
## All of the other seasons had casts that were more than 41% women.
## Five seasons had an equal number of men and women: Season 1 San Francisco, Season 9 Texas, Season 10 Seattle, Season 15 Colorado and Season 20 World All Stars.
##--Four of these seasons had a F8 with the same gender ratio at F8 as the start; however, three of those four seasons had male winners.
##--1 of those seasons had a smaller ratio of women at F8 than at the start, and there was a male winner.

## The average % of women at the F8 across seasons was 44.4%, ranging from 25% to 62.5%
## At the final 8, the ratio of women to men ranged from 25% to 62.5%.
## The seasons with the fewest women at F8 were Seasons 3 Miami and 13 California. These are also the two seasons with the largest drop in % of the cast being women; in Season 3 Miami, the F8 had a % that was 21.7% lower than at the start of the season and this was 16.2% for Season 13 California.
## Twelve seasons had an equal number of men and women at F8.
##--Eight of these seasons had a male winner.
##--Seven of them had a greater ratio of women at the F8 than at the start, but four of the seven had a male winner.

table(ratiocomparison$compare
      ,ratiocomparison$genderofwinner)
chisq.test(ratiocomparison$compare[ratiocomparison$genderofwinner != "Current season"]
      ,ratiocomparison$genderofwinner[ratiocomparison$genderofwinner != "Current season"])


table(ratiocomparison$compare[ratiocomparison$SeasonStart == .5]
      ,ratiocomparison$genderofwinner[ratiocomparison$SeasonStart == .5])

table(ratiocomparison$compare[ratiocomparison$final8 == .5]
      ,ratiocomparison$genderofwinner[ratiocomparison$final8 == .5])


write.csv(ratiocomparison %>%
            arrange(season)
          ,paste0(directory,"GenderAtF8.csv")
          ,row.names=FALSE)












removenoplacement <- chefdetails %>%
  mutate(placement=as.numeric(placement)
         ,highleveloccupation = case_when(occupation_category %in%
                              c("Executive pastry chef"
                              ,"Line cook","Private chef","Sout Chef") ~ "Other"
                              ,TRUE ~ occupation_category))

placementreg <- lm(removenoplacement$placement ~ removenoplacement$gender +
                     removenoplacement$personOfColor +
                     removenoplacement$highleveloccupation )
summary(placementreg)





nrow(chefdetails)
nrow(chefdetails %>% select(name) %>% distinct())

# How many times are they in the data?
    temp <- chefdetails %>%
      group_by(chef) %>%
      summarise(n=n())
    table(temp$n)

# How many of each gender? (all chefs, even those who didn't qualify for main)
chefdetails %>%
  group_by(gender) %>%
  summarise(n=n()) %>%
  mutate(percent = n/sum(n))

chefdetails %>%
  group_by(personOfColor) %>%
  summarise(n=n()) %>%
  mutate(percent = n/sum(n))

chefdetails  %>%
  group_by(personOfColor,gender) %>%
  summarise(n=n()) %>%
  mutate(percentOfRace = n/sum(n)) %>%
  ungroup() %>%
  mutate(percentOfTotal = n/sum(n)) %>%
  group_by(gender) %>%
  mutate(percentOfGender=n/sum(n))

# Gender over time
chefdetails %>%
  group_by(seasonNumber,season,gender) %>%
  summarise(n=n()) %>%
  mutate(percent = n/sum(n)) %>%
  filter(gender == "Female") %>%
  print(n=22)


# People of color over time
chefdetails %>%
  group_by(seasonNumber,season,personOfColor) %>%
  summarise(n=n()) %>%
  mutate(percent = n/sum(n)) %>%
  filter(personOfColor == "POC") %>%
  print(n=22)


# region <- chefdetails %>%
#   mutate(nyorca = ifelse(state %in% c("New York","California"),"NY/CA","other")) %>%
#   group_by(seasonNumber,season,nyorca) %>%
#   summarise(n=n()) %>%
#   mutate(percent = n/sum(n)) %>%
#   filter(nyorca == "other") %>%
#   print(n=21)
# summary(region$percent)


## Placement and gender/race
placement <- chefdetails %>%
  # get rid of those who were eliminated in the qualifying rounds or
  #   who didn't make it out of LCK
  filter(!(placement %in% c("Didn't make it out of LCK"
                          ,"Eliminated in qualifying rounds"))) %>%
  mutate(placement=as.numeric(placement)
         # there's a huge difference before and after season 18
         ,seasoncategory = ifelse(seasonNumber >=18,"S18 and after"
                                  ,"Seasons before 18")
         # create combined categories
         ,combinedcategory = paste(personOfColor,gender,seasoncategory,sep="\n"))

  placementsummary <- placement %>%
    group_by(combinedcategory) %>%
    summarise(count = n(),
      mean = mean(placement,na.rm=TRUE),
      stddev = sd(placement, na.rm=TRUE),
      meansd_l = mean - stddev,
      meansd_u = mean + stddev
    )

  # graph
  ggplot(placementsummary, aes(x=combinedcategory, y=mean)) +
    #Now plotting the individual data points before the mean values
    geom_point(data=placement, aes(x=combinedcategory, y=placement
                                   ,color=seasoncategory
                                   ,shape=gender)
               , position = position_jitter()) +
    geom_point() +
    geom_errorbar(aes(ymin = meansd_l, ymax = meansd_u), width=0.1) +
    ylim(0.5,20) +
    labs(x="Category", y = "Placement (-/+SD) [lower is better]")



  # regression
  summary(lm(placement$placement ~ placement$personOfColor +
               placement$gender))

  priorto18 <- placement %>% filter(seasonNumber <18)
  summary(lm(priorto18$placement ~ priorto18$personOfColor +
               priorto18$gender))


  # Is Season 18 a turning point? chi square
  chisq.test(placement$seasoncategory,placement$gender)

  chisq.test(placement$seasoncategory,placement$personOfColor)






