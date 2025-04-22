rm(list=ls())
library(tidyverse)
library(topChef)

directory <- "/Users/carlylevitz/Documents/Data/topChef/"
chefdetails <- read.csv(paste0(directory,"Top Chef - Chef details.csv")) %>%
  filter(series == "US" ) %>%
  mutate(personOfColor = ifelse(is.na(personOfColor),"white",personOfColor))


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






