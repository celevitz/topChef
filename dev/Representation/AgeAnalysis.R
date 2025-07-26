# Carly Levitz
# April 22, 2025
# Do an analysis of age, age of winners, placement vs age

rm(list=ls())
library(tidyverse)

directory <- "/Users/carlylevitz/Documents/Data/topChef/"
chefdetails <- read.csv(paste0(directory,"Top Chef - Chef details.csv")) %>%
  filter(series == "US" )



# I only have consistent data for seasons prior to season 14
before14 <- chefdetails %>%
  filter(seasonNumber < 14) %>%
  mutate(placement=as.numeric(placement))


## Averages
  # Overall
    chefdetails %>%
      filter(seasonNumber < 14) %>%
      summarise(age=mean(age,na.rm=T))
  # Exclude people who were eliminated in qualifiers
    before14 %>%
      filter(!(is.na(placement))) %>%
      summarise(age=mean(age,na.rm=T))
  # By gender
    before14 %>%
      group_by(gender) %>%
      summarise(age=mean(age,na.rm=T))

    t.test(before14$age[before14$gender == "Female"]
           ,before14$age[before14$gender == "Male"])

  # By occupation category
    # Actually, not going to do this because I'm missing data for seasons 4-6
    # before14 %>%
    #   group_by(occupation_category) %>%
    #   summarise(age=mean(age,na.rm=T)
    #             ,n=n())

  # By race/ethnicity
    before14 %>%
      group_by(personOfColor) %>%
      summarise(age=mean(age,na.rm=T))

    t.test(before14$age[before14$personOfColor == "POC"]
           ,before14$age[is.na(before14$personOfColor)])

  # How many chefs were over 40? 50? Under 25?
    nrow(before14[before14$age <25,])
    nrow(before14[before14$age > 40 & before14$age <=50,])
    nrow(before14[before14$age > 50,])


  # How many seasons have a chef 41-50? Nine
    before14 %>%
      filter(age > 40 & age <= 50) %>%
      select(season,seasonNumber) %>%
      distinct()

  # How many seasons have a chef over 50? Four.
    before14 %>%
      filter(age > 50) %>%
      select(season,seasonNumber) %>%
      distinct()

  # How many seasons have a chef Under 25? Five.
    before14 %>%
      filter(age < 25) %>%
      select(season,seasonNumber) %>%
      distinct()


# relationship between placement and age
# need to remove the NAs to get the correlation
    temp <- before14 %>% filter(!(is.na(placement)))

    cor(temp$placement,temp$age)
    cor.test(temp$placement,temp$age)
    before14 %>%
      ggplot(aes(x=placement,y=age)) +
      geom_jitter()

## Distribution of age by season
    before14 %>%
      ggplot(aes(x=seasonNumber,y=age)) +
      geom_boxplot(aes(group=seasonNumber))

## Get the average age of the cast + age of winner
summarybefore14 <- before14 %>%
  group_by(seasonNumber) %>%
  summarise(age=mean(age,na.rm=T)) %>%
  left_join(
    before14 %>%
      filter(placement == 1) %>%
      select(seasonNumber,age) %>%
      rename(winnerage=age)
  ) %>%
  mutate(winnercomparedtoaverage = case_when(
                            winnerage<age~"Winner younger than season average"
                            ,winnerage>age~"Winner older than season average"
                            ,TRUE ~"Winner same age as season average"))


lm(before14$age ~ before14$seasonNumber)

summarybefore14 %>%
  ggplot(aes(x=seasonNumber,y=age
             ,fill=winnercomparedtoaverage,color=winnercomparedtoaverage)) +
  geom_point(shape=20) +
  geom_point(aes(x=seasonNumber,y=winnerage),shape = 23,size=3) +
  geom_rect(mapping=aes(xmin=seasonNumber,xmax=seasonNumber
                        ,ymin=age,ymax=winnerage))

## Placement of oldest person on cast
before14 %>%
  # keep just the people who actually placed
  # (i.e., remove those elim'd in qualifiers)
  filter(!(is.na(placement))) %>%
  group_by(seasonNumber) %>%
  mutate(maxage=max(age,na.rm=T)
         ,maxplacement = max(placement)) %>%
  filter(age == maxage) %>%
  select(name,age,season,seasonNumber,placement,maxplacement) %>%
  mutate(percent = placement/maxplacement)

# Youngest person on cast
before14 %>%
  # keep just the people who actually placed
  # (i.e., remove those elim'd in qualifiers)
  filter(!(is.na(placement))) %>%
  group_by(seasonNumber) %>%
  mutate(minage=min(age,na.rm=T)) %>%
  filter(age == minage) %>%
  select(name,age,season,seasonNumber,placement)


