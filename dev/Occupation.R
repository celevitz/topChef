#library(topChef)
library(tidyverse)
library(devtools)
library(openxlsx)
library(ggplot2)
#install_github("celevitz/topChef")

#chefs <- topChef::chefdetails

directory <- "/Users/carlylevitz/Documents/Data/"

chefs <- as_tibble(read.xlsx(paste(directory
                                         ,"TopChefData.xlsx",sep=""),sheet=1))
occ_cat <- as_tibble(read.xlsx(paste(directory
                                   ,"TopChefData.xlsx",sep=""),sheet=10))

occupations <- chefs %>%
  #filter(series == "US") %>%
  # wnat to try the new map/crosswalk of occupation & category so we can have
  # multiple optoins for people
  select(name,chef,season,seasonNumber,series,placement,occupation) %>%
  # join on the new occupational category data
  left_join(occ_cat %>% rename(occupation=Occupation))

# make sure everyone has an occupation category
occupations %>%
  filter(is.na(Chef.adjacent) & is.na(Chef) & is.na(Chef.de.Cuisine) &
           is.na(line.cook) & is.na(Sous.chef) & is.na(Pastry.chef) &
           is.na(Teacher) &
           is.na(`Owner,.founder,.proprietor,.partner,.restaurateur`) &
           is.na(Consultant) & is.na(Private.chef) & is.na(Caterer) &
           is.na(`Director,.President,.VP`) & is.na(Executive.chef)) %>%
  select(occupation) %>%
  arrange(occupation) %>%
  distinct()


# occupations
occupationsovertime <- occupations %>%
  group_by(series,seasonNumber) %>%
  mutate(id=paste0(name,seasonNumber,series)
         ,chefsperseason=n()) %>%
  select(!c(name,chef,season,placement,occupation)) %>%
  pivot_longer(!c(id,seasonNumber,series,chefsperseason),names_to="occupationcategory",values_to="value") %>%
  filter(!(is.na(value))) %>%
  group_by(series,seasonNumber,chefsperseason,occupationcategory) %>%
  summarise(n=n()) %>%
  mutate(percent=n/chefsperseason)


occupationsovertime %>%
  filter(series == "US") %>%
  ggplot(aes(x=seasonNumber,y=percent,color=occupationcategory
             ,shape=occupationcategory)) +
  geom_point() +
  geom_line() +
  scale_shape_manual(values=seq(1:13))

## consolidate even more categories
consolidated <- occupationsovertime %>%
  mutate(occupationcategory = ifelse(occupationcategory %in% c(
    "Chef.adjacent","Pastry.chef","Teacher"),"Other"
    ,occupationcategory)) %>%
  group_by(series,seasonNumber,chefsperseason,occupationcategory) %>%
  summarise(n=sum(n),percent=sum(percent))

consolidated %>%
  filter(series == "US") %>%
  ggplot(aes(x=seasonNumber,y=percent,color=occupationcategory
             ,shape=occupationcategory)) +
  geom_point() +
  geom_line() +
  scale_shape_manual(values=seq(1:11))

