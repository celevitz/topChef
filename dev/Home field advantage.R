## Seeing extent of home field advantage

rm(list=ls())

library(tidyverse)
directory <- "/Users/carlylevitz/Documents/Data/topChef/"

# Bring in data
chefdetails_raw <- read.csv(paste0(directory,"Top Chef - Chef details.csv"))
chefdetails <- chefdetails_raw %>%
  filter(series == "US") %>%
  group_by(name) %>%
  mutate(homestate = case_when(season %in% c("All Stars: New York","New York") & state == "New York" ~ "Homestate"
                               ,season %in% c("All-Stars L.A.","California","Los Angeles","San Francisco") & state == "California" ~ "Homestate"
                               ,season %in% c("Boston") & state == "Massachusetts" ~ "Homestate"
                               ,season %in% c("Charleston") & state == "South Carolina" ~ "Homestate"
                               ,season %in% c("Chicago") & state == "Illinois" ~ "Homestate"
                               ,season %in% c("Colorado") & state == "Colorado" ~ "Homestate"
                               ,season %in% c("D.C.") & state == "D.C." ~ "Homestate"
                               ,season %in% c("Houston","Texas") & state == "Texas" ~ "Homestate"
                               ,season %in% c("Kentucky") & state == "Kentucky" ~ "Homestate"
                               ,season %in% c("Las Vegas") & state == "Nevada" ~ "Homestate"
                               ,season %in% c("Miami") & state == "Florida" ~ "Homestate"
                               ,season %in% c("New Orleans") & state == "Louisiana" ~ "Homestate"
                               ,season %in% c("Portland") & state == "Oregon" ~ "Homestate"
                               ,season %in% c("Seattle") & state == "Washington" ~ "Homestate"
                               ,season %in% c("Wisconsin") & state == "Wisconsin" ~ "Homestate"
                               ,season %in% c("World All Stars") & state == "England" ~ "Homestate"
                               ,TRUE ~ "Out of state"
                               )
         ,winner = ifelse(placement==1,"winner","non-winner")
         ,topfive = ifelse(placement<=5,"top 5","worse than top 5")
         ,numberofseasonsin = n()
         ,multipleseasons = ifelse(numberofseasonsin >1,"more than 1 season","1 season"))

#chefdetails %>% filter(homestate == "Homestate") %>% select(season,chef)
table(chefdetails$homestate)

## All seasons:
  winnerchisq <- chisq.test(chefdetails$winner,chefdetails$homestate)
  winnerchisq
  winnerchisq$observed
  winnerchisq$expected

  topfivechisq <- chisq.test(chefdetails$topfive,chefdetails$homestate)
  topfivechisq
  topfivechisq$observed
  topfivechisq$expected

  # placementreg <- lm(chefdetails$placement ~ chefdetails$homestate)
  # summary(placementreg)

  t.test(chefdetails$placement[chefdetails$homestate=="Homestate"]
       ,chefdetails$placement[chefdetails$homestate=="Out of state"]
       ,alternative="less")

# Just newbie seasons
  newbies <- chefdetails %>% filter(!(seasonNumber %in% c(8,10,13,14,15,16,17,20)))
  winnerchisq <- chisq.test(newbies$winner,newbies$homestate)
  winnerchisq

  topfivechisq <- chisq.test(newbies$topfive,newbies$homestate)
  topfivechisq

  t.test(chefdetails$placement[chefdetails$homestate=="Homestate" & !(chefdetails$seasonNumber %in% c(8,10,13,14,15,16,17,20))]
         ,chefdetails$placement[chefdetails$homestate=="Out of state" & !(chefdetails$seasonNumber %in% c(8,10,13,14,15,16,17,20))]
         ,alternative="less")

# Seasons with both newbies and vets: do the vets do better?
  combo <- chefdetails %>% filter(seasonNumber %in% c(10,13,14,15,16,20))
  winnerchisq <- chisq.test(combo$winner,combo$homestate)
  winnerchisq
  winnerchisq$observed

  topfivechisq <- chisq.test(combo$topfive,combo$homestate)
  topfivechisq
  topfivechisq$observed

  t.test(chefdetails$placement[chefdetails$homestate=="Homestate" & chefdetails$seasonNumber %in% c(10,13,14,15,16,20)]
         ,chefdetails$placement[chefdetails$homestate=="Out of state" & chefdetails$seasonNumber %in% c(10,13,14,15,16,20)]
         ,alternative="less")

  # Charleston only
  t.test(chefdetails$placement[chefdetails$homestate=="Homestate" & chefdetails$seasonNumber %in% c(14)]
         ,chefdetails$placement[chefdetails$homestate=="Out of state" & chefdetails$seasonNumber %in% c(14)]
         ,alternative="less")

#########################################
## Ratio of homestate to out of state
  # beginning
  table(chefdetails$homestate)

  # halfway
    # mark the halfway point of each season
    #drop the chefs with a placement worse than that
    # compare the ratio of homestate to out of state
    chefdetails %>%
      select(series,season,seasonNumber,name,homestate,placement) %>%
      group_by(series,season,seasonNumber) %>%
      mutate(maxplacement=max(placement)
             ,halfway = maxplacement/2) %>%
      filter(placement < halfway) %>%
      ungroup() %>%
      group_by(homestate) %>%
      summarise(n=n())

  # top five
    # mark the halfway point of each season
    #drop the chefs with a placement worse than that
    # compare the ratio of homestate to out of state
    chefdetails %>%
      filter(placement < 6) %>%
      ungroup() %>%
      group_by(homestate) %>%
      summarise(n=n())


  # winners
    # mark the halfway point of each season
    #drop the chefs with a placement worse than that
    # compare the ratio of homestate to out of state
    chefdetails %>%
      filter(placement ==1) %>%
      ungroup() %>%
      group_by(homestate) %>%
      summarise(n=n())




