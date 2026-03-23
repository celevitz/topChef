library(topChef)
library(tidyverse)
library(devtools)
#install_github("celevitz/topChef")

rm(list=ls())
directory <- "/Users/carlylevitz/Documents/Data/topChef/"

# Bring in data
chefs <- read.csv(paste0(directory,"Top Chef - Chef details.csv"))
challenges <- read.csv(paste0(directory,"Top Chef - Challenge wins.csv"))

## Keep just the wins
## for now, just going to keep elimination-type challenges
wins <- challenges %>%
  filter(outcome %in% c("WIN","WINNER") &
           series %in% "US" &
           challengeType %in% c("Elimination","Quickfire Elimination"
                                ,"Sudden Death Quickfire") &
           inCompetition %in% "TRUE") %>%
  # keep just relevant variables
  select(seasonNumber,episode,chef) %>%
  arrange(seasonNumber,chef,episode) %>%
  # is there someone who won both elim chall types in 1 episode?
  # I can use summarize b/c I'm using all variables as the grping vars
  # this will consolidate the episodes. I'm counting it as a consecutive win if
  #   they won at least 1 elim chall in that episode. May need to revisit this.
  #   Stefan S5E1, Gregory S12E3, Jeremy S13E13, Carrie S15E9
  group_by(seasonNumber,episode,chef) %>%
  mutate(nwinsinepisode=n()) %>%
  distinct() %>%
  # how many times have the chefs won?
  group_by(seasonNumber,chef) %>%
  mutate(numwins = n())

# Capture chefs with any streak
winstreaks <- wins %>%
  # What's the difference between the episodes?
  bind_cols(twoinarow = sapply(1:dim(wins)[1], function(z){
              (wins$episode[z + 1] - wins$episode[z]) ==1 &
                wins$seasonNumber[z+1] == wins$seasonNumber[z] &
                wins$chef[z+1] == wins$chef[z]    })
            ,threeinarow = sapply(1:dim(wins)[1], function(y){
              (wins$episode[y+2] - wins$episode[y]) ==2 &
                wins$seasonNumber[y+2] == wins$seasonNumber[y] &
                wins$chef[y+2] == wins$chef[y]  })
            ,fourinarow = sapply(1:dim(wins)[1], function(x){
              (wins$episode[x+3] - wins$episode[x]) ==3 &
                wins$seasonNumber[x+3] == wins$seasonNumber[x] &
                wins$chef[x+3] == wins$chef[x]  })
            ,fiveinarow = sapply(1:dim(wins)[1], function(w){
              (wins$episode[w+4] - wins$episode[w]) ==4 &
                wins$seasonNumber[w+4] == wins$seasonNumber[w] &
                wins$chef[w+4] == wins$chef[w]  })
            ) %>%
  # need to filter AFTER I bind columns - cuz there's different numbers of rows
  group_by(seasonNumber,chef) %>%
  mutate(streak2 = max(ifelse(twoinarow == TRUE & !(is.infinite(twoinarow))
                              ,1,0),na.rm=T)
         ,streak3 = max(ifelse(threeinarow == TRUE & is.finite(threeinarow)
                               ,1,0),na.rm=T)
         ,streak4 = max(ifelse(fourinarow == TRUE & is.finite(fourinarow)
                               ,1,0),na.rm=T)
         ,streak5 = max(ifelse(fiveinarow == TRUE & is.finite(fiveinarow)
                               ,1,0),na.rm=T) ) %>%
  # filter out those without any streaks
  filter(streak2 == 1 | streak3 == 1 | streak4 == 1 | streak5 == 1)


# streak of 3 or better
winstreaks %>% filter(streak3 == 1 | streak4 == 1 | streak5 == 1) %>% print(n=50)
# Dale, S4, Eps 6-8. Brooke, S10, Eps 13-15. Gregory S12, eps 2-5. Melissa S 17, eps 11-14. Evelyn S19, eps 5-6.

# how many chefs have had streaks of 2?
winstreaks %>% filter(streak2 == 1) %>%
  select(seasonNumber,chef) %>%
  distinct() %>%
  group_by(chef) %>%
  summarise(n=n()) %>%
  print(n=50)

# Buddha, Gregory, Melissa, Richard, Sheldon have had streaks of 2 in both szns



