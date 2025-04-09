library(topChef)
library(tidyverse)
library(devtools)
directory <- "/Users/carlylevitz/Documents/Data/topChef/"
# Exclude withdrawals, disqualifications and the "outs" from the finale

# Bring in data
challenges <- read.csv(paste0(directory,"Top Chef - Challenge wins.csv"))
challengedescriptions <- read.csv(paste0(directory
                                   ,"Top Chef - Challenge descriptions.csv"))
numchefsinepisode <- challenges %>%
  filter(inCompetition == TRUE) %>%
  select(season,seasonNumber,series,episode,chef) %>%
  distinct() %>%
  group_by(season,seasonNumber,series,episode) %>%
  summarise(nchefs = n())

# Flag the episodes in which more than one person was eliminated, but could be across diff challs
episodeswithmorethan1personeliminated <- challenges %>%
  filter(outcome %in% c("OUT")) %>%
  group_by(series,season,seasonNumber,episode) %>%
  summarise(flagformorethanonepersoneliminated=n()) %>%
  filter(flagformorethanonepersoneliminated>1 & series == "US") %>%
  ungroup() %>%
  select(!c(flagformorethanonepersoneliminated,series)) %>%
  arrange(seasonNumber,episode) %>%
  rename(episodewithmultipleeliminations=episode)

# Flag the episodes in which more than one person was eliminated in the SAME challenge
episodeswithDE <- challenges %>%
  filter(outcome %in% c("OUT")) %>%
  group_by(series,season,seasonNumber,episode,challengeType) %>%
  summarise(flagformorethanonepersoneliminatedinchall=n()) %>%
  ungroup() %>%
  filter(flagformorethanonepersoneliminatedinchall>1 & series == "US") %>%
  select(!c(flagformorethanonepersoneliminatedinchall,series)) %>%
  arrange(seasonNumber,episode) %>%
  left_join(numchefsinepisode %>%
              filter(series == "US") %>%
              select(!series)) %>%
  rename(episodewithDE=episode)

## Summary stats
  # With more than 1 person eliminated in an episode
    # number of episodes
    nrow(episodeswithmorethan1personeliminated)
    # number of seasons
    nrow(episodeswithmorethan1personeliminated %>%
           select(seasonNumber) %>%
           distinct())
    # number of seasons with different #s of episodes
    episodeswithmorethan1personeliminated %>%
      group_by(season,seasonNumber) %>%
      summarise(numberofepisodes = n()) %>%
      arrange(seasonNumber)

    episodeswithmorethan1personeliminated %>%
      group_by(season,seasonNumber) %>%
      summarise(numberofepisodes = n()) %>%
      ungroup() %>%
      group_by(numberofepisodes) %>%
      summarise(numberofseasonswiththisnumberofepswithmorethan1elim=n())
    print("and you'll need to add in the ones with no DEs")

  # With more than 1 person eliminated in a given challenge
    nrow(episodeswithDE)
    # number of seasons
    nrow(episodeswithDE %>%
           select(seasonNumber) %>%
           distinct())
    # number of seasons with different #s of episodes
    episodeswithDE %>%
      group_by(season,seasonNumber) %>%
      summarise(numberofepisodes = n()) %>%
      arrange(seasonNumber)

    episodeswithDE %>%
      group_by(season,seasonNumber) %>%
      summarise(numberofepisodes = n()) %>%
      ungroup() %>%
      group_by(numberofepisodes) %>%
      summarise(numberofseasonswiththisnumberofepsofDEs=n())
    print("and you'll need to add in the ones with no DEs")

  # Team or individual challenges
    episodeswithDE %>%
      left_join(challengedescriptions %>%
              filter(series == "US") %>%
              select(season,seasonNumber,episode,challengeType,outcomeType) %>%
              rename(episodewithDE = episode) %>%
              distinct()
                ) %>%
      group_by(outcomeType) %>%
      summarise(n=n())

  # How many people are in the competition at time of DE?
    episodeswithDE %>%
      ungroup() %>%
      group_by(nchefs) %>%
      summarise(ntimes=n())

    summary(episodeswithDE$nchefs)

## I want to see how strong the chefs were who were eliminated in DEs --
## Looking at JUST double eliminations, not when two people were eliminated in
    #the same episode
## Maybe look @ the difference between their scores at the point of elimination?
  ## Would need to flag which chefs were in the double elimination
  ## For each chef, how many Elims and QFs have they taken part of?
  ## Get the score for them

    # Who was eliminated in these episodes?
    eliminatedchefs <- episodeswithDE %>%
      left_join(challenges %>%
              filter(outcome == "OUT" & series == "US") %>%
              select(season,seasonNumber,episode,challengeType,chef,outcome) %>%
              rename(episodewithDE=episode))

    ## at the given number of eliminations, how many of the other types
    ## of challenges had occurred? We'll use that to calcualte the score
      challengetypesforscore <- episodeswithDE %>%
        select(!challengeType) %>%
        full_join(challenges %>%
                    filter(series == "US") %>%
                    select(season,seasonNumber,episode,challengeType) %>%
                    distinct()
                # to account for the seasons with more than one double
                # elimination, need to set this to a many-to-many relationship
                  ,relationship="many-to-many") %>%
        ## Drop the episodes that are after the elimination
        ## Drop the qualifying hchallenges
        filter(episode <= episodewithDE) %>%
        filter(challengeType != "Qualifying Challenge") %>%
        ## simplify the challenge type so that the index can be used
        mutate(challengeType=ifelse(challengeType %in%
                                      c("Quickfire Elimination"
                                        ,"Sudden Death Quickfire"), "Elimination"
                                    ,challengeType)) %>%
        # get the number of challenges by type for each of the the DEs
        # include episodewithDE as a grouping variable because of the seasons
        # with more than one DE per season
        group_by(season,seasonNumber,episodewithDE,challengeType) %>%
        summarise(n=n()) %>%
        # pivot it so that each row is just one DE episode
        pivot_wider(names_from=challengeType,values_from=n)

   ## Get these chefs' scores at this point
      ## holding data
      holding <- data.frame(chef=character(),
                             season=character(),
                             seasonNumber=integer(),
                             indexWeight=integer(),
                             scorerank=integer()
                            ,numchefs=integer())
      ## scores
      for (sn in unique(challengetypesforscore$seasonNumber)) {
        for (epwithde in unique(challengetypesforscore$episodewithDE[
                                  challengetypesforscore$seasonNumber == sn])) {
          for (c in unique(eliminatedchefs$chef[eliminatedchefs$seasonNumber==sn
                                & eliminatedchefs$episodewithDE == epwithde])) {
                numberofelimchalls <- challengetypesforscore$Elimination[
                              challengetypesforscore$seasonNumber == sn &
                              challengetypesforscore$episodewithDE == epwithde]
                numberofquickfires <- challengetypesforscore$Quickfire[
                              challengetypesforscore$seasonNumber == sn &
                              challengetypesforscore$episodewithDE == epwithde]

                ## Which chefs were in the competition at that time?
                numchefsattime <- challenges %>%
                  filter(series == "US" & episode == epwithde &
                           seasonNumber == sn & inCompetition == TRUE) %>%
                  select(chef) %>% distinct()

                ## what's the ranked order of the chefs?
                ## keep just the relevant chef
                temp <- weightedindex("US",sn,numberofelimchalls
                                      ,numberofquickfires) %>%
                        # want to see the scores prior to people's eliminations
                        mutate(indexWeight = ifelse(Elimination.OUT == 0
                                            ,indexWeight
                                            ,indexWeight+(Elimination.OUT*7))) %>%
                        arrange(desc(indexWeight)) %>%
                        # who was still in the competition?
                        right_join(numchefsattime)

                temp <- temp %>%
                  mutate(scorerank = as.numeric(row.names(temp))
                         ,numchefs=n()) %>%
                        filter(chef == c) %>%
                        select(chef,season,seasonNumber,indexWeight
                               ,scorerank,numchefs)

                ## add onto holding dataset
                holding <- holding %>%
                  bind_rows(temp)

             } # close chef loop
        } # close DE episode loop
      } # close season loop

      indexscoresofeliminatedchefs <- holding

  # Bring the data together
      indexscoresofeliminatedchefs <- indexscoresofeliminatedchefs %>%
      # then, bring back on the info of when they were eliminated to account
      # for seasons w/ multiple DEs
        left_join(eliminatedchefs %>% select(!c(challengeType,outcome))) %>%
      ## what is the minimum and maximum score for each DE?
      ##And then what's the difference?
        group_by(season,seasonNumber,episodewithDE) %>%
        mutate(worsescore=min(indexWeight,na.rm=T)
               ,betterscore=max(indexWeight,na.rm=T)
               ,difference = betterscore-worsescore
               ,betterrank=min(scorerank,na.rm=T)
               ,worserank=max(scorerank,na.rm=T)
               ,differenceinrank=worserank-betterrank
               # create flag for the chefs that are better/worse
               ,status = ifelse(worsescore==indexWeight
                                ,"worse","better"))

      summaryofscorediff <- indexscoresofeliminatedchefs %>%
        select(!c(indexWeight,scorerank)) %>%
        pivot_wider(names_from=status,values_from=chef) %>%
        distinct() %>%
        # Add on notes that help with interpretation
        mutate(note = case_when(season == "California" & episodewithDE == 14 ~ "Second to last episode"
               ,season == "Los Angeles" & episodewithDE == 12 ~ "Second to last episode"
               ,season == "New York" & episodewithDE == 13 ~  "Second to last episode"
               ,season == "New Orleans" & episodewithDE == 16 ~ "Second to last episode"
               ,TRUE ~ "-")
               # add context to rank
               ,betterrankpercent = (betterrank-1)/nchefs
               ,worserankpercent = (worserank-1)/nchefs)
      dataforexport <- summaryofscorediff
        dataforexport <- dataforexport[,c("season","seasonNumber"
                        ,"episodewithDE","numchefs"
                        ,"worse","worsescore","worserank","worserankpercent"
                        ,"better","betterscore","betterrank","betterrankpercent"
                        ,"difference","differenceinrank"
                        ,"note")]
      write.csv(dataforexport,paste0(directory,"DoubleEliminations.csv")
                                          ,row.names=F)

  # what are the key takeaways
      summaryofscorediff %>%
        arrange(note,episodewithDE,seasonNumber) %>%
        print(n=21)

      summary(summaryofscorediff$difference)
      summary(summaryofscorediff$difference[summaryofscorediff$note == "-"])
      summary(summaryofscorediff$differenceinrank)
      summary(summaryofscorediff$differenceinrank[summaryofscorediff$note == "-"])

      summaryofscorediff %>%
        filter(betterrank < 3)

      summaryofscorediff %>%
        group_by(note) %>%
        summarise(worsescore=mean(worsescore)
                  ,betterscore=mean(betterscore)
                  ,difference=mean(difference)
                  ,betterrank = mean(betterrank)
                  ,worserank = mean(worserank)
                  ,betterrankpercent=mean(betterrankpercent)
                  ,worserankpercent=mean(worserankpercent)
                  ,differenceinrank = mean(differenceinrank))












