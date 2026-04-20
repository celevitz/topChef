library(tidyverse)
directory <- "/Users/carlylevitz/Documents/Data/topChef/"

# Bring in data
challengedescriptions <- read.csv(paste0(directory
                                ,"Top Chef - Challenge descriptions.csv"))

# which challenges to keep? The first 5.
# just elimination - not QF Elim or SDQ
  challs <- challengedescriptions %>%
    select(season,seasonNumber,series,episode,challengeType,outcomeType
           ,shopTime,prepTime,cookTime) %>%
    filter(challengeType %in% "Elimination")

  challs$firstep = min(challs$episode,na.rm=T)
  challs$secondep[challs$ep > challs$firstep] = min(challs$episode[challs$ep >
                                                       challs$firstep],na.rm=T)
  challs$thirdep[challs$ep > challs$secondep] = min(challs$episode[challs$ep >
                                                     challs$secondep],na.rm=T)
  challs$fourthep[challs$ep > challs$thirdep] = min(challs$episode[challs$ep >
                                                       challs$thirdep],na.rm=T)
  challs$fifthep[challs$ep > challs$fourthep] = min(challs$episode[challs$ep >
                                                     challs$fourthep],na.rm=T)

  # keep just the relevant elim challs:
  challsfirstfive <- challs %>%
    filter(episode == firstep | episode == secondep | episode == thirdep |
             episode == fourthep | episode == fifthep) %>%
    # keep just the US data
    filter(series == "US") %>%
    select(season,seasonNumber,series,episode,outcomeType,prepTime,cookTime) %>%
    # combine prep and cook time
    # there are some NAs - remove them
    # group it so that the time isn't summed across everything
    group_by(season,seasonNumber,series,episode,outcomeType) %>%
    mutate(prepTime = as.numeric(prepTime)
           ,cookTime = as.numeric(cookTime)
           ,time = sum(prepTime,cookTime,na.rm=T)
           ,hours = time/60) %>%
    # drop the challenges for which I don't have time data
    filter(!(is.na(prepTime)) & !(is.na(cookTime)))

# Average by season
  challsfirstfive %>%
    ungroup() %>% group_by(season,seasonNumber,series) %>%
    summarise(avg = mean(hours,na.rm=T)
              ,total = sum(hours,na.rm=T)
              ,max = max(hours,na.rm=T)
              ,numeps = n()) %>%
    arrange(desc(avg)) %>%
    view()
