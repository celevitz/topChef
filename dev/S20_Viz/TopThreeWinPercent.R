## Top Three - win percents
## written: 2023-06-10
## updated: 2023-06-10
## Carly Levitz

savedirectory <- "/Users/carlylevitz/Documents/Data/TCSeason20/Episode-agnostic/"

library(topChef); library(tidyverse); library(ggtext); library(ggpubr)

##############################################################################
## Get just the data that we are interested in
dataofinterest <- topChef::challengewins %>%
  # exclude elimination challenges without winners
  filter(!(episode == 9 & sznnumber == 13)) %>%
  # exclude finale
  group_by(szn,sznnumber,series) %>%
  mutate(maxepi = case_when(challenge_type == "Elimination" ~ max(episode)
                            ,TRUE ~ NA) ) %>%
  filter(episode != maxepi) %>%
  # how many elimination challenges did they participate in
  filter(in.competition == "TRUE" & challenge_type == "Elimination" & series == "US") %>%
  ungroup()

##############################################################################
## Data set up
  ## Win percentages for just the elimination challenges prior the finale
    results <- dataofinterest %>%
        group_by(szn,sznnumber,chef) %>%
        # how many wins did they have
        mutate(numberofepisodes=n()
               ,won=ifelse(grepl("WIN",outcome),1,0)
               ,numberofwins = sum(won)) %>%
        # keep just relevant variables
        select(szn,sznnumber,series,chef,numberofepisodes,numberofwins) %>%
        distinct() %>%
        # win %
        mutate(winpercent = numberofwins/numberofepisodes) %>%
        # call out just the final 3
        # doing a left join because we've already filtered out all but US main seasons
        left_join(topChef::chefdetails %>% select(series,sznnumber,szn,chef,placement)) %>%
        mutate(final3 = ifelse(placement<=3,"Final 3","All others")
               # want to organize the data for the visualizaiton
               ,placementmodified =as.character(placement)
               ,placementmodified = case_when(placementmodified == "1" ~ "1st"
                                              ,placementmodified == "2" ~ "2nd (a)"
                                              ,placementmodified == "3" ~ "3rd (a)"
                                              ,TRUE ~ placementmodified)
               ,placementmodified = case_when(
                                              #chef %in% c("Casey T.","Lisa F.","Carla H.","Bryan V.","Angelo S.","Sara B.","Dawn B.","Evelyn G.") & placement == 2~ "2nd (a)"
                                              chef %in% c("Dale L.","Richard B.","Stefan R.","Kevin G.","Ed C.","Stephanie C.","Shota N.","Sarah W.","Gabriel Rodriguez") & placement == 2 ~ "2nd (b)"
                                              #,chef %in% c("Elia A.") & placement == 3 ~ "3rd (a)"
                                              ,chef %in% c("Sam T.") & placement == 3 ~ "3rd (b)"
                                              ,TRUE ~ placementmodified ) ) %>%
        # was there a final 3 person who had a win % of more than 30%?
        # and did the dominant chef end up winning?
        ungroup() %>% group_by(series,szn,sznnumber) %>%
        mutate(morethan30 = ifelse(winpercent >= .3 & final3 == "Final 3",1,0)
               ,seasonmorethan30 = max(morethan30)
               ,chefmorethan30 = ifelse(winpercent >= .3 & placement == 1,1,0)
               ,dominantchefwon = max(chefmorethan30)
               ,dominantchef = case_when(seasonmorethan30 == 1 & dominantchefwon == 1~"Dominant chef won"
                                         ,seasonmorethan30 == 1 & dominantchefwon == 0~"Dominant chef lost"
                                         ,TRUE ~ "No dominant chef")) %>%
        select(!c(morethan30,seasonmorethan30))

  ## who had the highest win % in that season who was in at least 4 elim challs?
      results %>%
        filter(numberofepisodes >= 6) %>%
        ungroup() %>% group_by(series,szn,sznnumber) %>%
        filter(winpercent == max(winpercent)) %>%
        print(n=30)

  ## What percent of all elimination challenges did the winner and other Final 3 chefs win?
  ## To account for group challenges: if the winner was on the team, then it counts as the winner winning it
  ## if the winner wasn't on the team but another final 3 person was, then it counts as a final three person winning
      allchallengeswonbyfinal3 <- dataofinterest %>%
        # how many elimination challenges were there in a season where somebody won?
        filter(grepl("WIN",outcome)) %>%
        group_by(szn,sznnumber,series) %>%
        mutate(numberofchalls = n_distinct(episode)) %>%
        # doing a left join because we've already filtered out all but US main seasons
        left_join(topChef::chefdetails %>% select(series,sznnumber,szn,chef,placement)) %>%
        mutate(final3 = case_when(placement == 1 ~ 1
                                  ,placement %in% c(2,3) ~2
                                  ,TRUE ~ 3) ) %>%
        # who was the winner of each challenge/ was the winner in the final 3?
        ungroup() %>% group_by(szn,sznnumber,series,episode) %>%
        mutate(winnercategory = min(final3)) %>%
        ungroup() %>% group_by(szn,sznnumber,series,winnercategory) %>%
        mutate(numberofwinsbycategory = n_distinct(episode)) %>%
        select(szn,sznnumber,series,numberofchalls,winnercategory,numberofwinsbycategory) %>%
        distinct() %>%
        mutate(winnercategorylabs = case_when(winnercategory == 1 ~ "Winner"
                                              ,winnercategory == 2 ~ "Final 2 or 3"
                                              ,winnercategory == 3 ~ "Other chef not in finale") ) %>%
        # get the % wins by category
        mutate(percentwon = numberofwinsbycategory/numberofchalls)

      # order things by the % of elimination challenges won by winner
      allchallengeswonbyfinal3$szn <- factor(allchallengeswonbyfinal3$szn,
                                             levels=unique(allchallengeswonbyfinal3$szn[order(allchallengeswonbyfinal3$winnercategory,allchallengeswonbyfinal3$percentwon)]))


##############################################################################
## Visualizations
  # Histogram of all win %s
  graph1 <-
  results %>%
    ggplot(aes(x=winpercent,group=final3,color=final3)) +
    geom_histogram() +
    xlab("Elimination challenge win percent prior to finale") +
    ylab("Number of chefs")

  # % of season's elimination challenges that the winner and other final 3 won
    percentwontitle <- str_glue("Percent of elimination challenges won by season winner")
    graph2 <-
      allchallengeswonbyfinal3 %>%
        ggplot(aes(x=percentwon,y=szn,fill=winnercategorylabs)) +
        geom_bar(stat="identity") +
        labs(title=percentwontitle) +
        xlab("Percent of elimination challenges won")

  # graphs of just the final 3s
    facettitle <- str_glue("Elimination challenge win percent of the Final 3 chefs")
    facetsubtitle <- str_glue("If there was a chef with a winning percentage of 30% or greater prior to the finale, did they win?")
    facetcaption <- str_glue("A dominant chef is one with at least a 30% elimination challenge win percentage prior to the finale. The finale challenge is excluded.")

    graph3 <-
    results %>%
      filter(final3 == "Final 3") %>%
      ggplot(aes(x=placementmodified,y=winpercent,label=chef,col = dominantchef)) +
      facet_wrap(~sznnumber) +
      geom_bar(stat="identity") +
      geom_text() +
      labs(title = facettitle
           ,subtitle=facetsubtitle
           ,caption = facetcaption) +
      xlab("Placement") +
      ylab("Elimination challenge win percent prior to finale") +
      scale_y_continuous(lim=c(0,.6),breaks=seq(0,.6,.2),labels=c("0%","20%","40%","60%"))






ggarrange(ggarrange(graph1,graph2,nrow=1,ncol=2),graph3,nrow=2,ncol=1,heights=c(1,2))
dev.print(png, file = paste(savedirectory,"TopThreeWinPercent.png",sep=""), width =900, height = 1600)
dev.off()






