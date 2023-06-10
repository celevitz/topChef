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
                                              ,winnercategory == 2 ~ "Non-winning chef in finale"
                                              ,winnercategory == 3 ~ "Chef not in finale") ) %>%
        # get the % wins by category
        mutate(percentwon = numberofwinsbycategory/numberofchalls
              ,cheflabels = ifelse(winnercategory == 1,paste0(round(percentwon*100,1),"% of ",numberofchalls),NA))

      # order things by the % of elimination challenges won by winner
      allchallengeswonbyfinal3$szn <- factor(allchallengeswonbyfinal3$szn,
                                             levels=unique(allchallengeswonbyfinal3$szn[order(allchallengeswonbyfinal3$winnercategory,allchallengeswonbyfinal3$percentwon)]))


##############################################################################
## Visualizations
  # set up
      bkg_col <- "lightcyan"
      legend_bkgcol <- "gray90"
      title_col <- subtitle_col <- caption_col <- text_col <- "darkslategray"

      histogramtitle <- str_glue("Distribution of percent of elimination <br> challenges won prior to finale")
      histogramsubtitle <- str_glue("Denominator for each chef is number of elimination <br> challenges participated in")

      percentwontitle <- str_glue("Percent of all elimination<br>challenges won by season<br>winner prior to finale")

      facettitle <- str_glue("Elimination challenge win percent of the Final 3 chefs")
      facetsubtitle <- str_glue("If there was a chef with a winning percentage of 30% or greater prior to the finale, did they win?")
      facetcaption <- str_glue("A dominant chef is one with at least a 30% elimination challenge win percentage prior to the finale. The finale challenge is excluded.")

  # Histogram of all win %s
  graph1 <-
    results %>%
    ggplot(aes(x=winpercent,group=final3,color=final3,fill=final3)) +
    geom_histogram(alpha=.5) +
    xlab("Elimination challenge win percent prior to finale") +
    ylab("Number of chefs") +
    scale_x_continuous(lim=c(-.05,.8),breaks=seq(0,.8,.2),labels=c("0%","20%","40%","60%","80%")) +
    scale_fill_manual(values=c("#ffbc69","#fc7d0b"))+
    scale_color_manual(values=c("#ffbc69","#fc7d0b"))+
    labs(title=histogramtitle
         ,subtitle=histogramsubtitle) +
    guides(fill=guide_legend(title="Placement"),color=guide_legend(title="Placement")) +
    theme_minimal() +
    theme(legend.position    = c(.8,.8)
          ,panel.grid        = element_blank()
          ,panel.background  = element_rect(fill=bkg_col,color=bkg_col)
          ,plot.margin       = margin(t=10,r=10,b=10,l=10)
          ,plot.background   = element_rect(fill=bkg_col,color=bkg_col)
          ,plot.title        = element_markdown(
                    size    = 24
                    ,face   = "bold"
                    ,color  = title_col
                    ,margin = margin(t=10,b=5))
          ,plot.subtitle     = element_markdown(
                    size    = 20
                    ,color  = subtitle_col
                    ,margin = margin(t=5,b=10))
          ,plot.caption      = element_markdown(
                    size    = 15
                    ,color  = caption_col
                    ,hjust  = .5
                    ,halign = .5)
          ,axis.line  = element_line(color=title_col)
          ,axis.ticks = element_line(color=title_col)
          ,axis.text = element_markdown(size=12,color=text_col)
          ,axis.title = element_markdown(size=15,color=text_col))

  # % of season's elimination challenges that the winner and other final 3 won

    graph2 <-
      allchallengeswonbyfinal3 %>%
      ggplot(aes(x=percentwon,y=szn
                 ,fill=winnercategorylabs,color=winnercategorylabs
                 ,label=cheflabels)) +
      geom_bar(stat="identity") +
      geom_text(aes(x=0.01,y=szn),color="white",hjust=0) +
      labs(title=percentwontitle) +
      xlab("Percent of elimination challenges won") +
      ylab("") +
      scale_x_continuous(lim=c(0,1),breaks=seq(0,1,.2),labels=c("0%","20%","40%","60%","80%","100%")) +
      scale_fill_manual(values=c(bkg_col,"#fc7d0b","#c85200"))+
      scale_color_manual(values=c(rep(legend_bkgcol,3)))+
      labs(title=percentwontitle) +
      guides(fill=guide_legend(title="Placement"),color=guide_legend(title="Placement")) +
      theme_minimal() +
      theme(legend.position    = c(.8,.15)
            ,legend.background = element_rect(fill=legend_bkgcol,color=legend_bkgcol)
            ,legend.text       = element_text(color=caption_col)
            ,legend.title      = element_text(color=caption_col)
            ,panel.grid        = element_blank()
            ,panel.background  = element_rect(fill=bkg_col,color=bkg_col)
            ,plot.margin       = margin(t=10,r=10,b=10,l=10)
            ,plot.background   = element_rect(fill=bkg_col,color=bkg_col)
            ,plot.title        = element_markdown(
              size    = 24
              ,face   = "bold"
              ,color  = title_col
              ,margin = margin(t=10,b=5))
            ,plot.subtitle     = element_markdown(
              size    = 20
              ,color  = subtitle_col
              ,margin = margin(t=5,b=10))
            ,plot.caption      = element_markdown(
              size    = 15
              ,color  = caption_col
              ,hjust  = .5
              ,halign = .5)
            ,axis.line.x  = element_line(color=title_col)
            ,axis.ticks.x = element_line(color=title_col)
            ,axis.text = element_markdown(size=12,color=text_col)
            ,axis.title = element_markdown(size=15,color=text_col))

  # graphs of just the final 3s
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






ggarrange(ggarrange(graph1,graph2,nrow=2,ncol=1),graph3,nrow=1,ncol=2,widths=c(1,2))
dev.print(png, file = paste(savedirectory,"TopThreeWinPercent.png",sep=""), width =1600, height = 900)
dev.off()






