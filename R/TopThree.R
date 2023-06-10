## Top Three
## written: 2023-06-08
## updated: 2023-06-10
## Carly Levitz

savedirectory <- "/Users/carlylevitz/Documents/Data/TCSeason20/Episode-agnostic/"

library(topChef); library(tidyverse); library(ggtext)

allseasons <- weightedindex("US",1,13,10)
for (season in seq(2,20,1)) {
  allseasons <- rbind(allseasons,weightedindex("US",season,13,10))

}


allseasons <- allseasons %>%
  # keep just variables of interest
  select(chef,szn,sznnumber,placement,indexWeight) %>%
  # keep just top three / season 20 folks
  filter(placement <=3 | is.na(placement)) %>%
  # put the current season folks at placement of "4th"
  mutate(placement = ifelse(sznnumber == 20,4,placement)) %>%
  # label chefs with their index scores
  mutate(labelname = paste0(chef," (",indexWeight,")")
         # change alignment for some chefs that would plot over each other
         ,alignment = case_when(placement == 1 & chef %in% c("Brooke W.") ~ 1
                                ,placement == 1 & chef %in% c("Michael V.") ~ 0
                                ,placement == 2 & chef %in% c("Sara B.","Bryan V.","Dale L.","Mike I.") ~ 1
                                ,placement == 2 & chef %in% c("Stephanie C.","Tiffani F.","Amar S.","Casey T.","Nina C.") ~ 0
                                ,placement == 3 & chef %in% c("Dave M.","Sheldon S.") & indexWeight != 28~ 1
                                ,placement == 3 & chef %in% c("Isaac T.","Doug A.") ~ 0
                                ,placement == 4 ~ .5
                                ,TRUE ~ .5)
         ,plotxforlabel = case_when(alignment == 1 ~placement-.05
                                    ,alignment == 0 ~placement+.05
                                    ,alignment == .5 ~ placement
                                    ,TRUE ~ placement)
         )

## Scatter plot
  ## Titles and captions
    captionsource <- str_glue("Visualization: @carlylevitz &bull; Source: github.com/celevitz/topChef &bull; Tools: #rstats #ggplot #ggtext #tidyverse")
    scoring <- str_glue("Methodology for weighted index scores: Elimination win = 7 points &bull; Elimination high = 3 points<br>Elimination low = - 3 points &bull; Eliminated = - 7 points &bull; Quickfire win = 4 points <br> Quickfire high = 2 points &bull; Quickfire low = - 2 points<br><br>")
    titletext <- str_glue("Comparing Final Three Chefs Across Seasons")
    subtitletext <- str_glue("Top Chef Weighted Index 13 elimination challenges into season")

    captiontext <- str_glue("{scoring} {captionsource}")

  ## colors
    bkg_col <- "#F9E3D1"
    title_col <- subtitle_col <- caption_col <- text_col <- "darkslategray"

  ## Visual
    allseasons %>%
      ggplot(aes(x=placement,y=indexWeight))  +
      geom_hline(yintercept=0, color="#ffbc69")  +
      labs(title=titletext
           ,subtitle=subtitletext
           ,caption= captiontext) +
      scale_x_continuous(lim=c(.7,4.5), breaks=c(1,2,3,4),labels=c("1st place","2nd place","3rd place","Season 20\nFinal Three")) +
      scale_y_continuous(lim=c(-5,60),breaks=c(-5,seq(0,60,10)),labels=c(-5,seq(0,60,10))) +
      theme_minimal() +
      ylab("Index score") + xlab("") +
      theme(panel.grid         = element_blank()
            ,panel.background  = element_rect(fill=bkg_col,color=bkg_col)

            ,plot.margin       = margin(t=10,r=10,b=10,l=10)
            ,plot.background   = element_rect(fill=bkg_col,color=bkg_col)
            ,plot.title        = element_markdown(
                                    size    = 24
                                    ,face   = "bold"
                                    ,color  = title_col
                                    ,margin = margin(t=10,b=5))
            ,plot.subtitle     = element_markdown(
                                    size    = 24
                                    ,color  = subtitle_col
                                    ,margin = margin(t=5,b=10))
            ,plot.caption      = element_markdown(
                                    size    = 15
                                    ,color  = caption_col
                                    ,hjust  = .5
                                    ,halign = .5)
            ,axis.ticks.y      = element_line(color=text_col)
            ,axis.line.y       = element_line(color=text_col)
            ,axis.line.x       = element_blank()
            ,axis.ticks.x      = element_blank()

            ,axis.text=element_text(size=18,color=text_col)
            ,axis.title.y = element_text(size     = 18
                                         ,color   = text_col
                                         ,margin  = margin(l=15,r=15)
                                         ,angle  = 0)
      ) +
      # label people
      geom_text(aes(x=plotxforlabel,y=indexWeight,label=labelname,hjust=alignment),color=text_col) +
      geom_rect(xmin = 3.8, xmax = 4.2, ymin = 49 , ymax = 51,color=text_col,fill=NA) +
      geom_rect(xmin = 3.8, xmax = 4.2, ymin = 3 , ymax = 5,color=text_col,fill=NA) +
      geom_rect(xmin = 3.6, xmax = 4.4, ymin = -1 , ymax = -3,color=text_col,fill=NA)

    dev.print(png, file = paste(savedirectory,"TopThreeIndexScores.png",sep=""), width = 900, height = 900)
    dev.off()


## Weighted index by episode: cumulative
    # Outcomes of the challenges
      challengewins <- topChef::challengewins[,names(topChef::challengewins)[!(names(topChef::challengewins) %in% "rating")] ]

    # combine types of challenges
      challengewins$challenge_type[challengewins$challenge_type %in% c("Quickfire Elimination","Sudden Death Quickfire")] <- "Elimination"

    # Exclude the uncommon challenge types
      challengewins <- challengewins[!(challengewins$challenge_type %in% c("Battle of the Sous Chefs","Qualifying challenge")),]

    # clean up outcomes: consolidate
      challengewins$outcome[challengewins$outcome %in% c("High","HiGH")] <- "HIGH"
      challengewins$outcome[grepl("LOW",challengewins$outcome)] <- "LOW"
      challengewins$outcome[challengewins$outcome %in% c("DISQUALIFIED","RUNNER-UP","WITHDREW") | grepl("OUT",challengewins$outcome) ] <- "OUT"
      challengewins$outcome[challengewins$outcome %in% c("Didn't compete") | grepl("N/A",challengewins$outcome) | grepl("Qualified",challengewins$outcome) ] <- "IN"
      challengewins$outcome[challengewins$outcome %in% c("WINNER")] <- "WIN"

    # need to sum (total) the counts, because of the combination of challenge types
      challengewins <- challengewins %>%
        filter(in.competition == "TRUE") %>%
        group_by(szn,sznnumber,series,episode,chef,challenge_type,outcome) %>%
        summarise(n=n()) %>%
        # reshape so that we can get the index score
        pivot_wider(names_from=challenge_type,values_from = n) %>%
        pivot_wider(names_from = outcome,values_from = c("Elimination","Quickfire")) %>%
        # Get the score for the episode
        mutate(Quickfire_HIGH = ifelse(is.na(Quickfire_HIGH),0,Quickfire_HIGH*2)
               ,Quickfire_WIN = ifelse(is.na(Quickfire_WIN),0,Quickfire_WIN*4)
               ,Quickfire_LOW = ifelse(is.na(Quickfire_LOW),0,Quickfire_LOW*-2)
               ,Quickfire_IN = ifelse(is.na(Quickfire_IN),0,Quickfire_IN)
               ,Elimination_HIGH = ifelse(is.na(Elimination_HIGH),0,Elimination_HIGH*3)
               ,Elimination_WIN = ifelse(is.na(Elimination_WIN),0,Elimination_WIN*7)
               ,Elimination_LOW = ifelse(is.na(Elimination_LOW),0,Elimination_LOW*-3)
               ,Elimination_IN = ifelse(is.na(Elimination_IN),0,Elimination_IN)
               ,Elimination_OUT = ifelse(is.na(Elimination_OUT),0,Elimination_OUT*-7)) %>%
        mutate(index=Elimination_WIN+
                     Elimination_HIGH+
                     Elimination_LOW+
                     Elimination_OUT+
                     Quickfire_WIN+
                     Quickfire_HIGH+
                     Quickfire_LOW)

    # get cumulative score
      cumulative <- challengewins %>%
        select(szn,sznnumber,series,episode,chef,index) %>%
        # add on the placement
        full_join(topChef::chefdetails %>% select(chef,szn,sznnumber,series,placement)) %>%
        # get the cumulative total
        ungroup() %>% group_by(szn,sznnumber,series,chef) %>%
        arrange(series,sznnumber,episode,chef) %>%
        mutate(indexcumulative = cumsum(index))

    # keep just top three
      cumulativeT3 <- cumulative %>%
        filter((placement <=3 | is.na(placement)) & series == "US") %>%
        mutate(placementstring = case_when(placement == 1 ~ "1st"
                                           ,placement == 2 ~ "2nd"
                                           ,placement == 3 ~ "3rd"
                                           ,is.na(placement) ~ "Current season"
                                           ,TRUE ~ "")) %>%
        # what is the max episode by chef? use this to create labels
        group_by(sznnumber,chef) %>%
        mutate(maxepi = max(episode)
               ,cheflabelall = ifelse(episode == maxepi,paste0(str_split_fixed(chef," ",2)[,1]," (S",sznnumber,")"),NA)
               # make some of the labels be not at the end
               ,cheflabelall = case_when(chef == "Stefan R." & sznnumber == 5 & episode == 11 ~ "Stefan (S5)"
                                         ,chef == "Stefan R." & sznnumber == 5 & episode == 14 ~ NA
                                         ,chef == "Gregory G." & sznnumber == 12 & episode == 5 ~ "Gregory (S12)"
                                         ,chef == "Gregory G." & sznnumber == 12 & episode == 15 ~ NA
                                         ,chef == "Doug A." & sznnumber == 12 & episode == 9 ~ "Doug (S12)"
                                         ,chef == "Doug A." & sznnumber == 12 & episode == 14 ~ NA
                                         ,chef == "Kevin G." & sznnumber == 6 & episode == 14 ~ "Kevin (S6)"
                                         ,chef == "Kevin G." & sznnumber == 6 & episode == 15 ~ NA
                                         ,chef == "Tiffani F." & placement == 2 ~ NA
                                         ,chef == "Carla H." & placement == 2 ~ NA
                                         ,chef == "Amar S." & placement == 2 ~ NA
                                         ,chef == "Shirley C." & placement == 2 ~ NA
                                         ,chef == "Angelo S." & placement == 2 ~ NA
                                         ,chef == "Evelyn G." & placement == 2 ~ NA
                                         ,chef == "Ed C." & placement == 2 ~ NA
                                         ,chef == "Dawn B." & placement == 2 ~ NA
                                         ,chef == "Shota N." & placement == 2 ~ NA
                                         ,chef == "Marcel V." & placement == 2 ~ NA
                                         ,chef == "Casey T." & placement == 2 ~ NA
                                         ,chef == "Sam T." & placement == 3 ~ NA
                                         ,chef == "Sheldon S." & placement == 3 & sznnumber == 14~ NA
                                         ,chef == "Joe S." & placement == 3 & sznnumber == 15~ NA
                                         ,chef == "Harold D." & placement == 1 ~ NA
                                         ,chef == "Ilan H." & placement == 1 ~ NA
                                         ,chef == "Gabe E." & placement == 1 ~ NA
                                         ,TRUE ~ cheflabelall)
               ,alignment = case_when(chef == "Stephanie I." & placement == 1 ~ 1
                                      ,chef == "Hung H." & placement == 1 ~ .5
                                      ,chef == "Richard B." & placement == 1 ~ 1
                                      ,chef == "Gregory G." & placement == 2 ~ 1
                                      ,chef == "Stefan R." & placement == 2 ~ 1
                                      ,chef == "Doug A." & placement == 3 ~ 1
                                      ,TRUE ~ 0)
               ,xposition = case_when(alignment == 0 ~ episode+.05
                                      ,alignment == 1 ~ episode - .05
                                      ,TRUE ~ episode))

  ## Viz
    # set it up
      bkg_col <- "lightcyan"
      title_col <- subtitle_col <- caption_col <- text_col <- "darkslategray"

      titletext <- "Top Chef cumulative weighted index score by episode"
      captionsource <- str_glue("Visualization: @carlylevitz &bull; Source: github.com/celevitz/topChef &bull; Tools: #rstats #ggplot #ggtext #tidyverse")
      scoring <- str_glue("Methodology for weighted index scores: Elimination win = 7 points &bull; Elimination high = 3 points<br>Elimination low = - 3 points &bull; Eliminated = - 7 points &bull; Quickfire win = 4 points <br> Quickfire high = 2 points &bull; Quickfire low = - 2 points<br><br>")
      captiontext <- str_glue("{scoring} {captionsource}")
      subtitletext <- "Top three chefs across all seasons - *Do not spoil Season 20 Finale!*"

    # plot it
      cumulativeT3 %>%
        arrange(sznnumber,episode,chef) %>%
        ggplot(aes(x=episode,y=indexcumulative,color=chef,label=cheflabelall)) +
          geom_hline(yintercept=0, color="#ffbc69")  +
          labs(title=titletext
               ,subtitle=subtitletext
               ,caption= captiontext) +
          ylab("Cumulative Weighted Index Score") + xlab("Episode") +
          facet_wrap(~placementstring) +
          geom_point(aes(x=episode,y=indexcumulative,color=chef)) +
          geom_line(aes(x=episode,y=indexcumulative,color=chef)) +
          geom_text(aes(x=xposition,y=indexcumulative),col=text_col,hjust=cumulativeT3$alignment) +
          scale_y_continuous(lim=c(-20,70),breaks=seq(-20,70,10),labels=seq(-20,70,10)) +
          scale_x_continuous(lim=c(1,20),breaks=seq(1,16,2),labels=seq(1,16,2)) +
          theme(legend.position    = "none"
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

                ,strip.text        = element_textbox(size     = 15
                                                     ,face     = 'bold'
                                                     ,fill    = text_col
                                                     ,color = "white")
                ,strip.background = element_rect(fill=text_col)

                ,axis.ticks.y      = element_line(color=text_col)
                ,axis.line.y       = element_line(color=text_col)
                ,axis.line.x       = element_line(color=text_col)
                ,axis.ticks.x      = element_line(color=text_col)

                ,axis.text=element_text(size=18,color=text_col)
                ,axis.title.x = element_text(size     = 18
                                             ,color   = text_col)
                ,axis.title.y = element_text(size     = 18
                                             ,color   = text_col
                                             ,margin  = margin(l=15,r=15) ) )

      dev.print(png, file = paste(savedirectory,"TopThreeIndexScores_Cumulative.png",sep=""), width =1100, height = 900)
      dev.off()

# Another version: by season
      # plot it
      cumulativeT3 %>%
        mutate(cheflabelall = ifelse(episode == maxepi,str_split_fixed(chef," ",2)[,1],NA)
               ,cheflabelall = case_when(chef == "Brooke W." & sznnumber == 10 & episode == 17 ~ NA
                                         ,chef == "Brooke W." & sznnumber == 10 & episode == 16 ~ "Brooke"
                                         ,chef == "Michael V." & sznnumber == 6 & episode == 15 ~ NA
                                         ,chef == "Michael V." & sznnumber == 6 & episode == 14 ~ "Michael"
                                         ,chef == "Ed C." & sznnumber == 7 & episode == 14 ~ NA
                                         ,chef == "Ed C." & sznnumber == 7 & episode == 12 ~ "Ed"
                                         ,chef == "Angelo S." & sznnumber == 7 & episode == 14 ~ NA
                                         ,chef == "Angelo S." & sznnumber == 7 & episode == 9 ~ "Angelo"
                                         ,chef == "Kelsey B.-C." & sznnumber == 16 & episode == 15 ~ NA
                                         ,chef == "Kelsey B.-C." & sznnumber == 16 & episode == 14 ~ "Kelsey"
                                         ,chef == "Eric A." & sznnumber == 16 & episode == 15 ~ NA
                                         ,chef == "Eric A." & sznnumber == 16 & episode == 12 ~ "Eric"
                                         ,chef == "Bryan V." & sznnumber == 17 & episode == 14 ~ NA
                                         ,chef == "Bryan V." & sznnumber == 17 & episode == 12 ~ "Bryan"
                                         ,chef == "Shota N." & sznnumber == 18 & episode == 14 ~ NA
                                         ,chef == "Shota N." & sznnumber == 18 & episode == 13 ~ "Shota"
                                        ,TRUE ~ cheflabelall)) %>%
        arrange(sznnumber,episode,chef) %>%
        ggplot(aes(x=episode,y=indexcumulative,color=chef,label=cheflabelall)) +
        geom_hline(yintercept=0, color="#ffbc69")  +
        labs(title=titletext
             ,subtitle=subtitletext
             ,caption= captiontext) +
        ylab("Cumulative Weighted Index Score") + xlab("Episode") +
        facet_wrap(~sznnumber) +
        geom_point(aes(x=episode,y=indexcumulative,color=chef)) +
        geom_line(aes(x=episode,y=indexcumulative,color=chef)) +
        geom_text(aes(x=xposition,y=indexcumulative),col=text_col,hjust=0,size=3) +
        scale_y_continuous(lim=c(-20,70),breaks=seq(-20,70,10),labels=seq(-20,70,10)) +
        scale_x_continuous(lim=c(1,20),breaks=seq(1,16,2),labels=seq(1,16,2)) +
        theme(legend.position    = "none"
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

              ,strip.text        = element_textbox(size     = 15
                                                   ,face     = 'bold'
                                                   ,fill    = text_col
                                                   ,color = "white")
              ,strip.background = element_rect(fill=text_col)

              ,axis.ticks.y      = element_line(color=text_col)
              ,axis.line.y       = element_line(color=text_col)
              ,axis.line.x       = element_line(color=text_col)
              ,axis.ticks.x      = element_line(color=text_col)

              ,axis.text=element_text(size=13,color=text_col)
              ,axis.title.x = element_text(size     = 18
                                           ,color   = text_col)
              ,axis.title.y = element_text(size     = 18
                                           ,color   = text_col
                                           ,margin  = margin(l=15,r=15) ) )

      dev.print(png, file = paste(savedirectory,"TopThreeIndexScores_Cumulative_SeasonsSeparated.png",sep=""), width =1100, height = 900)
      dev.off()

##########################################
## Win %s
results <- topChef::challengewins %>%
        # exclude finale
        group_by(szn,sznnumber,series) %>%
        mutate(maxepi = case_when(challenge_type == "Elimination" ~ max(episode)
                                   ,TRUE ~ NA) ) %>%
        filter(episode != maxepi) %>%
        # how many elimination challenges did they participate in
        filter(in.competition == "TRUE" & challenge_type == "Elimination" & series == "US") %>%
        ungroup() %>% group_by(szn,sznnumber,chef) %>%
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
        full_join(topChef::chefdetails %>% select(series,sznnumber,szn,chef,placement)) %>%
        mutate(final3 = ifelse(placement<=3,"Final 3","All others"))







results %>%
  ggplot(aes(x=winpercent,group=final3,color=final3)) +
  geom_histogram()















