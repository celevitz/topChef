## Carly Levitz
## Date written: 2023-05-20
## Date updated: 2023-05-20
## Purpose: Create a sankey diagram for this season of top chef


#####
## Set up
rm(list=ls())
devtools::install_github("celevitz/topChef")
library(tidyverse); library(topChef); library(ggalluvial)

savedirectory <- "/Users/carlylevitz/Documents/Data/TCSeason20/Episode-specific/"

#############################################################################
# Alluvial plot
  ##################
  # data set-up
  #################
    alluvialdata <- topChef::challengewins %>%
      filter(series=="US" & szn == "World All Stars") %>%
      # for sorting purposes, make the episode a two-digit character
      mutate(episodechar=case_when(episode <= 9 ~paste0("0",as.character(episode))
                                   ,TRUE ~as.character(episode) )
             # for sorting purposes, make quickfire come before elimination alphabetically
             ,challenge_type=case_when(challenge_type == "Quickfire" ~ "AQuickfire"
                                       ,TRUE ~ challenge_type)
             # make the x label a combination of episode number & challenge type
             ,epichallenge=paste("Ep. ",episodechar,":\n",challenge_type,sep="")
             # want everyone to be listed as out after they are eliminated
             ,outcome=ifelse(in.competition == "FALSE","OUT",outcome)
             # but if they're OUT, put them as low in the episode that they were eliminated
             ,outcome=ifelse(in.competition == "TRUE" & outcome == "OUT","LOW",outcome)
             ,freq=1 ) %>%
      select(!(c(szn,sznnumber,series,in.competition,rating,episode)))

      alluvialdata$outcome <- factor(alluvialdata$outcome,levels=c("OUT","LOW","IN","HIGH","WIN"))

      alluvialdata <- alluvialdata[order(alluvialdata$episodechar,alluvialdata$challenge_type,alluvialdata$outcome,alluvialdata$chef),]
      alluvialdata$challenge_type <- NULL

    # Add names so that it's easier to read
    # combine with full dataset
    alluvialdataEp00 <- alluvialdata %>% filter(episodechar == "01") %>%
          mutate(episodechar = "00"
                 ,epichallenge = "Ep. 00"
                 ,outcome=chef) %>%
          distinct() %>%
          bind_rows(alluvialdata)

    # Order the data in a way that makes sense
      alluvialdataEp00$outcome <- factor(alluvialdataEp00$outcome,levels=c("OUT","LOW","IN","HIGH","WIN"
                                                                           ,"Samuel Albert" ,"Dawn B.","May Phattanant Thongthong"
                                                                           ,"Luciana Berry" ,"Begoña Rodrigo" ,"Sylwia Stachyra"
                                                                           ,"Dale MacKay" ,"Charbel Hayek" ,"Nicole Gomes","Victoire Gouloubi"
                                                                           ,"Amar S."  ,"Sara B."
                                                                           ,"Gabriel Rodriguez" ,"Tom Goetter","Ali Ghzawi","Buddha"   ))
      alluvialdataEp00$chef <- factor(alluvialdataEp00$chef,levels=c("Samuel Albert" ,"Dawn B.","May Phattanant Thongthong"
                                                                     ,"Luciana Berry" ,"Begoña Rodrigo" ,"Sylwia Stachyra"
                                                                     ,"Dale MacKay" ,"Charbel Hayek" ,"Nicole Gomes","Victoire Gouloubi"
                                                                     ,"Amar S."  ,"Sara B."
                                                                     ,"Gabriel Rodriguez" ,"Tom Goetter","Ali Ghzawi","Buddha"   ))
      alluvialdataEp00 <- alluvialdataEp00[order(alluvialdataEp00$episodechar,alluvialdataEp00$epichallenge,alluvialdataEp00$outcome,alluvialdataEp00$chef),]




  ##################
  ## viz
  ##################
      alluvialdataEp00 %>%
        ggplot(aes(x = epichallenge, stratum = outcome, alluvium = chef)) +
        geom_flow(aes(fill=chef),alpha=.7) +
        scale_fill_manual(values=c("Samuel Albert" = "#ffff6d"
                                   ,"Dawn B." = "#b6dbff"
                                   ,"May Phattanant Thongthong" = "#006edb"
                                   ,"Luciana Berry" = "#009292"
                                   ,"Begoña Rodrigo" = "#ffb6db"
                                   ,"Sylwia Stachyra" = "#b66dff"
                                   ,"Dale MacKay" = "#490092"
                                   ,"Charbel Hayek" = "#db6d00"
                                   ,"Nicole Gomes" = "#920000"
                                   ,"Victoire Gouloubi" = "gray75"
                                   ,"Amar S." = "#004949"
                                   ,"Sara B." = "#ff6db6"
                                   ,"Ali Ghzawi" = "gray10"
                                   ,"Buddha" = "#6db6ff"
                                   ,"Gabriel Rodriguez" = "#24ff24"
                                   ,"Tom Goetter" = "#924900"
                                   )) +
        geom_stratum()  +
        geom_text(stat = "stratum", size=3,aes(label= paste(after_stat(stratum)))) +
        ggtitle("Top Chef Season 20: World All Stars") +
        theme_minimal() +
        xlab("") +
        scale_x_discrete(breaks = unique(alluvialdataEp00$epichallenge)
                         ,labels=gsub("AQuickfire","Quickfire",unique(alluvialdataEp00$epichallenge))) +
        theme(panel.grid=element_blank()
              ,axis.text.y=element_blank()
              ,legend.position="none")

      dev.print(png, file = paste(savedirectory,"S20SankeyDiagram.png",sep=""), width = 2000, height = 900)







