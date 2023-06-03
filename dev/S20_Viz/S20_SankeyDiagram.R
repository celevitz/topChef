## Carly Levitz
## Date written: 2023-05-20
## Date updated: 2023-06-03
## Purpose: Create a sankey diagram for this season of top chef


#####
## Set up
rm(list=ls())
devtools::install_github("celevitz/topChef")
library(tidyverse); library(topChef); library(ggalluvial); library(ggpubr);

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
             ,freq=1 ) %>%
      select(!(c(szn,sznnumber,series,in.competition,rating,episode))) %>%
      # drop those after they are out
      filter(!(is.na(outcome)))

      alluvialdata$outcome <- factor(alluvialdata$outcome,levels=c("WIN","HIGH","IN","LOW","OUT"))

      alluvialdata <- alluvialdata[order(alluvialdata$episodechar,alluvialdata$challenge_type,alluvialdata$outcome,alluvialdata$chef),]
      alluvialdata$challenge_type <- NULL

    # Add names so that it's easier to read
    # combine with full dataset
    alluvialdataEp00 <- alluvialdata %>% filter(episodechar == "01") %>%
          mutate(episodechar = "00"
                 ,epichallenge = "Chef"
                 ,outcome=chef) %>%
          distinct() %>%
          bind_rows(alluvialdata)

    # Order the data in a way that makes sense
      alluvialdataEp00$outcome <- factor(alluvialdataEp00$outcome
                                         ,levels=c("WIN","HIGH","IN","LOW","OUT"
                                                   ,"Samuel Albert" ,"Dawn B.","May Phattanant Thongthong"
                                                   ,"Luciana Berry" ,"Begoña Rodrigo" ,"Sylwia Stachyra"
                                                   ,"Dale MacKay" ,"Charbel Hayek" ,"Nicole Gomes","Victoire Gouloubi"
                                                   ,"Amar S."  ,"Tom Goetter","Ali Ghzawi","Sara B.","Gabriel Rodriguez" ,"Buddha"   ))
      alluvialdataEp00$chef <- factor(alluvialdataEp00$chef
                                      ,levels=c("Samuel Albert" ,"Dawn B.","May Phattanant Thongthong"
                                               ,"Luciana Berry" ,"Begoña Rodrigo" ,"Sylwia Stachyra"
                                               ,"Dale MacKay" ,"Charbel Hayek" ,"Nicole Gomes","Victoire Gouloubi"
                                               ,"Amar S."  ,"Tom Goetter","Ali Ghzawi","Sara B.","Gabriel Rodriguez" ,"Buddha"   ))
      alluvialdataEp00 <- alluvialdataEp00[order(alluvialdataEp00$episodechar,alluvialdataEp00$epichallenge,alluvialdataEp00$outcome,alluvialdataEp00$chef),]


  ##################
  ## viz
  ##################
      chefcolors <- c("Samuel Albert" = "#ffff6d"
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
                      ,"Tom Goetter" = "#924900"   )
      outcomecolors <- c("OUT" = "gray75"
                         ,"LOW" = "#ffbc69"
                         ,"IN" = "#a3cce9"
                         ,"HIGH" = "#5fa2ce"
                         ,"WIN" = "#1170AA")

      # Elimination challenge
          eliminationchall <- alluvialdataEp00 %>%
            filter(grepl("Elimination",alluvialdataEp00$epichallenge) | alluvialdataEp00$epichallenge == "Chef") %>%
            ggplot(aes(x = epichallenge, stratum = outcome, alluvium = chef,color=outcome)) +
            geom_flow(aes(fill=chef,color=outcome),alpha=.4) +
            scale_fill_manual(values=c(chefcolors)) +
            scale_color_manual(values=c(chefcolors ,outcomecolors)) +
            geom_stratum()  +
            geom_text(stat = "stratum", size=5,aes(label= paste(after_stat(stratum))),color="black") +
            labs(title="\n    Top Chef World All Stars: Each Chef's Elimination Challenge Journey") +
            theme_minimal() +
            xlab("") +
            scale_x_discrete(breaks = unique(alluvialdataEp00$epichallenge)
                             ,labels=gsub("AQuickfire","Quickfire",unique(alluvialdataEp00$epichallenge))) +
            theme(panel.grid=element_blank()
                  ,axis.text.y=element_blank()
                  ,axis.text.x=element_text(size=18,color="black")
                  ,legend.position="none"
                  ,plot.title=element_text(size=24,face="bold")
                  ,plot.caption = element_text(size=18))

      # Quickfire challenge
          QFchall  <- alluvialdataEp00 %>%
            filter(grepl("Quickfire",alluvialdataEp00$epichallenge) | alluvialdataEp00$epichallenge == "Chef") %>%
            ggplot(aes(x = epichallenge, stratum = outcome, alluvium = chef,color=outcome)) +
            geom_flow(aes(fill=chef,color=outcome),alpha=.4) +
            scale_fill_manual(values=c(chefcolors)) +
            scale_color_manual(values=c(chefcolors, outcomecolors)) +
            geom_stratum()  +
            geom_text(stat = "stratum", size=5,aes(label= paste(after_stat(stratum))),color="black") +
            labs(title="\n    Top Chef World All Stars: Each Chef's Quickfire Challenge Journey"
                 ,caption="Twitter @carlylevitz        Data from github.com/celevitz/topChef") +
            theme_minimal() +
            xlab("") +
            scale_x_discrete(breaks = unique(alluvialdataEp00$epichallenge)
                             ,labels=gsub("AQuickfire","Quickfire",unique(alluvialdataEp00$epichallenge))) +
            theme(panel.grid=element_blank()
                  ,axis.text.y=element_blank()
                  ,axis.text.x=element_text(size=18,color="black")
                  ,legend.position="none"
                  ,plot.title=element_text(size=24,face="bold")
                  ,plot.caption = element_text(size=18))

      ## Combine the elimination chall & QF challenges into one figure
        ggarrange(eliminationchall,QFchall,nrow=2)
        dev.print(png, file = paste(savedirectory,"S20SankeyDiagram.png",sep=""), width = 2000, height = 1200)
        dev.off()

      # do a sankey for each chef
      for (c in unique(alluvialdataEp00$chef)) {
        print(c)
        alluvialdataEp00 %>%
          ggplot(aes(x = epichallenge, stratum = outcome, alluvium = chef)) +
          geom_flow(aes(fill=chef),alpha=.7) +
          scale_fill_manual(values=c(ifelse(alluvialdataEp00$chef==c,"#db6d00","gray75")
          )) +
          geom_stratum()  +
          geom_text(stat = "stratum", size=3,aes(label= paste(after_stat(stratum)))) +
          ggtitle(paste("Top Chef Season 20 World All Stars: ",c,"'s Journey",sep="") )+
          theme_minimal() +
          xlab("") +
          scale_x_discrete(breaks = unique(alluvialdataEp00$epichallenge)
                           ,labels=gsub("AQuickfire","Quickfire",unique(alluvialdataEp00$epichallenge))) +
          theme(panel.grid=element_blank()
                ,axis.text.y=element_blank()
                ,legend.position="none")

        dev.print(png, file = paste(savedirectory,"S20SankeyDiagram_",c,".png",sep=""), width = 2000, height = 900)
        dev.off()

      }







