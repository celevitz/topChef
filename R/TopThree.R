## Top Three
## 2023-06-08
## Carly Levitz


savedirectory <- "/Users/carlylevitz/Documents/Data/TCSeason20/Episode-agnostic/"

library(topChef); library(tidyverse)

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
    allseasons %>%
      ggplot(aes(x=placement,y=indexWeight))  +
      geom_hline(yintercept=0, color="#ffbc69")  +
      labs(title="\n\nComparing Final Three Chefs Across All Seasons:"
           ,subtitle=paste0("Top Chef Weighted Index: 13 elimination challenges into each season")
           ,caption="Scoring: Elimination win = 7 points. Elimination high = 3. Elimination low = -3. Eliminated = -7.\nQuickfire win = 4. Quickfire high = 2. Quickfire low = -2.\n\nData github.com/celevitz/topChef ||| Twitter @carlylevitz")+
      scale_x_continuous(lim=c(.7,4.5), breaks=c(1,2,3,4),labels=c("1st place","2nd place","3rd place","Season 20\nFinal Three")) +
      scale_y_continuous(lim=c(-5,60),breaks=c(-5,seq(0,60,10)),labels=c(-5,seq(0,60,10))) +
      theme_minimal() +
      ylab("\nIndex score\n") + xlab("") +
      theme(panel.grid = element_blank()
            ,panel.background = element_rect(fill="darkcyan",color="darkcyan")
            ,plot.background = element_rect(fill="darkcyan",color="darkcyan")
            ,axis.ticks.y = element_line(color="white")
            ,axis.line.y = element_line(color="white")
            ,axis.line.x=element_blank()
            ,axis.ticks.x=element_blank()
            ,plot.title = element_text(size=24,face="bold",color="white")
            ,plot.subtitle = element_text(size=24,color="white")
            ,plot.caption = element_text(size=15,color="white")
            ,axis.text=element_text(size=18,color="white")
            ,axis.title = element_text(size=18,color="white")
      ) +
      # label people
      geom_text(aes(x=plotxforlabel,y=indexWeight,label=labelname,hjust=alignment),color="white") +
      # call out buddha & sara in their previous seasons
      #geom_rect(xmin = .8, xmax = 1.2, ymin = 34 , ymax = 36,color="darkslategray",fill=NA) +
      #geom_rect(xmin = 1.6, xmax = 2, ymin = 11 , ymax = 13,color="darkslategray",fill=NA) +
      geom_rect(xmin = 3.8, xmax = 4.2, ymin = 49 , ymax = 51,color="darkslategray",fill=NA) +
      geom_rect(xmin = 3.8, xmax = 4.2, ymin = 3 , ymax = 5,color="darkslategray",fill=NA) +
      geom_rect(xmin = 3.6, xmax = 4.4, ymin = -1 , ymax = -3,color="darkslategray",fill=NA)


    dev.print(png, file = paste(savedirectory,"TopThreeIndexScores.png",sep=""), width = 900, height = 900)
    dev.off()

## Histogram

    allseasons %>%
      ggplot(aes(x=indexWeight,color=factor(placement),fill=factor(placement))) +
      geom_histogram(alpha = .3,binwidth=5,position="identity") +
      xlab("Weighted Index Score") + ylab("Number of chefs") +
      theme_minimal() +
      scale_color_manual(values = c("slategray3","slategray2","slategray1","slategray4")) +
      scale_fill_manual(values = c("slategray3","slategray2","slategray1","slategray4")) +
      theme(panel.grid = element_blank()
            #,panel.background = element_rect(fill="darkcyan",color="darkcyan")
            #,plot.background = element_rect(fill="darkcyan",color="darkcyan")
            ,axis.ticks.y = element_line(color="darkslategray")
            ,axis.line.y = element_line(color="darkslategray")
            ,axis.ticks.x = element_line(color="darkslategray")
            ,axis.line.x = element_line(color="darkslategray")
            ,plot.title = element_text(size=24,face="bold",color="darkslategray")
            ,plot.subtitle = element_text(size=24,color="darkslategray")
            ,plot.caption = element_text(size=15,color="darkslategray")
            ,axis.text=element_text(size=18,color="darkslategray")
            ,axis.title = element_text(size=18,color="darkslategray")
      )

