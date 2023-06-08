## Top Three
## 2023-06-08
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
    subtitletext <- str_glue("Top Chef Weighted Index: 13 elimination challenges into each season")

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
      ylab("\nIndex score\n") + xlab("") +
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
            ,axis.title = element_text(size=18,color=text_col)
      ) +
      # label people
      geom_text(aes(x=plotxforlabel,y=indexWeight,label=labelname,hjust=alignment),color=text_col) +
      geom_rect(xmin = 3.8, xmax = 4.2, ymin = 49 , ymax = 51,color=text_col,fill=NA) +
      geom_rect(xmin = 3.8, xmax = 4.2, ymin = 3 , ymax = 5,color=text_col,fill=NA) +
      geom_rect(xmin = 3.6, xmax = 4.4, ymin = -1 , ymax = -3,color=text_col,fill=NA)

    dev.print(png, file = paste(savedirectory,"TopThreeIndexScores.png",sep=""), width = 900, height = 900)
    dev.off()
