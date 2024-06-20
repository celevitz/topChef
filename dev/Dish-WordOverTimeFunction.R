## Analyze dish data

rm(list=ls())

library(stringr)
library(topChef)
library(openxlsx)
library(ggplot2)

directory <- "/Users/carlylevitz/Documents/Data/"

dishesraw <- as_tibble(read.xlsx(paste(directory,"TopChefData.xlsx",sep="")
                                 ,sheet=2)) %>%
  filter(inCompetition == TRUE & !(is.na(dish))) %>%
  select(series,season,seasonNumber,episode,chef,challengeType
         ,dish,outcome,notes) %>%
  mutate(dish = tolower(dish),notes=tolower(notes))




# Function
wordtrend <- function(seriesname,wordorwordsofinterest) {

  basicdata <- dishesraw[dishesraw$series == seriesname,]
  basicdata$word <- 0

  for (w in wordorwordsofinterest) {
    basicdata$word[grepl(w,basicdata$dish) | grepl(w,basicdata$notes)] <- 1
  }


  ## Multiple functions:
    ## descriptions with and without the word/phrase
      highlevel <- basicdata
      highlevel$temp <- 1
      highlevel <- aggregate(highlevel$temp
                             ,by=list(highlevel$seasonNumber,highlevel$word)
                             ,FUN=length)
      names(highlevel) <- c("seasonNumber","usesWord","count")
      highlevel <- reshape(highlevel
                           ,direction="wide"
                           ,timevar="usesWord"
                           ,idvar="seasonNumber")
      highlevel$count.1[is.na(highlevel$count.1)] <- 0
      highlevel$percent <- highlevel$count.1/(highlevel$count.0+
                                                highlevel$count.1)
      names(highlevel) <- c("seasonNumber","disheswithoutwordorwords"
                            ,"disheswithwordorwords"
                            ,"percentofdisheswithwordorwords")
      print(highlevel)

    ## Now just look at when the word/s were used:
      basicdata <- basicdata[basicdata$word == 1,]

      ## Number of episodes in which that word was used
      episodes <- unique(basicdata[,c("series","season","seasonNumber"
                                      ,"episode")])
      print("Number of episodes in which word/s of interest was used by season")
      print(table(episodes$seasonNumber))

      ## number of chefs who used that word in a season
      chefs <- unique(basicdata[,c("series","season","seasonNumber","chef")])
      print("Number of chefs whose dishes had word/s of interest by season")
      print(table(chefs$seasonNumber))

      ## win vs not or something?
}

wordorwordsofinterest <- c("aguachile","carpaccio","crudo","ceviche","crudite"
                         ,"futomake","leche de tigre","nigiri","poke","sashimi")
seriesname <- "US"
wordtrend("US",wordorwordsofinterest)










