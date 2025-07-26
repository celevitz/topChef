## Analyze dish data

rm(list=ls())

library(stringr)
library(topChef)
library(openxlsx)
library(ggplot2)
library(tidyverse)

directory <- "/Users/carlylevitz/Documents/Data/"

dishesraw <- read.csv(paste0(directory
                             ,"/topChef/Top Chef - Dishes wide form.csv")
                      ,header = TRUE)

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
      highlevel$percent <- paste0(round((highlevel$count.1/(highlevel$count.0+
                                                highlevel$count.1))*100,1),"%")
      names(highlevel) <- c("seasonNumber","disheswithoutwordorwords"
                            ,"disheswithwordorwords"
                            ,"percentofdisheswithwordorwords")
      print("Number of dishes")
      print(sum(highlevel$disheswithwordorwords))

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

      ## outcomes of dishes with the word/s of interest
      print("Outcomes of dishes with the word/s of interest")
      print(table(basicdata$outcome))

      ## % of times that it won or was eliminated
      print("What's the % of times this dish won?")
      print(paste0(round(nrow(basicdata[basicdata$outcome %in% c("WIN","WINNER"),])/
              nrow(basicdata[,])*100,1),"%"))
      print("What's the % of times this dish won or was at the top?")
      print(paste0(round(nrow(basicdata[basicdata$outcome %in% c("WIN","WINNER","HIGH"),])/
                           nrow(basicdata[,])*100,1),"%"))

      print("What's the % of times this dish was eliminated?")
      print(paste0(round(nrow(basicdata[basicdata$outcome %in% c("OUT","RUNNER-UP"),])/
                           nrow(basicdata[,])*100,1),"%"))
      print("What's the % of times this dish was eliminated or at the bottom?")
      print(paste0(round(nrow(basicdata[basicdata$outcome %in% c("OUT","RUNNER-UP","LOW"),])/
                           nrow(basicdata[,])*100,1),"%"))

      ## Who won?
      print("Who won?")
      print(basicdata[basicdata$outcome %in% c("WIN","WINNER")
                      ,c("season","seasonNumber","episode","chef"
                         ,"challengeType")])
}


seriesname <- "US"

wordtrend("US",c("risotto"))


wordtrend("US",c("aguachile","crudo","ceviche","crudite"
                 ,"futomake","leche de tigre","nigiri","poke","sashimi"
                 ,"negitoro"))
wordtrend("US",c("duo","trio","3-ways","3 ways","2 ways","2-ways","three ways"
                 ,"two ways","dual"))
wordtrend("US",c("foam","gel","mousse","snow","espuma"))
wordtrend("US",c("compressed"))
wordtrend("US",c("sous vide"))
wordtrend("US",c("dashi"))
wordtrend("US",c("plantain"))
wordtrend("US",c("pickle","pikliz"))
wordtrend("US",c("sunchoke","jerusalem artichoke"))
wordtrend("US",c("tuile"))
wordtrend("US",c("scallop"))
wordtrend("US",c("bacon"))
wordtrend("US",c("foie"))
wordtrend("US",c("beet"))
wordtrend("US",c("gazpacho"))
wordtrend("US",c("confit"))
wordtrend("US",c("pollen"))
wordtrend("US",c("xo"))
wordtrend("US",c("maple"))
wordtrend("US",c("fennel"))
wordtrend("US",c("pasta","ravioli","linguine","spaghetti","lasagna"
                 ,"gnocchi","fussili","penne","orzo","fettuccine","cavatelli"
                 ,"gemelli","campanelle","pappardelle","orecchiette"))


wordtrend("US",c("liquid nitrogen"))

