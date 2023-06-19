# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#     Other checks:             rhub::check_for_cran()
#                               devtools::check()
#   Test Package:              'Cmd + Shift + T'
#   Knit                       'Cmd + Shift + K'

# Check for things that don't yet have documentation library(tools); undoc(topChef)
# Note to self:
#   After updating something, run:
# devtools::document()
# goodpractice::gp()
# usethis::use_cran_comments()

rm(list=ls())

library(openxlsx)
library(usethis)
library(rmarkdown)
library(goodpractice)
library(stringi)
library(tidyr)

directory <- "/Users/carlylevitz/Documents/Data/"

chefdetails <- as_tibble(read.xlsx(paste(directory,"TopChefData.xlsx",sep=""),sheet=1))
challengewins <- as_tibble(read.xlsx(paste(directory,"TopChefData.xlsx",sep=""),sheet=2))

challengedescriptions <- as_tibble(read.xlsx(paste(directory,"TopChefData.xlsx",sep=""),sheet=3))
rewards <- as_tibble(read.xlsx(paste(directory,"TopChefData.xlsx",sep=""),sheet=4))
judges <- as_tibble(read.xlsx(paste(directory,"TopChefData.xlsx",sep=""),sheet=5))
episodeinfo <- as_tibble(read.xlsx(paste(directory,"TopChefData.xlsx",sep=""),sheet=6))
  # fix the date
  episodeinfo$airDate <- as.Date(as.numeric(episodeinfo$airDate)
                                  , origin = "1899-12-30")

## Check for UTF-8 strings
#
#   chefdetails$name[grepl("ö",chefdetails$name)] <-
#  gsub("ö",as.character(iconv("ö","UTF-8","latin1","Unicode"))
#       ,chefdetails$name[grepl("ö",chefdetails$name)])
#
#   chefdetails$name[stri_enc_mark(chefdetails$name) == "UTF-8"]
#   chefdetails$chef[stri_enc_mark(chefdetails$chef) == "UTF-8"]
#   chefdetails$occupation[stri_enc_mark(chefdetails$occupation) == "UTF-8"]
#   chefdetails$hometown[stri_enc_mark(chefdetails$hometown) == "UTF-8"]
#   chefdetails$city[stri_enc_mark(chefdetails$city) == "UTF-8"]
#   chefdetails$state[stri_enc_mark(chefdetails$state) == "UTF-8"]
#   episodeinfo$episode_name[stri_enc_mark(episodeinfo$episode_name) == "UTF-8"]
#   judges$guestjudge[stri_enc_mark(judges$guestjudge) == "UTF-8"]
#   challengewins$chef[stri_enc_mark(challengewins$chef) == "UTF-8"]
   # challengedescriptions$shop.budget[stri_enc_mark(challengedescriptions$shop.budget) == "UTF-8"]
   # challengedescriptions$shop.time[stri_enc_mark(challengedescriptions$shop.time) == "UTF-8"]
   # challengedescriptions$product.placement[stri_enc_mark(challengedescriptions$product.placement) == "UTF-8"]
   # challengedescriptions$Last.Chance.Kitchen.winner.enters[stri_enc_mark(challengedescriptions$Last.Chance.Kitchen.winner.enters) == "UTF-8"]
   # challengedescriptions$advantage[stri_enc_mark(challengedescriptions$advantage) == "UTF-8"]
#   rewards$reward[stri_enc_mark(rewards$reward) == "UTF-8"]
#   rewards$chef[stri_enc_mark(rewards$chef) == "UTF-8"]
#   asc <- function(x) { strtoi(charToRaw(x),16L)}
#   chr <- function(n) { rawToChar(as.raw(n))}
#
#   chefdetails$name <- gsub("ñ",chr(asc("ñ")),chefdetails$name)




# For now - replace umlauts and such with non-special characters.
#  definitely not ideal

chefdetails$name <- gsub("à","a",gsub("é","e",gsub("ñ","n",gsub("ö","o"
                ,gsub( "\\\\" ,"", gsub("ä","a",chefdetails$name) )))))
chefdetails$chef <- gsub("à","a",gsub("é","e",gsub("ñ","n",gsub("ö","o"
                ,gsub( "\\\\" ,"", gsub("ä","a",chefdetails$chef) )))))
challengewins$chef <- gsub("à","a",gsub("é","e",gsub("ñ","n",gsub("ö","o"
                ,gsub( "\\\\" ,"", gsub("ä","a",challengewins$chef) )))))
rewards$chef <- gsub("à","a",gsub("é","e",gsub("ñ","n",gsub("ö","o"
                ,gsub( "\\\\" ,"", gsub("ä","a",rewards$chef) )))))
judges$guestJudge <- gsub("à","a",gsub("é","e",gsub("ñ","n",gsub("ö","o"
                ,gsub( "\\\\" ,"", gsub("ä","a",judges$guestJudge) )))))
challengedescriptions$challengeDescription <- gsub("à","a",gsub("é","e"
                 ,gsub("ñ","n",gsub("ö","o",gsub( "\\\\" ,"", gsub("ä","a"
                 ,challengedescriptions$challengeDescription) )))))

episodeinfo$episodeName <-gsub("à","a",gsub("é","e",gsub("ñ","n",gsub("ö","o"
              ,gsub( "\\\\" ,"", gsub("ä","a",episodeinfo$episodeName) )))))

rewards$reward <- gsub("à","a",gsub("é","e",gsub("ñ","n",gsub("ö","o"
              ,gsub( "\\\\" ,"", gsub("ä","a",rewards$reward) )))))

## save things as RDA

save(chefdetails, file = "data/chefdetails.rda")
save(challengewins, file = "data/challengewins.rda")
save(challengedescriptions, file = "data/challengedescriptions.rda")
save(rewards, file = "data/rewards.rda")
save(judges, file = "data/judges.rda")
save(episodeinfo, file = "data/episodeinfo.rda")

# Check for CRAN specific requirementss
#results <- rhub::check_for_cran()
#results$cran_summary()
#usethis::use_cran_comments()

# Run this to see how much of your function is covered: devtools::test_coverage()
# run good practice: goodpractice::gp()

