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
#   Test Package:              'Cmd + Shift + T'

# Check for things that don't yet have documentation library(tools); undoc(topChef)

rm(list=ls())

library(tidyverse); library(openxlsx); library(usethis)

directory <- "/Users/carlylevitz/Documents/Data/"

chefdetails <- read.xlsx(paste(directory,"TopChefData.xlsx",sep=""),sheet=1)
challengewins <- read.xlsx(paste(directory,"TopChefData.xlsx",sep=""),sheet=2)
  # drop the ones that haven't yet aired
  challengewins <- challengewins %>% filter(!(szn == "World All Stars" & episode %in% c(9,10)))
challengedescriptions <- read.xlsx(paste(directory,"TopChefData.xlsx",sep=""),sheet=3)
  # drop the ones that haven't yet aired
  challengedescriptions <- challengedescriptions %>% filter(!(is.na(outcome_type)))
rewards <- read.xlsx(paste(directory,"TopChefData.xlsx",sep=""),sheet=4)
judges <- read.xlsx(paste(directory,"TopChefData.xlsx",sep=""),sheet=5)
episodeinfo <- read.xlsx(paste(directory,"TopChefData.xlsx",sep=""),sheet=6)
  # drop the ones that haven't yet aired
  episodeinfo <- episodeinfo %>% filter(!(is.na(air_date)))
  # fix the date
  episodeinfo$air_date <- as.Date(as.numeric(episodeinfo$air_date), origin = "1899-12-30")

## save things as RDA

save(chefdetails, file = "data/chefdetails.rda")
save(challengewins, file = "data/challengewins.rda")
save(challengedescriptions, file = "data/challengedescriptions.rda")
save(rewards, file = "data/rewards.rda")
save(judges, file = "data/judges.rda")
save(episodeinfo, file = "data/episodeinfo.rda")


