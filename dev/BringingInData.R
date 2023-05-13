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

rm(list=ls())

library(tidyverse); library(openxlsx); library(usethis); library(rmarkdown)

directory <- "/Users/carlylevitz/Documents/Data/"

chefdetails <- as_tibble(read.xlsx(paste(directory,"TopChefData.xlsx",sep=""),sheet=1))
challengewins <- as_tibble(read.xlsx(paste(directory,"TopChefData.xlsx",sep=""),sheet=2))

challengedescriptions <- as_tibble(read.xlsx(paste(directory,"TopChefData.xlsx",sep=""),sheet=3))
rewards <- as_tibble(read.xlsx(paste(directory,"TopChefData.xlsx",sep=""),sheet=4))
judges <- as_tibble(read.xlsx(paste(directory,"TopChefData.xlsx",sep=""),sheet=5))
episodeinfo <- as_tibble(read.xlsx(paste(directory,"TopChefData.xlsx",sep=""),sheet=6))
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

# Check for CRAN specific requirements
#results <- rhub::check_for_cran()
#results$cran_summary()
#usethis::use_cran_comments()

