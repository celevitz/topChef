library(tidyverse)
library(rvest)
library(openxlsx)

rm(list=ls())

## Get the list of all chefs
  directory <- "/Users/carlylevitz/Documents/Data/"

  chefs <- as_tibble(read.xlsx(paste(directory
                                     ,"TopChefData.xlsx",sep=""),sheet=1))
  cheflist <- chefs %>%
    filter(series=="US") %>%
    select(name) %>%
    mutate(name=gsub(" ","-",name)) %>%
    distinct()

  numberofchefs <- dim(cheflist)[1]

## Set up the dataset
    bios <- data.frame(chefname=character(),
                       biotext=character(),
               stringsAsFactors=FALSE)

    ## Loop through all chefs
    for (i in seq(1,numberofchefs,1)) {

      # get the chef's name
      chefname <- as.character(gsub("-"," ",cheflist[i,]))
      print(chefname)
      # episode url
      url <- paste0("https://www.bravotv.com/people/"
                    ,as.character(cheflist[i,])
                    )
      # Need to figure out how to skip someone if they don't have a page
        skip_to_next <- FALSE

        # Note that print(b) fails since b doesn't exist

      tryCatch(page <- read_html(url), error = function(e) { skip_to_next <<- TRUE})

      if(skip_to_next) { next }  else {
        page <- read_html(url)

        x_p <- page |>
          html_nodes("p")

        # what parts of the page mention the chef's name?
        ids <-which(str_detect(as.character(x_p),chefname ))

        # put into holding
        bios <- bios %>%
          bind_rows(as.data.frame(cbind(
            chefname=chefname
            # text from website
            ,biotext = x_p[ids] |> html_text())))
      }


    }

bios <- bios %>% distinct()

## What are the words to look for?
## put it in lower case because I don't know if "award" and such will always be capitalized
wordstolookfor <- c("james beard","award","win","nominat","star")

bios$keep <- 0


  for (word in wordstolookfor) {
    bios$keep[grepl(word,tolower(bios$biotext))] <- 1

  }


table(bios$keep)
table(is.na(bios$biotext))

  bios %>%
    filter(is.na(biotext)) %>%
    select(chefname) %>% distinct()



write.csv(bios,paste(directory,"topChef/TopChefBios.csv"),row.names=F)

