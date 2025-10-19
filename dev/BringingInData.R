## Bring in data from Excel file

rm(list=ls())

library(openxlsx)
library(usethis)
library(rmarkdown)
library(goodpractice)
library(stringi)
library(tidyr)
library(tidyverse)

directory <- "/Users/carlylevitz/Documents/Data/"

chefdetails <- as_tibble(read.xlsx(paste(directory
                                         ,"TopChefData.xlsx",sep=""),sheet=1))
  # Drop the birth year and astrological things
    chefdetails$birth.year <- chefdetails$premiere <- chefdetails$Sign <-
      chefdetails$Polarity <- chefdetails$Modality <- chefdetails$Triplicity <-
      chefdetails$Northern.Hemisphere.Season <- chefdetails$Animal <-
      chefdetails$Yin.Yang <- chefdetails$Element <-
      chefdetails$culinary.education <- NULL

challengewins <- as_tibble(read.xlsx(paste(directory
                                           ,"TopChefData.xlsx",sep=""),sheet=2))
  challengewins$dish <- NULL
  challengewins$notes <- NULL

challengedescriptions <- as_tibble(read.xlsx(paste(directory
                                         ,"TopChefData.xlsx",sep=""),sheet=3))
rewards <- as_tibble(read.xlsx(paste(directory
                                     ,"TopChefData.xlsx",sep=""),sheet=4))
  # drop the advantages
  rewards <- rewards %>% filter(!(rewardCategory %in% "advantage"))
  rewards$rewardCategory <- NULL
judges <- as_tibble(read.xlsx(paste(directory
                                    ,"TopChefData.xlsx",sep=""),sheet=5))
episodeinfo <- as_tibble(read.xlsx(paste(directory
                                         ,"TopChefData.xlsx",sep=""),sheet=6))
  # fix the date
  episodeinfo$airDate <- as.Date(as.numeric(episodeinfo$airDate)
                                  , origin = "1899-12-30")

## Check for UTF-8 strings
#
#   chefdetails$name[grepl("ö",chefdetails$name)] <-
#  gsub("ö",as.character(iconv("ö","UTF-8","latin1","Unicode"))
#       ,chefdetails$name[grepl("ö",chefdetails$name)])
#
#   chefdetails$name[(chefdetails$name) == "UTF-8"]
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


## Add category for occupation
    chefdetails <- chefdetails %>%
      mutate(occupation_category = case_when(
        occupation %in% c("Assistant professor","Author/dietician"
                          ,"Bartender","Chef Educator","Chef/Artist/Father"
                          ,"Chef, Artist and Teacher","Cooking teacher"
                          ,"Cruise ship chef","Culinary director"
                         ,"Culinary Director","Culinary student"
                         ,"Consultant/Cooking School Owner"
                         ,"Director of Food & Beverage"
                         ,"Director of culinary operations"
                         ,"Head of Development","Instructor"
                         ,"Natural nutritionist","Owner","Owner/caterer"
                         ,"Pitmaster/Owner","President","Principal"
                         ,"Restaurateur","Sommelier"
                         ,"Vice President of Culinary Arts") ~ "Other"
       ,occupation %in% c("Chef","Chef/Consultant","Chef/cheese maker"
                          ,"Chef/restaurateur"
                          ,"Head Chef","Hotel chef"
                          ,"Master cook","Restaurant chef"
                          ,"Room service chef") ~ "Chef"
       ,occupation == "Chef de Cuisine" ~ "Chef de Cuisine"
       ,occupation %in% c("Executive Chef","Exeecutive Chef","Executive chef"
                          ,"Executive consulting chef"
                          ,"Executive Chef/Culinary Director"
                          ,"Executive chef/Culinary director"
                          ,"Executive Chef/General Manager"
                          ,"Executive chef/General Manager"
                          ,"Executive chef/Co-Partner"
                          ,"Executive Chef/Restaurant Consultant"
                          ,"Executive chef/Restaurant Consultant"
                          ,"Executive chef/chef/restaurateur"
                          ,"Executive chef/restaurateur"
                          ,"Executive chef/restaurateur ") ~ "Executive Chef"
       ,occupation %in% c("Chef/Business owner","Chef/Business owner/founder"
                      ,"Chef/Co-founder","Chef/Co-owner","Chef/Co-Owner"
                      ,"Chef/Founder","Chef/Owner","Chef/Partner","Chef/Patron"
                      ,"Chef/Proprieter"
                      ,"Executive Chef/Co-Partner","Executive chef/Co-Partner"
                      ,"Executive Chef/Owner","Executive chef/Owner"
                      ,"Executive Chef/Partner","Executive chef/Partner"
                      ,"Executive Chef/Proprietor","Executive chef/Proprietor"
                      ,"Founder/Executive Chef")~
                                "Executive Chef and Owner/Partner/Founder"
       ,occupation %in% c("Consultant/Private Caterer","Private Chef"
                          ,"Private chef"
                          ,"Private Chef/Consultant","Private chef/Consultant"
                          ,"Personal chef/caterer") ~ "Private chef"
       ,occupation %in% c("Executive Sous Chef","Executive sous chef"
                          ,"Sous Chef","Sous chef") ~ "Sous Chef"
       ,TRUE ~occupation))

## Do some data validation
    # Challenge wins
    table(challengewins$inCompetition,challengewins$outcome)
    table(challengewins$inCompetition,challengewins$immune)
    challengewins %>% group_by(series,season,chef) %>%
      filter(outcome %in% "OUT") %>%
      summarise(n=n()) %>%
      filter(n<1)

    # chef details
    chefdetails %>% group_by(series,chef) %>%
      select(series,chef,name) %>% distinct() %>%
      summarise(n=n()) %>% filter(n>1)

    # rewards: each row is a single chef
    rewards %>% filter(grepl(",",chef))


## save things as RDA
    # challengewinsnoLCK <- challengewins %>% filter(series != "US LCK")
    # challengedescriptionsnoLCK <- challengedescriptions %>%
    #                                 filter(series != "US LCK")
    # episodeinfonoLCK <- episodeinfo %>% filter(series != "US LCK")

save(chefdetails, file = "data/chefdetails.rda")
save(challengewins, file = "data/challengewins.rda")
save(challengedescriptions, file = "data/challengedescriptions.rda")
save(rewards, file = "data/rewards.rda")
save(judges, file = "data/judges.rda")
save(episodeinfo, file = "data/episodeinfo.rda")

## save as CSV for my own use later
write.csv(chefdetails
          ,paste0(directory,"/topChef/Top Chef - Chef details.csv")
          ,row.names=FALSE)
write.csv(challengewins
          ,paste0(directory,"topChef/Top Chef - Challenge wins.csv")
          ,row.names=FALSE)
write.csv(challengedescriptions
          ,paste0(directory,"/topChef/Top Chef - Challenge descriptions.csv")
          ,row.names=FALSE)
write.csv(rewards
          ,paste0(directory,"/topChef/Top Chef - Rewards.csv")
          ,row.names=FALSE)
write.csv(judges
          ,paste0(directory,"/topChef/Top Chef - Guest judges.csv")
          ,row.names=FALSE)
write.csv(episodeinfo
          ,paste0(directory,"/topChef/Top Chef - Episode information.csv")
          ,row.names=FALSE)

