## What is the result of advantages, and how does it differ by adv. type?
## Carly Levitz
## 10/5/2025

rm(list=ls())

library(tidyverse)
library(openxlsx)
directory <- "/Users/carlylevitz/Documents/Data/topChef/"

challdata <- read.csv(paste0(directory
                                    ,"Top Chef - Challenge wins.csv")
                             ,header = TRUE) %>%
  filter(series == "US" & inCompetition == TRUE) %>%
  ## Who ever had an advantage? Who ever had immunity?
    mutate(everimm = ifelse(immune == TRUE,1,0)
           ,everadv = ifelse(!(is.na(advantage)),1,0)
            ,advTF = ifelse(!(is.na(advantage)),TRUE,FALSE)) %>%
    group_by(series,season,chef) %>%
    mutate(everimm = max(everimm,na.rm=T)
           ,everadv = max(everadv,na.rm=T)
  ## did they win?
  ## reclassify "DIDN"T COMPETE" as an "IN"
        ,win = ifelse(outcome %in% c("WIN","WINNER"),1,0)
        ,outcome = ifelse(outcome %in% "DIDN'T COMPETE","IN",outcome)
  ## reclassify challenge types
        , challengeType = ifelse(challengeType %in% c("Elimination"
                            ,"Quickfire Elimination","Sudden Death Quickfire")
          ,"Elimination",challengeType
        )   ) %>%
  ## Did anyone have immunity in that episode?
  ungroup() %>% group_by(series,season,seasonNumber,episode,challengeType) %>%
  mutate(immunityoffered = max(ifelse(immune == TRUE,1,0),na.rm=T)
  ## categorize the advantages
    ,choseinspiration = ifelse(advantage %in%
           c("Choose course; assign course to others"
             ,"Choose dish for inspiration"
             ,"Choose foods"
             ,"Choose inspiration (chef icon) for yourself; Choose others' inspiration (chef icon)"
             ,"Choose inspiration (what region of food)"
             ,"Choose inspiration (which person to cater to)"
             ,"Choose protein; Choose course"
             ,"Choose which dish to make"
             ,"Switch knife block selection"
             ,"Switch which dish to make"),1,0)
    ,chooseorassignsous = ifelse(advantage %in% c("Assign others' sous chefs"
              ,"Choose order of picking protein; Choose order of picking sous chefs"
              ,"Choose sous chef; Assign others' sous chefs"
              ,"Choose sous chefs; Assign others' sous chefs"
              ,"Choose your sous chefs"
              ,"Choose your sous chef; Assign others' sous chefs"),1,0)
    ,chooseteam = ifelse(advantage %in% c("Be team leader"
              ,"Choose partner; Extra 30 minutes"
              ,"Choose team","Choose team captains"
              ,"Choose team leader","Choose team to join"
              ,"Choose team; Assign teams for others"),1,0)
    ,extratime = ifelse(advantage %in% c("Choose partner; Extra 30 minutes"
               ,"Extra 15 minutes","Extra 30 minutes"
               ,"Extra 30 minutes; Serve judges first"
               ,"Extra 30 minutse"
               ,"Extra 60 minutes"),1,0)
    ,ingredients = ifelse(advantage %in% c("Better ingredients"
               ,"Choose ingredients first; choose order"
               ,"Choose order of picking protein; Choose order of picking sous chefs"
               ,"Choose protein","Choose proteins"
               ,"Choose protein (fish)"
               ,"Choose protein first","Choose protein; Choose course"
               ,"Restrict others' access to items"
               ,"Use items brought from home"),1,0)
    ,extramoney= ifelse(advantage %in% c("Extra $100","Extra $200 for wine")
                        ,1,0)
         )

## Let's start off with the basics. How do people do in immunity challenges when
## they have immunity?
  challdata %>%
    filter(immune == TRUE) %>%
    group_by(outcome) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    mutate(N=sum(n,na.rm=T)
           ,percent=n/N) %>%
    arrange(desc(percent))

  challdata %>%
    filter(!(is.na(advantage))) %>%
    group_by(outcome) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    mutate(N=sum(n,na.rm=T)
           ,percent=n/N) %>%
    arrange(desc(percent))

## How does this compare to the distribution when there isn't immunity?
  challdata %>%
    mutate(outcome = case_when(outcome %in% c("DISQUALIFIED","RUNNER-UP"
                                              ,"WITHDREW") ~ "OUT"
                               ,outcome %in% c("WINNER") ~ "WIN"
                               ,TRUE ~ outcome)) %>%
    filter(immune == FALSE & is.na(advantage) &
             challengeType == "Elimination") %>%
    group_by(outcome) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    mutate(N=sum(n,na.rm=T)
           ,percent=n/N) %>%
    arrange(desc(percent))


## How do the different advantages differ?
  temp <- data.frame(challdata)
  for (varname in c("choseinspiration","chooseorassignsous","chooseteam"
                    ,"extratime","ingredients","extramoney")) {

    temp1 <- temp[temp[,varname] %in% 1,]
    print(varname)
    print(dim(temp1)[1])
    print(table(temp1[,varname],temp1$outcome))
    print("*******************************")

  }


#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# ## When immunity was offered, how did it affect odds of winning?
# immoffereddata <- challdata %>% filter(immunityoffered == 1)
#
# summary(glm(immoffereddata$win ~ immoffereddata$immune+immoffereddata$advTF))
#
# immoffereddata %>%
#   group_by(immune,win) %>%
#   summarise(n=n()) %>%
#   ungroup() %>% group_by(immune) %>%
#   mutate(N=sum(n)
#          ,percent=n/N)
#
# immoffereddata %>%
#   group_by(advTF,win) %>%
#   summarise(n=n()) %>%
#   ungroup() %>% group_by(advTF) %>%
#   mutate(N=sum(n)
#          ,percent=n/N)
#
# ## How did people who had immunity at some point in the season do
# ## when they had immunity versus when they didn't?
# elim <- challdata %>%
#   filter(challengeType %in% "Elimination" &
#            everimm %in% 1)
#
#   elim %>%
#     ungroup() %>%
#     group_by(win,immune) %>%
#     summarise(n=n())
#
#   table(elim$immune,elim$win)
#   chisq.test(elim$immune,elim$win)
#
# ## Advantages?
#   adv <- challdata %>%
#     filter(challengeType %in% "Elimination" &
#              everadv %in% 1)
#
#   adv %>%
#     ungroup() %>%
#     group_by(win,advTF) %>%
#     summarise(n=n())
#
#   table(adv$advTF,adv$win)
#   chisq.test(adv$advTF,adv$win)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# ## Prep data
#   ## Bring in the data
#   challengedescr <- read.csv(paste0(directory
#                                   ,"Top Chef - Challenge descriptions.csv")
#                            ,header = TRUE) %>%
#     filter(series == "US") %>%
#   select(series,season,seasonNumber,episode,challengeType,outcomeType,advantage)
#
#   ## keep just the ones that have an advantage
#   advantages <- challengedescr %>%
#     filter(!(is.na(advantage))) %>%
#     # to make it easier to see the words, remove "advantage in elim chall"
#     mutate(advantage = gsub("Advantage: ",""
#                           ,gsub("Advantage in Elimination: ",""
#                           ,gsub("Advantage in elimination challenge: ",""
#                             ,gsub("Advantage in Elimination challenge: ",""
#                            ,gsub("Advantage in Elimination Challenge: ",""
#                            ,gsub(" Advantage in Elimination Challenge: ",""
#                            ,gsub(" advantage in elimination challenge: ",""
#                             ,advantage)))))))) %>%
#   ## categorize the advantages
#     mutate(AdvantageFlag = 1
#       ,Immunity = ifelse(grepl("mmunity",advantage) |
#                                advantage %in% c("Winner goes straight to finale"
#                                               ,"Automatic spot in the finale") |
#                                grepl("sit out",advantage),1,0)
#            ,ExtraTime = ifelse(grepl("minutes",advantage) ,1,0)
#           ,ExtraMoney = ifelse(grepl("\\$",advantage),1,0)
#            ,ChooseTeam = ifelse(grepl("choose team",advantage) |
#                                   grepl("choose team to join",advantage) |
#                                   grepl("choose your team",advantage),1,0)
#           ,OtherTeamAdvantage = ifelse(grepl("choose team captains",advantage) |
#                                      grepl("choose the team leader",advantage)
#                                        ,1,0)
#           ,AssignDishToOthers = ifelse(grepl("choose the icon",advantage) |
#                                           grepl("choose dish",advantage) |
#                                           grepl("choose course",advantage),1,0)
#            ,IngredientAdvantage = ifelse(grepl("better ingredients",advantage) |
#                                            grepl("choose foods",advantage) |
#                                            grepl("choose fish",advantage) |
#                                            grepl("choose protein",advantage)
#                                          ,1,0))
#   ## merge it onto the challenge data
#
