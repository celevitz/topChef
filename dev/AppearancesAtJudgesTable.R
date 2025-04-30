rm(list=ls())
library(tidyverse)
library(openxlsx)
library(topChef)
library(ggplot2)
library(devtools)
#devtools::install_github("celevitz/topChef")

directory <- "/Users/carlylevitz/Documents/Data/topChef/"

challengewins <- read.csv(paste0(directory,"Top Chef - Challenge wins.csv"))%>%
  filter(series == "US")

chefdetails <- read.csv(paste0(directory,"Top Chef - Chef details.csv"))  %>%
  filter(series == "US" )

# Prior to the Double Elimination at 12 chefs: 4 elims and 4 QFs
    episodenumber <- 7
    numberofelimchalls <- 7
    numberofquickfirechalls <- 6
    eliminatedchefs <- c("Anya El-Wattar","Zubair Mohajir"
                         ,"Mimi Weissenborn","Sam Olayinka"
                         ,"Ying Gao","Katianna Hong","Corwin Hemming"
                         ,"Kat Turner","Henry Lu")

## Index
## Write it out here, because it calls on the Top Chef package and that's
    # not working right now


    weightedindex <- function(seriesname,seasonnumberofchoice,numberofelimchalls
                              ,numberofquickfires) {
      # 1. Set up the data
      placementdata <- topChef::chefdetails[,c("chef","series","season"
                                               ,"seasonNumber","placement")]
      placementdata <- placementdata[placementdata$series == seriesname &
                                       placementdata$seasonNumber == seasonnumberofchoice,]


      # 1a. Outcomes of the challenges
      challengewins <-
        challengewins[,names(challengewins)[!(names(challengewins) %in% "rating")] ]
      challengewins <- challengewins[challengewins$series == seriesname &
                                       challengewins$seasonNumber == seasonnumberofchoice,]

      # 1ai. combine types of challenges
      # because there could be both a SDQ & an elimination in an episode, and I
      # use episode as a row ID, I need to make sure that we keep those challenge
      # results separate from the other elimination challenges in that episode
      challengewins$episodeascharacter <- as.character(challengewins$episode)
      challengewins$episodeascharacter[nchar(challengewins$episodeascharacter)==1] <-
        paste0("0",challengewins$episodeascharacter[nchar(challengewins$episodeascharacter)==1])

      challengewins$challID <- paste0(challengewins$episodeascharacter,"a_",challengewins$challengeType)
      challengewins$challID[challengewins$challengeType == "Quickfire Elimination"] <-
        paste0(challengewins$episodeascharacter[challengewins$challengeType == "Quickfire Elimination"] ,"b_qe")
      challengewins$challID[challengewins$challengeType == "Sudden Death Quickfire"] <-
        paste0(challengewins$episodeascharacter[challengewins$challengeType == "Sudden Death Quickfire"] ,"b_sdq")
      challengewins$challID[challengewins$challengeType == "Quickfire"] <-
        paste0(challengewins$episodeascharacter[challengewins$challengeType == "Quickfire"] ,"a_qf")
      challengewins$challID[challengewins$challengeType == "Elimination"] <-
        paste0(challengewins$episodeascharacter[challengewins$challengeType == "Elimination"] ,"c_elim")

      challengewins$challengeType[challengewins$challengeType %in%
                                    c("Quickfire Elimination"
                                      ,"Sudden Death Quickfire")] <- "Elimination"

      # 1aii. Exclude the uncommon challenge types
      challengewins <- challengewins[!(challengewins$challengeType %in%
                                         c("Battle of the Sous Chefs"
                                           ,"Qualifying Challenge")),]

      # 1aiii. clean up outcomes: consolidate
      challengewins$outcome[challengewins$outcome %in% c("High","HiGH")] <-
        "HIGH"
      challengewins$outcome[grepl("LOW",challengewins$outcome)] <- "LOW"
      challengewins$outcome[challengewins$outcome %in%
                              c("DISQUALIFIED","RUNNER-UP","WITHDREW") |
                              grepl("OUT",challengewins$outcome) ] <- "OUT"
      challengewins$outcome[challengewins$outcome %in% c("DIDN'T COMPETE") |
                              grepl("N/A",challengewins$outcome) |
                              grepl("QUALIFIED",challengewins$outcome) ] <- "IN"
      challengewins$outcome[challengewins$outcome %in% c("WINNER")] <- "WIN"

      # 1b. need to consecutively number each challenge of each challenge type
      challnum_temp <- unique(challengewins[, c("season","seasonNumber"
                                                ,"challengeType","episode"
                                                ,"episodeascharacter","challID")])
      challnum_temp <- challnum_temp[order(challnum_temp$seasonNumber
                                           ,challnum_temp$episode
                                           ,challnum_temp$challID
                                           ,desc(challnum_temp$challengeType)),]

      # not all challenges that we're looking at start in episode 1
      # and, some episode 1s don't have a quickfire
      # order of eliminations
      elimorder <- unique(challnum_temp[challnum_temp$challengeType == "Elimination",c("challengeType","episode","challID")])
      row.names(elimorder) <- NULL
      elimorder$count <- row.names(elimorder)
      elimorder$challengeType <- "Elimination"
      elimorder$seasonNumber <- seasonnumberofchoice

      # order of quickfires
      qforder <- unique(challnum_temp[challnum_temp$challengeType == "Quickfire",c("challengeType","episode","challID")])
      row.names(qforder) <- NULL
      qforder$count <- row.names(qforder)
      qforder$challengeType <- "Quickfire"
      qforder$seasonNumber <- seasonnumberofchoice

      challnum <- rbind(elimorder,qforder)

      # if an elimination and quickfire don't happen in ep 1
      # need to add a row of quickfire
      # if the first episode of a quickfire is after the first episode of an elim,
      # then make the count 0 until it is the episode of the first quickfire
      firstelim_ep <- min(challnum$episode[challnum$challengeType == "Elimination"])
      firstqf_ep <- min(challnum$episode[challnum$challengeType == "Quickfire"])

      for (tempcount in seq(firstelim_ep, firstqf_ep,1)) {
        if (tempcount < firstqf_ep) {
          newrow <- data.frame(cbind("Quickfire",tempcount
                                     ,paste0("0",as.character(tempcount),"a_qf")
                                     ,0,seasonnumberofchoice
          ) )
          names(newrow) <- names(challnum)
          challnum <- rbind(challnum,newrow )
        }
      }

      # 1bi. keep just the challenges that meet the criteria
      # update: in case the numbers are vastly different (e.g., 10 elim challs & 0 Qfs)
      # need to instead of having: an OR statement about keeping things if the
      # count is less than or = to the number of elim challs select OR
      # count is less than or = to the # of qf challs
      # instead going to append the datasets together

      challnum$count <- as.numeric(challnum$count)

      challkeep <- rbind(challnum[challnum$count <= numberofelimchalls &
                                    challnum$challengeType == "Elimination" &
                                    !(is.na(challnum$count)),]
                         ,challnum[(challnum$count <= numberofquickfires &
                                      challnum$challengeType == "Quickfire" &
                                      !(is.na(challnum$count))) ,] )

      ## 2. get the number of wins, losses, highs, etc. by chef
      statsbynumberofchalls <- merge(challengewins,placementdata,by=c("series"
                                              ,"season","seasonNumber","chef"))
      statsbynumberofchalls <- merge(statsbynumberofchalls,challkeep,by=
                                       c("seasonNumber","challengeType"
                                         ,"episode","challID"))

      # 2a. keep just the episodes that are at or the same # of challenges
      # that have happened
      statsbynumberofchalls <- statsbynumberofchalls[!(is.na(
        statsbynumberofchalls$count)),]

      # 2b. drop when people were not in the competition
      statsbynumberofchalls <- statsbynumberofchalls[statsbynumberofchalls$
                                                       inCompetition == "TRUE",]

      # 2c. drop unnecessary variables
      statsbynumberofchalls <- statsbynumberofchalls[,c("season","seasonNumber"
                                                        ,"series","challengeType","chef","outcome","placement")]


      # 3. create summary stats by chef
      # get counts of wins, highs, and lows
      # don't combine outs & lows because we want to count those differently
      # in the index
      # for the currently airing season, they don't yet have a placement;
      # they'll get dropped in this aggregate function if they are empty;
      # fill them in for now
      statsbynumberofchalls$placement[is.na(statsbynumberofchalls$placement)] <-
        1.5
      statsbynumberofchalls$tempcount <- 1
      statsbynumberofchalls <- aggregate(statsbynumberofchalls$tempcount
                                         ,by=list(statsbynumberofchalls$chef
                                                  ,statsbynumberofchalls$season
                                                  ,statsbynumberofchalls$seasonNumber
                                                  ,statsbynumberofchalls$series
                                                  ,statsbynumberofchalls$challengeType
                                                  ,statsbynumberofchalls$outcome
                                                  ,statsbynumberofchalls$placement)
                                         ,FUN=sum)
      names(statsbynumberofchalls) <- c("chef","season","seasonNumber","series"
                                        ,"challengeType","outcome","placement"
                                        ,"count")

      # 3a. reshape the data
      # we don't need the "IN"/safe counts because we are holding constant
      # the number of challenges done
      statsbynumberofchalls <-
        statsbynumberofchalls[statsbynumberofchalls$outcome != "IN",]
      statsbynumberofchalls <- reshape(statsbynumberofchalls,
                                       timevar = "challengeType",
                                       idvar = c("chef","season","seasonNumber"
                                                 ,"series","outcome"
                                                 ,"placement"),
                                       direction = "wide")
      statsbynumberofchalls <- reshape(statsbynumberofchalls,
                                       timevar = "outcome",
                                       idvar = c("chef","season","seasonNumber"
                                                 ,"series","placement"),
                                       direction = "wide")
      names(statsbynumberofchalls) <- gsub("count.",""
                                           ,names(statsbynumberofchalls))
      statsbynumberofchalls$Quickfire.OUT <- NULL

      # 3b. if NA, replace with 0
      for (var in c(names(statsbynumberofchalls)[!(names(statsbynumberofchalls)
                                                   %in% c("chef","season","seasonNumber","series"
                                                          ,"placement"))])) {
        statsbynumberofchalls[is.na(statsbynumberofchalls[,var]),var] <- 0
      }

      # 3c. How many times are they at jduges table?
      statsbynumberofchalls$judgestableQF <-statsbynumberofchalls$Quickfire.WIN+
        statsbynumberofchalls$Quickfire.HIGH+statsbynumberofchalls$Quickfire.LOW
      statsbynumberofchalls$judgestableElim <-
        statsbynumberofchalls$Elimination.WIN+
        statsbynumberofchalls$Elimination.HIGH+
        statsbynumberofchalls$Elimination.LOW +
        statsbynumberofchalls$Elimination.OUT
      statsbynumberofchalls$judgestableElimPercent <-
        statsbynumberofchalls$judgestableElim/numberofelimchalls

      # 4. get the index
      statsbynumberofchalls$indexWeight <-
        statsbynumberofchalls$Elimination.WIN*7+
        statsbynumberofchalls$Elimination.HIGH*3 -
        statsbynumberofchalls$Elimination.LOW*3-
        statsbynumberofchalls$Elimination.OUT*7 +
        statsbynumberofchalls$Quickfire.WIN*4+
        statsbynumberofchalls$Quickfire.HIGH*2-
        statsbynumberofchalls$Quickfire.LOW*2

      statsbynumberofchalls
    }



#############################################################################
## Now play around

  ## All seasons: what % of the time are people at the Judges table?
    allseasons <- weightedindex("US",22
                                ,numberofelimchalls,numberofquickfirechalls)
    for (sn in 1:21) {
      allseasons <- allseasons %>%
        bind_rows(weightedindex("US",sn
                                ,numberofelimchalls,numberofquickfirechalls))
    }
    allseasons$percentofjudgestableHigh <- (allseasons$Elimination.HIGH +
                                allseasons$Elimination.WIN)/(allseasons$Elimination.HIGH +
                               allseasons$Elimination.WIN + allseasons$Elimination.LOW +
                                   allseasons$Elimination.OUT)
    summary(allseasons$judgestableElimPercent)

    # Average ranges from 32.8% to 58.3%
    # Six seasons have at least 1 person who was at judges table 100%
    # Only 1 season had someone at 0%
    averages <- allseasons %>%
      group_by(season,seasonNumber) %>%
      summarise(mean=mean(judgestableElimPercent)
                ,median=median(judgestableElimPercent)
                ,min=min(judgestableElimPercent)
                ,max=max(judgestableElimPercent)) %>%
      arrange(desc(mean),desc(median))

    averagetrend <- averages %>%
      ggplot(aes(x=seasonNumber,y=mean,label=paste0(round(mean*100,1),"%"))) +
      geom_point(shape=18,size=2) + geom_text(nudge_y=0.03,cex=1.1) +
      ggtitle("Seven elimination challenges in, on average, what\npercent of the time did chefs appear at judges' table?"
              ,subtitle = "Appearing at judges' table means they were at the top or bottom\nof the group (including wins and elimimations") +
      scale_y_continuous("average % of challenges at judges' table"
                         ,breaks = seq(0,.7,.1), limits = c(0,.7)
                         ,labels = paste0(as.character(seq(0,.7,.1)*100),"%"))+
      scale_x_continuous("Season number",breaks=seq(1,21,1),limits = c(1,21)) +
      labs(caption = "Created by Carly Levitz for Pack Your Knives")+
      theme_minimal() + theme(panel.grid = element_blank()
            ,panel.background = element_rect(color="white")
            ,plot.background = element_rect(color="white")
            ,axis.ticks = element_line(color="black")
            ,axis.line = element_line(color="black")
            ,axis.text = element_text(color="black",size=5)
            ,axis.title=element_text(size=5,color="black")
            ,title = element_text(size=7,color="black")
            ,plot.caption = element_text(size=3,color="black")  )

    ggsave(paste0(directory,"AverageAppearanceAtJudgesTable.png")
           ,averagetrend,width = 3.25,height = 2.6,dpi = 1200 )

  ## Winners' stats
    ## Winners and S22
    winnersand22 <- allseasons %>%
      filter(as.numeric(placement) == 1 | seasonNumber == 22) %>%
      mutate(winner = ifelse(as.numeric(placement) == 1, "Winner","S22")
             # not all the names are going to be plotted
             ,label=ifelse(chef %in% c("Henry Lu","Kevin Sb.","Stephanie I."
                                       ,"Michael V.","Richard B.","Joe F."
                                       ,"Mimi Weissenborn","Mei L."),chef,NA)
             ,label2=case_when(chef %in% "Ilan H." ~ "Ilan H., Nicholas E.,\nKelsey B.-C., Buddha L."
                               ,chef %in% "Anya El-Wattar" ~ "Anya E., Bailey S.,\nLana L., Massimo P.,\nZubair M."
                               ,chef %in% "Paul Q." ~ "Paul Q., Kristen K.,\nJeremy F., Danny G."
                               ,chef %in% "Paula Endara" ~ "Paula E., Kat T."
                               ,chef %in% "Melissa K." ~ "Melissa K.,\nGabe E."
                               ,chef %in% "Cesar Murillo" ~ "Cesar M., Katianna H.\nVinny L., Corwin H."
                               ,TRUE ~ NA))
        # remove people's last names for S22
          winnersand22$label[winnersand22$chef %in% "Henry Lu"] <- "Henry L."
          winnersand22$label[winnersand22$chef %in% "Mimi Weissenborn"] <- "Mimi W."

    winnersand22summ <- winnersand22 %>%
      group_by(winner,judgestableElim,percentofjudgestableHigh) %>%
      summarise(n=n())

      # Just winners
        temp <- winnersand22 %>%
          ggplot(aes(x=judgestableElim,y=percentofjudgestableHigh))  +
          #geom_point(data = winnersand22 %>%
          #             filter(is.na(label) & is.na(label2))
          #             ,aes(alpha = .7,color=winner,shape=winner)) +
          geom_text(aes(label=label,color=winner),cex=1.1) +
          geom_text(aes(label=label2,color=winner),cex=1) +
          ggtitle("Times at judges' table for being in the top out of seven\nelimination challenges"
                ,subtitle = "Appearing at judges' table means they were at the top or bottom\nof the group (including wins and elimimations)") +
          scale_y_continuous("% of time they were there for being at the top"
                         ,breaks = seq(0,1,.1), limits = c(0,1.01)
                         ,labels = paste0(as.character(seq(0,1,.1)*100),"%"))+
          scale_x_continuous("Times at judges' table out of 7 elimination challenges"
                             ,breaks=seq(1,7,1),limits = c(0.8,7.1) ) +
          scale_color_manual(values=c("orange","blue")) +
          labs(caption = "Created by Carly Levitz for Pack Your Knives")+
          theme_minimal() + theme(panel.grid = element_blank()
                          ,panel.background = element_rect(color="white")
                          ,plot.background = element_rect(color="white")
                          ,axis.ticks = element_line(color="black")
                          ,axis.line = element_line(color="black")
                          ,axis.text = element_text(color="black",size=5)
                          ,axis.title=element_text(size=5,color="black")
                          ,title = element_text(size=7,color="black")
                          ,plot.caption = element_text(size=3,color="black")
                          ,legend.title = element_blank()
                          ,legend.text = element_text(size =4, color="black")) +
          guides(alpha="none")
          ggsave(paste0(directory,"AppearanceAtJudgesTable_Winner.png")
               ,temp,width = 3.25,height = 2.6,dpi = 1200 )

          winnersand22 %>%
            filter(is.na(label) & seasonNumber == 22 & judgestableElim == 3) %>%
            arrange(desc(percentofjudgestableHigh)) %>%
            select(chef,judgestableElim,percentofjudgestableHigh)











          #+
          #annotate("text",x=2,y=0.4,color="orange",cex=1,label="Anya E., Bailey S.,\nLana L., Massimo P.,\nZubair M.") +
          #annotate("text",x=3,y=0.26,color="orange",cex=1,label="Paula E.,\nKat T.")+
          #annotate("text",x=3.3,y=0.33,color="blue",cex=1,label="Joe F.") +
          #annotate("text",x=2.3,y=0.67,color="blue",cex=1,label="Ilan H., Nicholas E.,\nKelsey B.-C., Buddha L.") +
          #annotate("text",x=3.7,y=0.67,color="orange",cex=1,label="Cesar M., Katianna H.\nVinny L., Corwin H.") +
          #annotate("text",x=3,y=.95,color="blue",cex=1,label="Paul Q., Kristen K.,\nJeremy F., Danny G.") +
          #annotate("text",x=4,y=.95,color="blue",cex=1,label="Melissa K.,\nGabe E.") +
          #annotate("text",x=4.4,y=1,color="orange",cex=1,label="Tristen E.") +
          #annotate("text",x=3.4,y=.75,color="blue",cex=1,label="Hosea R., Buddha L.")


          #winnersand22$label[winnersand22$chef %in% "Cesar Murillo"] <- "Cesar M."
          #winnersand22$label[winnersand22$chef %in% "Henry Lu"] <- "Henry L."
          #winnersand22$label[winnersand22$chef %in% "Katianna Hong"] <- "Katianna H."
          #winnersand22$label[winnersand22$chef %in% "Lana Lagomarsini"] <- "Lana L."
          #winnersand22$label[winnersand22$chef %in% "Paula Endara"] <- "Paula E."
          #winnersand22$label[winnersand22$chef %in% "Shuai Wang"] <- "Shuai W."
          #winnersand22$label[winnersand22$chef %in% "Tristen Epps"] <- "Tristen E."
          #winnersand22$label[winnersand22$chef %in% "Vincenzo Loseto"] <- "Vinny L."
          #winnersand22$label[winnersand22$chef %in% "Massimo Piedimonte"]<-"Massimo P."
          #winnersand22$label[winnersand22$chef %in% "Bailey Sullivan"] <- "Bailey S."
          #winnersand22$label[winnersand22$chef %in% "Kat Turner"] <- "Kat T."
          #winnersand22$label[winnersand22$chef %in% "Corwin Hemming"] <- "Corwin H."
          #winnersand22$label[winnersand22$chef %in% "Zubair Mohajir"] <- "Zubair M."
          #winnersand22$label[winnersand22$chef %in% "Anya El-Wattar"] <- "Anya E."
          #winnersand22$label[winnersand22$chef %in% "Mimi Weissenborn"] <- "Mimi W."
          #winnersand22$label[winnersand22$chef %in% "Danny Garcia"] <- "Danny G."

    ## What about people who have been in the top super irregularly
          irregular <- allseasons %>%
            filter(judgestableElim == 2) %>%
            select(chef,season,seasonNumber,placement,judgestableElim
                   ,percentofjudgestableHigh) %>%
            mutate(placement=as.numeric(placement))

          nrow(irregular %>%
                 filter(placement > 8))

          irregular %>%
            group_by(placement) %>%
            summarise(n=n())

          summary(irregular %>%
                    filter(placement != 1.5 & placement <=8) %>%
                    select(placement))






    # four seasons have outliers (3, 7, 11, 22)
    allseasons %>%
    ggplot(aes(x=seasonNumber,y=judgestableElimPercent)) +
      geom_boxplot(aes(group=seasonNumber)) #+
      #geom_jitter(color="blue")

    limited <- allseasons %>%
      group_by(season,seasonNumber) %>%
      mutate(seasonaverage = mean(judgestableElimPercent,na.rm=T)) %>%
      filter((seasonNumber == 3 & judgestableElimPercent == 1 ) |
               (seasonNumber == 7 & judgestableElimPercent >=.7) |
               (seasonNumber == 11 & judgestableElimPercent >=.75) |
               (seasonNumber == 22 & judgestableElimPercent >=.7) |
               as.numeric(placement) == 1) %>%
      mutate(outlier = case_when((seasonNumber == 3 & judgestableElimPercent == 1 ) |
                                   (seasonNumber == 7 & judgestableElimPercent >=.7) |
                                   (seasonNumber == 11 & judgestableElimPercent >=.75) |
                                   (seasonNumber == 22 & judgestableElimPercent >=.7) ~ "Outlier"
                                 ,TRUE ~ "Not outlier")
             ,winner = ifelse(as.numeric(placement) == 1, "Winner","Non-winner")) %>%
      select(chef,season,seasonNumber,placement,judgestableElimPercent
             ,percentofjudgestableHigh,seasonaverage,outlier,winner)

      limited %>%
        ggplot(aes(x=seasonNumber,y=judgestableElimPercent) )+
        geom_line(aes(x=seasonNumber,y=seasonaverage)) +
        geom_text(aes(x=seasonNumber,y=judgestableElimPercent
                      ,label=chef,color=winner))

      ## Winners and S22
      winnersand22 <- allseasons %>%
        filter(as.numeric(placement) == 1 | seasonNumber == 22) %>%
        mutate(winner = ifelse(as.numeric(placement) == 1, "Winner","S22"))

      winnersand22summ <- winnersand22 %>%
        group_by(winner,judgestableElim,percentofjudgestableHigh) %>%
        summarise(n=n())

      winnersand22 %>%
        ggplot(aes(x=judgestableElim,y=percentofjudgestableHigh
               ))  +
        geom_text(aes(label=chef,alpha=.7,color=winner),cex=3) +
        ylab("Percent of time they were there for being at the top") +
        xlab("Times at judges' table out of 7 elimination challenges") +
        ggtitle("Times at judges table and reason for being there")

      winnersand22 %>%
        ggplot(aes(x=judgestableElim,y=percentofjudgestableHigh
        ))  +
        geom_jitter(aes(color=winner,fill=winner,shape=winner)) +
        ylab("Percent of time they were there for being at the top") +
        xlab("Times at judges' table out of 7 elimination challenges") +
        ggtitle("Times at judges table and reason for being there (jittered)")

      winnersand22summ %>%
        ggplot(aes(x=judgestableElim,y=percentofjudgestableHigh
                   ,color=winner,fill=winner,shape=winner,size=n))  +
        geom_point(alpha=.5) +
        ylab("Percent of time they were there for being at the top") +
        xlab("Times at judges' table out of 7 elimination challenges") +
        ggtitle("Times at judges table and reason for being there")
