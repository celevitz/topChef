
library(testthat)        # load testthat package
library(topChef)         # load my package

# 1. Check that there aren't NAs where there shouldn't be
  ## A. Chef Details
    # NAs are okay in: hometown, city, state, age, poc, occupation,
    # and placement in season 21
      for (varname in c("name","chef","season","seasonNumber","series"
                        ,"gender")) {

        test_that(paste("There are no NAs in",varname,sep=" "), {
          expect_equal(all(!(is.na(chefdetails[,varname]))),TRUE)
        })

      }
      test_that("Everyone has a placement",
                {expect_equal(all(!(is.na(chefdetails$placement[chefdetails$seasonNumber!=21]))),TRUE)})

  ## B. Challenge descriptions
      # NAs are okay in challenge description (this will change over time as
      # I fill them in),shop time, shop budget, prep time, cook time, product
      # placement, advantage, LCK stuff & RW stuff
      for (varname in c("season","seasonNumber","series","episode"
                        ,"challengeType","outcomeType")) {

        test_that(paste("There are no NAs in",varname,sep=" "), {
          expect_equal(all(!(is.na(challengedescriptions[,varname]))),TRUE)
        })
      }
  ## C. Challenge wins
      # NAs are okay in rating
      for (varname in c("season","seasonNumber","series","episode",
                        "inCompetition","chef","challengeType")) {

        test_that(paste("There are no NAs in",varname,sep=" "), {
          expect_equal(all(!(is.na(challengewins[,varname]))),TRUE)
        })
      }
      # NA is okay in outcome only if in.competition is false
      test_that("The NAs in Outcome are only if they aren't in the competition"
                , {
        expect_equal(all(!(is.na(challengewins$outcome[challengewins$
                                             inCompetition == "TRUE"]))),TRUE)
      })
  ## D. Episode info
        # NAs are okay in overall episode number, episode name, air date as I
        # work towards complete data
        for (varname in c("season","seasonNumber","series","episode"
                          ,"nCompetitors")) {

          test_that(paste("There are no NAs in",varname,sep=" "), {
            expect_equal(all(!(is.na(episodeinfo[,varname]))),TRUE)
          })
        }
  ## E. Judges
        ## NAs are okay for the competed on TC and other shows variable
        for (varname in c("season","seasonNumber","series","episode"
                          ,"challengeType","outcomeType","guestJudge")) {

          test_that(paste("There are no NAs in",varname,sep=" "), {
            expect_equal(all(!(is.na(judges[,varname]))),TRUE)
          })
        }
  ## F. Rewards
        ## NAs are not okay
        for (varname in c("season","seasonNumber","series","episode"
                          ,"challengeType","outcomeType","rewardType","reward"
                          ,"chef")) {

          test_that(paste("There are no NAs in",varname,sep=" "), {
            expect_equal(all(!(is.na(rewards[,varname]))),TRUE)
          })
        }

## 2. Test Index Calculation
  ## A. Does it output a data frame?
        test_that("index calculation returns a data frame", {
          indexoutput <- weightedindex(seriesname="US",seasonnumberofchoice=20
                                       ,numberofelimchalls=10
                                       ,numberofquickfires=8)
          expect_s3_class(indexoutput,"data.frame")
        })


## 3. In each dataset, is there only one season number associated with a season name?
  ## A. Rewards
        for (seasonname in unique(rewards$season)) {
          test_that(paste("In rewards data, there is only one season # associated with",seasonname,sep=" "), {
            expect_equal(length(unique(rewards$seasonNumber[rewards$season == seasonname])),1)
          })
        }

  ## B. Judges
        for (seasonname in unique(judges$season)) {
          test_that(paste("In judges data, there is only one season # associated with",seasonname,sep=" "), {
            expect_equal(length(unique(judges$seasonNumber[judges$season == seasonname])),1)
          })
        }
  ## C. Episode info
        for (seasonname in unique(episodeinfo$season)) {
          test_that(paste("In episodeinfo data, there is only one season # associated with",seasonname,sep=" "), {
            expect_equal(length(unique(episodeinfo$seasonNumber[episodeinfo$season == seasonname])),1)
          })
        }
  ## D. Chef details
        for (seasonname in unique(chefdetails$season)) {
          test_that(paste("In chefdetails data, there is only one season # associated with",seasonname,sep=" "), {
            expect_equal(length(unique(chefdetails$seasonNumber[chefdetails$season == seasonname])),1)
          })
        }
  ## E. Challenge wins
        for (seasonname in unique(challengewins$season)) {
          test_that(paste("In challengewins data, there is only one season # associated with",seasonname,sep=" "), {
            expect_equal(length(unique(challengewins$seasonNumber[challengewins$season == seasonname])),1)
          })
        }
  ## F. Challenge descriptions
        for (seasonname in unique(challengedescriptions$season)) {
          test_that(paste("In challengedescriptions data, there is only one season # associated with",seasonname,sep=" "), {
            expect_equal(length(unique(challengedescriptions$seasonNumber[challengedescriptions$season == seasonname])),1)
          })
        }






