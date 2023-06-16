
library(testthat)        # load testthat package
library(topChef)         # lout my package

# 1. Check that there aren't NAs where there shouldn't be
  ## A. Chef Details
    # NAs are okay in: hometown, city, state, age, poc, occupation, and placement in season 20
      for (varname in c("name","chef","szn","sznnumber","series","gender")) {

        test_that(paste("There are no NAs in",varname,sep=" "), {
          expect_equal(all(!(is.na(chefdetails[,varname]))),TRUE)
        })

      }
      test_that("Everyone has a placement except in Season 20",
                {expect_equal(all(!(is.na(chefdetails$placement[chefdetails$szn != "World All Stars"]))),TRUE)})
  ## B. Challenge descriptions
      # NAs are okay in challenge description (this will change over time as I fill them in),shop time, shop budget, prep time, cook time, product placement, advantage, LCK stuff & RW stuff
      for (varname in c("szn","sznnumber","series","episode","challenge_type","outcome_type")) {

        test_that(paste("There are no NAs in",varname,sep=" "), {
          expect_equal(all(!(is.na(challengedescriptions[,varname]))),TRUE)
        })
      }
  ## C. Challenge wins
      # NAs are okay in rating
      for (varname in c("szn","sznnumber","series","episode","in.competition","chef","challenge_type")) {

        test_that(paste("There are no NAs in",varname,sep=" "), {
          expect_equal(all(!(is.na(challengewins[,varname]))),TRUE)
        })
      }
      # NA is okay in outcome only if in.competition is false
        test_that("The NAs in Outcome are only if they aren't in the competition", {
          expect_equal(all(!(is.na(challengewins$outcome[challengewins$in.competition == "TRUE"]))),TRUE)
        })
  ## D. Episode info
        # NAs are okay in overall episode number, episode name, air date as I work towards complete data
        for (varname in c("szn","sznnumber","series","episode","#.of.competitors")) {

          test_that(paste("There are no NAs in",varname,sep=" "), {
            expect_equal(all(!(is.na(episodeinfo[,varname]))),TRUE)
          })
        }
  ## E. Judges
        ## NAs are okay for the competed on TC and other shows variable
        for (varname in c("szn","sznnumber","series","episode","challenge_type","outcome_type","guestjudge")) {

          test_that(paste("There are no NAs in",varname,sep=" "), {
            expect_equal(all(!(is.na(judges[,varname]))),TRUE)
          })
        }
  ## F. Rewards
        ## NAs are not okay
        for (varname in c("szn","sznnumber","series","episode","challenge_type","outcome_type","reward_type","reward","chef")) {

          test_that(paste("There are no NAs in",varname,sep=" "), {
            expect_equal(all(!(is.na(rewards[,varname]))),TRUE)
          })
        }

## 2. Test Index Calculation
  ## A. Does it output a data frame?
        test_that("index calculation returns a data frame", {
          indexoutput <- weightedindex(seriesname="US",seasonnumber=20,numberofelimchalls=10,numberofquickfires=8)
          expect_s3_class(indexoutput,"data.frame")
        })





