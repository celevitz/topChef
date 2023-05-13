#' @title Calculate One Season's Chef's Weighted Scores (Index)
#'
#' @description Calculates the Index score for each person within a season of Top Chef.
#'
#' @param seriesname Values can be: US, US Masters, Canada
#' @param seasonnumber Integer of the season number within the series
#' @param numberofelimchalls Number of elimination challenges you want to include in the index
#' @param numberofquickfires Number of quickfire challenges you want to include in the index
#'
#' @details The result of elimination challenges and quickfire challenges are weighted.
#'          Scoring: Elimination win = +7 points. Elimination high = +3 points.
#'                   Elimination low = -3 points. Eliminated = -7 points.
#'                   Quickfire win = +4 points. Quickfire high = +2 points.
#'                   Quickfire low = -2 points.
#'          Combines Sudden Death Quickfires with Eliminations
#'          Excludes Qualifying Challenges
#'          Holding constant the number of elimination challenges and quickfire challengess
#'              will allow comparison across seasons if you want
#'
#' @return Tibble of index score for each contestant in that season and their placement
#'
#' @import tidyverse
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#' @importFrom stats filter

#' @importFrom dplyr case_when
#' @importFrom dplyr distinct
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#'
#' @export
#'
#'

weightedindex <- function(seriesname,seasonnumber,numberofelimchalls,numberofquickfires) {
  # Set up the data
    placementdata <- topChef::chefdetails %>%
      select(chef, series,szn,sznnumber,placement) %>%
      filter(series == seriesname & sznnumber == seasonnumber)

    # Outcomes of the challenges
      challengewins <- topChef::challengewins %>%
        filter(series == seriesname & sznnumber == seasonnumber) %>%
        # combine types of challenges
        mutate(challenge_type=case_when(challenge_type %in% c("Quickfire Elimination","Sudden Death Quickfire") ~ "Elimination",
                                        TRUE ~ challenge_type)) %>%
        # Exclude the uncommon challenge types
        filter(!(challenge_type %in% c("Battle of the Sous Chefs","Qualifying challenge"))) %>%
        # clean up outcomes: consolidate
        mutate(outcome=case_when(outcome %in% c("High","HiGH") ~ "HIGH",
                                 grepl("LOW",outcome) ~ "LOW",
                                 grepl("OUT",outcome) | outcome %in% c("DISQUALIFIED","RUNNER-UP","WITHDREW") ~ "OUT",
                                 grepl("N/A",outcome) | grepl("Qualified",outcome) | outcome %in% c("Didn't compete")~ "IN",
                                 outcome %in% c("WINNER") ~ "WIN",
                                 TRUE ~ outcome))
    # need to consecutively number each challenge of each challenge type
        challnum <- challengewins %>%
          select(szn,sznnumber,challenge_type,episode) %>%
          distinct() %>%
          mutate(count=NA)


          tempcount_elim <- 1
          for (e in unique(challnum$episode[challnum$szn == s & challnum$challenge_type == "Elimination"])) {
            challnum$count[challnum$szn == s & challnum$episode == e & challnum$challenge_type == "Elimination"] <- tempcount_elim
            tempcount_elim <- tempcount_elim +1
          }

          tempcount_qf <- 1
          for (e in unique(challnum$episode[challnum$szn == s & challnum$challenge_type == "Quickfire"])) {
            challnum$count[challnum$szn == s & challnum$episode == e & challnum$challenge_type == "Quickfire"] <- tempcount_qf
            tempcount_qf <- tempcount_qf +1
          }

    # keep just the challenges that meet the criteria
        challkeep <- challnum %>%
          filter( (count <= numberofelimchalls & challenge_type == "Elimination") |
                  (count <= numberofquickfires & challenge_type == "Quickfire") )


    ## get the results data
        statsbynumberofchalls <-
          challengewins %>%
          # merge on information about their placement
          left_join(placementdata ) %>%
          # keep just the episodes that are at or the same # of challenges that have happened
          left_join(challkeep) %>%
          filter(!(is.na(count))) %>%
          select(!c(rating,count,in.competition,episode)) %>%
          # create summary stats by chef
          # get counts of wins, highs, and lows
          # don't combine outs & lows because we want to count those differently in the index
          group_by(chef,szn,sznnumber,challenge_type,outcome) %>%
          mutate(tempcount=1,count=sum(tempcount)) %>%
          select(!tempcount) %>%
          distinct() %>%
          # reshape the data
          # we don't need the "IN"/safe counts because we are holding constant the number of challenges done
          filter(outcome != "IN") %>%
          pivot_wider(names_from=challenge_type,values_from=count) %>%
          ungroup() %>%
          pivot_wider(names_from=outcome,values_from=c(Elimination,Quickfire)) %>%
          # if NA, replace with 0
          mutate(Elimination_WIN=ifelse(is.na(Elimination_WIN),0,Elimination_WIN),
                 Elimination_HIGH=ifelse(is.na(Elimination_HIGH),0,Elimination_HIGH),
                 Elimination_LOW=ifelse(is.na(Elimination_LOW),0,Elimination_LOW),
                 Elimination_OUT=ifelse(is.na(Elimination_OUT),0,Elimination_OUT),
                 Quickfire_WIN=ifelse(is.na(Quickfire_WIN),0,Quickfire_WIN),
                 Quickfire_HIGH=ifelse(is.na(Quickfire_HIGH),0,Quickfire_HIGH),
                 Quickfire_LOW=ifelse(is.na(Quickfire_LOW),0,Quickfire_LOW)) %>%
          # it creating an impossible variable -- quickfire out. remove
          select(!Quickfire_OUT) %>%
          # get the index
          mutate(indexWeight=Elimination_WIN*7+
                   Elimination_HIGH*3 -
                   Elimination_LOW*3-
                   Elimination_OUT*7 +
                   Quickfire_WIN*4+
                   Quickfire_HIGH*2-
                   Quickfire_LOW*2)

  print(statsbynumberofchalls)
}



