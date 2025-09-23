## Bring in data from Excel file

rm(list=ls())

library(openxlsx)
library(tidyverse)

directory <- "/Users/carlylevitz/Documents/Data/"

chefdetails <- as_tibble(read.xlsx(paste(directory
                                         ,"TopChefData.xlsx",sep=""),sheet=1))

# Keep just the winners of the US version
chefdetails <- chefdetails %>% filter(placement == "1.0" & series == "US")

# Who is missing data?
  chefdetails %>%
    mutate(missingData= case_when(
            is.na(Sign) & is.na(Animal) ~ "Missing all data"
            ,is.na(Sign) & !(is.na(Animal)) ~ "Missing Western astrology"
            ,!(is.na(Sign)) & is.na(Animal) ~ "Missing Chinese astrology"
            ,TRUE ~ "No missing data" )  ) %>%
    filter(missingData != "No missing data") %>%
    select(chef,seasonNumber,missingData) %>%
    arrange(missingData,seasonNumber)

# Counts of each astrological variable
  for (var in c("Sign","Polarity","Modality","Triplicity"
                ,"Northern.Hemisphere.Season","Animal","Yin.Yang","Element")) {
    print(var)
    print(table(chefdetails[,var]))
  }



