## What is the result of advantages, and how does it differ by adv. type?
## Carly Levitz
## 10/5/2025

rm(list=ls())

library(tidyverse)
library(openxlsx)
directory <- "/Users/carlylevitz/Documents/Data/topChef/"

chefdetails <- read.xlsx(paste(directory
                                     ,"TopChefData.xlsx",sep=""),sheet=1) %>%
  filter(series == "US")

