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
#   Test Package:              'Cmd + Shift + T'

library(openxlsx); library(tidyverse)


rm(list=ls())

library(tidyverse); library(openxlsx); library(gt)

directory <- "/Users/carlylevitz/Documents/Data/"

chefdetails <- read.xlsx(paste(directory,"TopChefData.xlsx",sep=""),sheet=1)
challengewins <- read.xlsx(paste(directory,"TopChefData.xlsx",sep=""),sheet=2)
challengedescr <- read.xlsx(paste(directory,"TopChefData.xlsx",sep=""),sheet=3)
rewards <- read.xlsx(paste(directory,"TopChefData.xlsx",sep=""),sheet=4)
judges <- read.xlsx(paste(directory,"TopChefData.xlsx",sep=""),sheet=5)
epiinfo <- read.xlsx(paste(directory,"TopChefData.xlsx",sep=""),sheet=6)



