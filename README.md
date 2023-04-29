---
title: topChef Documentation
author: Carly Levitz
date: 2023-04-29
output: github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo=TRUE)
```

# Introduction to topChef

topChef is a collection of data sets detailing events across all seasons of Top Chef US and Top Chef Masters US and one season of Top Chef Canada. It includes Chef information, challenge descriptions, challenge winners, episode information, guest judge names, and reward/prize information.

# Installation

Not yet on CRAN.

```{r}
devtools::install.packages("topChef")

library(topChef); library(tidyverse)

```

# Dataset overview

## Chef details

A table containing information about Chefs for each season they are in, including placement and gender. For some but not all seasons, there is also information on hometown, current city of residence, age, a flag for whether they are a person of color, and their occupation.

`{ r, echo = FALSE} library(topChef) chefdetails %>% filter(szn == "World All Stars")`

## Challenge descriptions

## Challenge wins

## Judges

## Rewards

## Episode info

# References

Data were collected manually while watching each season of Top Chef. Additional data were collected from <https://en.wikipedia.org/wiki/Top_Chef>. My Top Chef data journey was inspired by <https://topchefstats.com/>.

Huge thanks to <https://github.com/doehm> for all his support!
