---
title: topChef Documentation
author: Carly Levitz
date: 2023-04-29
output: github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo=TRUE)
```


## Introduction to topChef

topChef is a collection of data sets detailing events across all seasons of Top Chef US and Top Chef Masters US and one season of Top Chef Canada. It includes Chef information, challenge descriptions, challenge winners, episode information, guest judge names, and reward/prize information.

## Installation

Not yet on CRAN. So please use: *devtools::install.packages("celevitz/topChef")*


## References & Acknowlegements

Data were collected manually while watching each season of Top Chef. Additional data were collected from <https://en.wikipedia.org/wiki/Top_Chef>. My Top Chef data journey was inspired by <https://topchefstats.com/>.

Huge thanks to <https://github.com/doehm> for all his support!

```{r Library set up, message=FALSE, warning=FALSE,echo=FALSE,eval=TRUE}
library(topChef); library(tidyverse)
```

## Dataset overview

Across datasets, key joining variables include:

* `chef`
* `szn`
* `sznnumber`
* `series`
* `episode`

### Chef details

A tibble containing information about Chefs for each season they are in, including placement and gender. For some but not all seasons, there is also information on hometown, current city of residence, age, a flag for whether they are a person of color, and their occupation.

```{r Chef details table , eval=TRUE} 
as_tibble(chefdetails )
```

### Challenge descriptions
A tibble containing information about each challenge that the Chefs compete in.
```{r Challenge descriptions table , eval=TRUE} 
as_tibble(challengedescriptions )
```

### Challenge wins
A tibble containing win and loss data for each chef in each episode.
```{r Challenge results table , eval=TRUE} 
as_tibble(challengewins) 
```

### Judges
A tibble containing information about who were the guest judges for each challenge.
```{r Guest Judges table , eval=TRUE} 
as_tibble(judges )
```

### Rewards
A tibble containing information about rewards and prizes won by challenge.
```{r Rewards table , eval=TRUE} 
as_tibble(rewards) 
```

### Episode info
A tibble containing information about each episode.
```{r Episode information table , eval=TRUE} 
as_tibble(episodeinfo )
```

