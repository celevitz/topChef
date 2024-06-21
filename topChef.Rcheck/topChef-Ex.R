pkgname <- "topChef"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('topChef')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("challengedescriptions")
### * challengedescriptions

flush(stderr()); flush(stdout())

### Name: challengedescriptions
### Title: challengedescriptions
### Aliases: challengedescriptions
### Keywords: datasets

### ** Examples

library(dplyr)
library(tidyr)
challengedescriptions %>%
   group_by(series,season,outcomeType) %>%
   summarise(n=n()) %>%
   pivot_wider(names_from=outcomeType,values_from=n)



cleanEx()
nameEx("challengewins")
### * challengewins

flush(stderr()); flush(stdout())

### Name: challengewins
### Title: challengewins
### Aliases: challengewins
### Keywords: datasets

### ** Examples

library(dplyr)
library(tidyr)
challengewins %>%
  group_by(outcome) %>%
  summarise(n=n())



cleanEx()
nameEx("chefdetails")
### * chefdetails

flush(stderr()); flush(stdout())

### Name: chefdetails
### Title: chefdetails
### Aliases: chefdetails
### Keywords: datasets

### ** Examples

library(dplyr)
library(tidyr)
chefdetails %>%
  filter(season == "World All Stars")



cleanEx()
nameEx("episodeinfo")
### * episodeinfo

flush(stderr()); flush(stdout())

### Name: episodeinfo
### Title: episodeinfo
### Aliases: episodeinfo
### Keywords: datasets

### ** Examples

library(dplyr)
library(tidyr)
episodeinfo %>% filter(season=="World All Stars")



cleanEx()
nameEx("judges")
### * judges

flush(stderr()); flush(stdout())

### Name: judges
### Title: judges
### Aliases: judges
### Keywords: datasets

### ** Examples

library(dplyr)
library(tidyr)
judges %>%
  filter(guestJudge == "Eric Ripert") %>%
  group_by(challengeType) %>%
  summarise(n=n())



cleanEx()
nameEx("rewards")
### * rewards

flush(stderr()); flush(stdout())

### Name: rewards
### Title: rewards
### Aliases: rewards
### Keywords: datasets

### ** Examples

library(dplyr)
library(tidyr)
rewards %>%
  filter(rewardType == "Money") %>%
  mutate(reward=as.numeric(reward)) %>%
  group_by(season) %>%
  summarise(total=sum(reward))



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
