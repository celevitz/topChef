# See the difference between scores, using S22 as an example
rm(list=ls())
library(tidyverse)
library(ggplot2)


seriesname<- "US"
seasonnumberofchoice <- 22
numberofchefsofinterest <- 13
scoringsystem <- "original"

numberofquickfires <- 3
numberofelimchalls <- 3

comp <- weightedindex(seriesname,seasonnumberofchoice
                                ,numberofquickfires,numberofelimchalls) %>%
  select(chef,indexWeight) %>%
  left_join(ScoresThroughAGivenNumberOfChefs(seriesname,seasonnumberofchoice
                                  ,numberofchefsofinterest, scoringsystem) %>%
        rename(basedonnumchefs=points)) %>%
  left_join(ScoresThroughAGivenNumberOfChefs(seriesname,seasonnumberofchoice
                                     ,numberofchefsofinterest, "modified") %>%
              rename(basedonnumchefs_modified=points))

## Visualize the differences
## The new version in navy blue SHOULD be the same as in orange because of how
## I set up the parameters. I don't know why it's not.
## also, why would Shuai's oringal index weight be greater than the other 2?
comp %>%
  select(chef,indexWeight,basedonnumchefs,basedonnumchefs_modified) %>%
  pivot_longer(!chef,names_to="measure",values_to="value") %>%
  ggplot(aes(x=value,y=chef,color=measure,shape=measure)) +
  geom_point(size=3) +
  scale_shape_manual(values=c(5,4,3)) +
  scale_color_manual(values=c("navyblue","forestgreen","orange"))





