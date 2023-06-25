# June 25, 2023
# Create an image for releasing the first topChef package


library(topChef)
library(ggplot2)
library(ggtext)
library(tidyverse)
library(showtext)
library(ggimage)
library(rmarkdown)

font_add("fa-brands", regular = "/Users/carlylevitz/downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf")
font_add("fa-regular", regular = "/Users/carlylevitz/downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Free-Regular-400.otf")
font_add("fa-solid", regular = "/Users/carlylevitz/downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Free-Solid-900.otf")
font_add_google("Roboto", "rob")
font_add_google("Barlow", "bar")
showtext_auto()
ft <- "rob"
ft1 <- "bar"

dark <- "#4b4f42"
mid2 <- "#6b625c"
mid <- "gray60"
main <- "orange"
light <- "#f0e0cc"
highlight <- "#46bdc6"

logo <- "~/topChef/dev/releaseAnnouncements/topCheflogo.png"
twitter <- str_glue("<span style='font-family:fa-brands; color:{highlight}'>&#xf099;</span>")
github <- str_glue("<span style='font-family:fa-brands; color:{highlight}'>&#xf09b;</span>")

axistitlesize <- 20
axissize <- 7
titlesize <- 32
subtitlesize <- 28
captionsize <- 15
textlabelsize <- 2
plotmarginL <- 1
plotmarginR <- 1
plotmarginT <- 1
plotmarginB <- 1
titlemarginL <- 1
titlemarginR <- 1
titlemarginT <- 1
titlemarginB <- 1

### Data
info <- chefdetails %>%
  select(series,season,seasonNumber) %>%
  distinct() %>%
  group_by(series) %>%
  summarise(count=n()) %>%
  mutate(image=logo)

  numMastersseasons <- info$count[info$series == "US Masters"]
  numCanadaseasons <- info$count[info$series == "Canada"]
  numUSseasons <- info$count[info$series == "US"]

totalchefsperseries <- chefdetails %>%
  group_by(series) %>%
  summarise(count=n())

chefswhohavecompetedmultipletimes <- chefdetails %>%
  group_by(series,chef) %>%
  summarise(count=n()) %>%
  filter(count > 1) %>%
  ungroup() %>% group_by(series) %>%
  summarise(count=n())


numberofchallenges <- challengewins %>%
  select(series,season,seasonNumber,challengeType,episode) %>%
  distinct() %>%
  group_by(series,challengeType) %>%
  summarise(count=n())


### graphing
gfinal <-
ggplot(info, aes(x=series,y=count)) +
  #geom_image(aes(x=1,y=1,image=logo),size=.1) +
  labs(title=str_wrap(str_glue("A package in R: {numUSseasons} seasons of Top
                               Chef US, {numMastersseasons} seasons of Top
                               Chef Masters, and {numCanadaseasons} season of
                               Top Chef Canada"),40)
       ,subtitle=str_glue("Package written by Carly Levitz {twitter}
                          @carlylevitz")
       ,caption=str_wrap(str_glue("Install from CRAN (topChef) or from {github}
                                  github.com/celevitz/topChef"),40)) +
  theme_minimal() +
  theme(panel.grid = element_blank()
        ,panel.background = element_rect(color="white")
        ,plot.background = element_rect(color="white")
        ,axis.text = element_blank()
        ,axis.line = element_blank()
        ,axis.ticks = element_blank()
        ,axis.title = element_blank()
        ,plot.title = element_text(size=titlesize,face="bold",family=ft
                                   ,color=main
                                   # ,margin=margin(t=titlemarginT
                                   #                          ,r=titlemarginR
                                   #                          ,b=titlemarginB
                                   #                          ,l=titlemarginL)
                                   )
        ,plot.subtitle = element_markdown(size=subtitlesize,family=ft,color=mid)
        ,plot.caption = element_markdown(size=captionsize,family=ft
                                         ,color=highlight)
        ,plot.margin = margin(t=plotmarginT,r=plotmarginR,b=plotmarginB
                              ,l=plotmarginL))



ggsave(plot = gfinal, filename = "~/topChef/dev/releaseAnnouncements/releasev010.png", height = 10, width = 10)

