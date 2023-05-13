topChef
================

## Introduction to topChef

topChef is a collection of data sets detailing events across all seasons
of Top Chef US and Top Chef Masters US and one season of Top Chef
Canada. It includes Chef information, challenge descriptions, challenge
winners, episode information, guest judge names, and reward/prize
information.

## Installation

Not yet on CRAN. So please use:
*devtools::install.packages(“celevitz/topChef”)*

``` r
devtools::install_github("celevitz/topChef")
```

## References & Acknowlegements

Data were collected manually while watching each season of Top Chef.
Additional data were collected from
<https://en.wikipedia.org/wiki/Top_Chef>. My Top Chef data journey was
inspired by <https://topchefstats.com/>.

Huge thanks to <https://github.com/doehm> for all his support!

## Dataset overview

Across datasets, key joining variables include:

- `chef`
- `szn`
- `sznnumber`
- `series`
- `episode`

### Chef details

A tibble containing information about Chefs for each season they are in,
including placement and gender. For some but not all seasons, there is
also information on hometown, current city of residence, age, a flag for
whether they are a person of color, and their occupation.

``` r
chefdetails 
#> # A tibble: 416 × 13
#>    name  chef  hometown city  state   age szn   sznnumber series placement poc  
#>    <chr> <chr> <chr>    <chr> <chr> <dbl> <chr>     <dbl> <chr>      <dbl> <chr>
#>  1 Rich… Rich… <NA>     <NA>  <NA>     38 All …         8 US             1 <NA> 
#>  2 Mike… Mike… <NA>     <NA>  <NA>     35 All …         8 US             2 <NA> 
#>  3 Anto… Anto… <NA>     <NA>  <NA>     34 All …         8 US             3 <NA> 
#>  4 Tiff… Tiff… <NA>     <NA>  <NA>     27 All …         8 US             4 POC  
#>  5 Carl… Carl… <NA>     <NA>  <NA>     46 All …         8 US             5 POC  
#>  6 Dale… Dale… <NA>     <NA>  <NA>     32 All …         8 US             6 POC  
#>  7 Ange… Ange… <NA>     <NA>  <NA>     35 All …         8 US             7 POC  
#>  8 Fabi… Fabi… <NA>     <NA>  <NA>     32 All …         8 US             8 <NA> 
#>  9 Tre … Tre … <NA>     <NA>  <NA>     34 All …         8 US             9 POC  
#> 10 Marc… Marc… <NA>     <NA>  <NA>     30 All …         8 US            10 <NA> 
#> # ℹ 406 more rows
#> # ℹ 2 more variables: occupation <chr>, gender <chr>
```

### Challenge descriptions

A tibble containing information about each challenge that the Chefs
compete in.

``` r
challengedescriptions 
#> # A tibble: 634 × 17
#>    szn      sznnumber series episode challenge_type outcome_type
#>    <chr>        <dbl> <chr>    <dbl> <chr>          <chr>       
#>  1 Kentucky        16 US           1 Quickfire      Team        
#>  2 Kentucky        16 US           1 Elimination    Team        
#>  3 Kentucky        16 US           2 Quickfire      Individual  
#>  4 Kentucky        16 US           2 Elimination    Team        
#>  5 Kentucky        16 US           3 Quickfire      Individual  
#>  6 Kentucky        16 US           3 Elimination    Individual  
#>  7 Kentucky        16 US           4 Quickfire      Individual  
#>  8 Kentucky        16 US           4 Elimination    Team        
#>  9 Kentucky        16 US           5 Elimination    Team        
#> 10 Kentucky        16 US           6 Elimination    Individual  
#> # ℹ 624 more rows
#> # ℹ 11 more variables: challenge.description <chr>, shop.time <chr>,
#> #   shop.budget <chr>, prep_time <dbl>, cook_time <dbl>,
#> #   product.placement <chr>, advantage <chr>,
#> #   Last.Chance.Kitchen.winner.enters <chr>, Restaurant.War.winner <chr>,
#> #   Restaurant.War.eliminated <chr>, Did.judges.visit.winning.team.first <chr>
```

### Challenge wins

A tibble containing win and loss data for each chef in each episode.

``` r
challengewins
#> # A tibble: 9,544 × 9
#>    szn      sznnumber series episode in.competition chef  challenge_type outcome
#>    <chr>        <dbl> <chr>    <dbl> <lgl>          <chr> <chr>          <chr>  
#>  1 All Sta…         8 US           1 TRUE           Ange… Quickfire      LOW    
#>  2 All Sta…         8 US           1 TRUE           Anto… Quickfire      WIN    
#>  3 All Sta…         8 US           1 TRUE           Carl… Quickfire      LOW    
#>  4 All Sta…         8 US           1 TRUE           Case… Quickfire      HIGH   
#>  5 All Sta…         8 US           1 TRUE           Dale… Quickfire      HIGH   
#>  6 All Sta…         8 US           1 TRUE           Dale… Quickfire      WIN    
#>  7 All Sta…         8 US           1 TRUE           Elia… Quickfire      LOW    
#>  8 All Sta…         8 US           1 TRUE           Fabi… Quickfire      LOW    
#>  9 All Sta…         8 US           1 TRUE           Jami… Quickfire      LOW    
#> 10 All Sta…         8 US           1 TRUE           Jenn… Quickfire      HIGH   
#> # ℹ 9,534 more rows
#> # ℹ 1 more variable: rating <dbl>
```

### Judges

A tibble containing information about who were the guest judges for each
challenge.

``` r
judges 
#> # A tibble: 748 × 9
#>    szn           sznnumber series episode challenge_type outcome_type guestjudge
#>    <chr>             <dbl> <chr>    <dbl> <chr>          <chr>        <chr>     
#>  1 All Stars: N…         8 US           1 Quickfire      Team         Tom Colic…
#>  2 All Stars: N…         8 US           1 Elimination    Individual   Anthony B…
#>  3 All Stars: N…         8 US           2 Quickfire      Individual   Joe Jonas 
#>  4 All Stars: N…         8 US           2 Elimination    Team         Katie Lee 
#>  5 All Stars: N…         8 US           3 Quickfire      Team         David Cha…
#>  6 All Stars: N…         8 US           3 Elimination    Team         Anthony B…
#>  7 All Stars: N…         8 US           3 Elimination    Team         Kate Krad…
#>  8 All Stars: N…         8 US           4 Quickfire      Individual   Tony Mant…
#>  9 All Stars: N…         8 US           4 Elimination    Team         Tony Mant…
#> 10 All Stars: N…         8 US           5 Quickfire      Individual   Tom Colic…
#> # ℹ 738 more rows
#> # ℹ 2 more variables: competed_on_TC <chr>, other_shows <chr>
```

### Rewards

A tibble containing information about rewards and prizes won by
challenge.

``` r
rewards
#> # A tibble: 326 × 9
#>    szn   sznnumber series episode challenge_type outcome_type reward_type reward
#>    <chr>     <dbl> <chr>    <dbl> <chr>          <chr>        <chr>       <chr> 
#>  1 All …         8 US           1 Elimination    Individual   Money       10000…
#>  2 All …         8 US           3 Quickfire      Team         Money       5000.0
#>  3 All …         8 US           3 Quickfire      Team         Money       5000.0
#>  4 All …         8 US           3 Quickfire      Team         Money       5000.0
#>  5 All …         8 US           3 Quickfire      Team         Money       5000.0
#>  6 All …         8 US           3 Elimination    Team         Prize       Trip …
#>  7 All …         8 US           4 Quickfire      Individual   Money       20000…
#>  8 All …         8 US           4 Elimination    Team         Prize       Trip …
#>  9 All …         8 US           5 Quickfire      Individual   Prize       Toyot…
#> 10 All …         8 US           6 Elimination    Team         Prize       Trip …
#> # ℹ 316 more rows
#> # ℹ 1 more variable: chef <chr>
```

### Episode info

A tibble containing information about each episode.

``` r
episodeinfo 
#> # A tibble: 0 × 8
#> # ℹ 8 variables: szn <chr>, sznnumber <dbl>, series <chr>,
#> #   overall.episode.number <dbl>, episode <dbl>, episode_name <chr>,
#> #   air_date <date>, #.of.competitors <dbl>
```

## Examples

### How many elimination challenge wins did Top Chef winners have?

#### Visualization

![](README_files/figure-gfm/EliminationWinsForTopChefsVisualization-1.png)<!-- -->

#### Code

``` r
library(ggplot2)

chefdetails %>% 
  # Keep just winners and relevant variables (just the winning chef's name)
  filter(placement == 1) %>%
  select(chef, gender,series,szn) %>%
  # Bring on the challenge result data
  left_join(challengewins) %>%
  # Keep just elimination & sudden death quickfire challenges
  filter(challenge_type %in% c("Elimination","Quickfire Elimination","Sudden Death Quickfire")) %>%
  # Get the number of wins for each person
  group_by(series,szn,sznnumber,chef,gender) %>%
  filter(outcome %in% c("WIN","WINNER")) %>%
  summarise(wins=n()) %>%
  # Plot
  ggplot(aes(x=wins,y=chef,color=gender,fill=gender,shape=series)) +
    geom_point(size=4) +
    theme_minimal() + 
    # beautification
    scale_color_manual(values=c("#1170AA", "#A3ACB9")) +
    scale_shape_manual(values=c(0,17,1)) +
    scale_x_continuous(name="Number of elimination wins", 
                         breaks=seq(1,9,1),
                         limits=c(.8,9.5),
                         labels=seq(1,9,1)) +
    theme(panel.grid = element_blank(),
            title=element_text(size=12),
            axis.line.x=element_line(color="black"),
            axis.ticks.x=element_line(color="black"),
            axis.text.x=element_text(size=12),
            axis.title.x=element_text(size=12)) +
  labs(title="Number of elimination wins of Top Chefs"
       ,caption="I should have ordered by # of wins...")
    
```

### How do past seasons compare to how Season 20 is currently going?

#### Visualization

![](README_files/figure-gfm/Viz_IndexAllSeasons-1.png)<!-- -->![](README_files/figure-gfm/Viz_IndexAllSeasons-2.png)<!-- -->
