# topChef

# of seasons
usseasons <- epiinfo %>% filter(series == "US") %>% select(szn) %>% distinct() %>% nrow()
usmastersseasons <- epiinfo %>% filter(series == "US Masters") %>% select(szn) %>% distinct() %>% nrow()
canadaseasons <- epiinfo %>% filter(series == "Canada") %>% select(szn) %>% distinct() %>% nrow()


`r usseasons` seasons of Top Chef US. `r usmastersseasons` seasons of Top Chef Masters US. `r canadaseasons` season of Top Chef Canada. 
