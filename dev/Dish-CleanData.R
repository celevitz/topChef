## Clean the dish data to be able to better analyze it

rm(list=ls())

library(stringr)
library(topChef)
library(openxlsx)
library(tidyverse)

directory <- "/Users/carlylevitz/Documents/Data/"

dishesraw <- as_tibble(read.xlsx(paste(directory,"TopChefData.xlsx",sep="")
                                 ,sheet=2)) %>%
  filter(!(is.na(dish))) %>%
  select(series,season,seasonNumber,episode,chef,challengeType
         ,dish,outcome,notes) %>%
  mutate(dish = tolower(dish)
         ,notes=tolower(notes)
         # remove punctuation
         #,dish=gsub("[[:punct:]]", " ",dish)
         #this will remove spaces too unfortunately so I commented it out
         #,dish=gsub("[^[:alnum:]]", "",dish)
         )
###############################################################################
# spelling mistakes:
#  fois glace glambe grmarnier hroll ong
# don't know what it's supposed to be: fettulini chateaubrinoodle englclam
#                                        glace glambe hroll ong
  dishesraw$dish <- gsub("suace","sauce",
          gsub("tartar ","tartare",
          gsub("tahni","tahini",
          gsub("souop","soup",
          gsub("kim chee","kimchi",
          gsub("kimchee","kimchi",
          gsub("brocolli","broccoli",
          gsub("applauce","applesauce",
          gsub("flambeeded","flambee",
          gsub("choclate","chocolate",
          gsub("cripsy","crispy",
          gsub("linguini","linguine",
          gsub("mouse","mousse",
          gsub("mussles","mussels",
          gsub("chantrelle","chanterelle",
          gsub("loing","loin",
          gsub("portabello","portobello",
          gsub("portobella","portobello",
          gsub("saurkraut","sauerkraut",
          gsub("cremeau","cremeux",
          gsub("cremeaux","cremeux",
          gsub("fois","foie",
          gsub("grmarnier","gran marnier",
          dishesraw$dish
          )))))))))))))))))))))))

###############################################################################
## Clean the data
## E.g., Make plurals singular, add hyphens for things like "crimini mushroom"
cleandishes <- dishesraw

# Remove punctuation (but leave spaces and single quotation marks)
cleandishes$dish <- gsub("\\\\"," ",
gsub("\\:","",
gsub("\\.","",
gsub("\\;","",
gsub("\\)","",
gsub("\\(","",
gsub(", "," ",
gsub("& ","",
cleandishes$dish
))))))))

# places
cleandishes$dish <-gsub("new york","new-york",
gsub("new zealand","new-zealand",
cleandishes$dish))

# Nuts
cleandishes$dish <-gsub("almonds","almond",
gsub("pistachios","pistachio",
gsub("hazelnuts","hazelnut",
gsub("pecans","pecan",
gsub("peanuts","peanut",
gsub("nuts","nut",
cleandishes$dish))))))

# meat
cleandishes$dish <-gsub("flank steak","flank-steak",
gsub("rib eye","rib-eye",
gsub("au jus","au-jus",
gsub("coq au vin","coq-au-vin",
gsub("au vin","au-vin",
gsub("burnt ends","burnt-ends",
gsub(" balls","-balls",
gsub(" ball","-ball",
gsub("eggs","egg",
gsub("dogs","dog",
gsub(" feet","-feet",
gsub("foie gras","foie-gras" ,
gsub("filet mignon","filet-mignon",
gsub("guineafowl","guinea-fowl",
gsub("guinea fowl","guinea-fowl",
gsub("kidneys","kidney",
gsub(" leg","-leg",
gsub("legs","leg",
gsub("meatballs","meatball",
gsub(" pork bell"," pork-bell",
gsub("pork bell","pork-bell",
gsub("short rib","short-rib",
gsub("top round","top-round",
cleandishes$dish)))))))))))))))))))))))


# kinda random stuff
cleandishes$dish <- gsub("chips","chip",
gsub("corn cake","corn-cake",
gsub("croquettes","croquette",
gsub("croutons","crouton",
gsub("breadcrumbs","breadcrumb",
gsub("bread-crumbs","breadcrumb",
gsub("bread-crumb","breadcrumb",
gsub("deep fried","deep-fried",
gsub("dirty rice","dirty-rice",
gsub(" chip","-chip",
gsub("empanadas","empanada",
gsub("enchiladas","enchilada",
gsub("leche de tigre","leche-de-tigre",
gsub("flambe","flambeed",
gsub("fritters","fritter",
gsub("foamed","foam",
gsub("glazed","glaze",
gsub("hoe cake","hoe-cake",
gsub("seeds","seed",
gsub("parker house","parker-house",
gsub("pizzas","pizza",
gsub("purees","puree",
gsub("sous vide","sous-vide",
gsub("angel hair","angel-hair",
gsub("stir fr","stir-fr",
gsub("sauces","sauce",
gsub("wild rice","wild-rice",
cleandishes$dish)))))))))))))))))))))))))))

# desserts
cleandishes$dish <- gsub("bread pudding","bread-pudding",
gsub("panna cotta","panna-cotta",
gsub("ice cream","ice-cream",
gsub("funnel cake","funnel-cake",
gsub(" desserts"," dessert",
gsub("desserts","dessert",
gsub("banana foster","banana-foster",
gsub("bananas foster","bananas-foster",
gsub(" biscuits"," biscuit",
gsub("biscuits","biscuit",
gsub("bon bons","bon-bons",
gsub("cakes","cake",
gsub("cookies","cookie",
gsub("crumbles","crumble",
gsub("clotted cream","clotted-cream",
gsub("condensed milk","condensed-milk",
gsub("cream puff","creampuff",
gsub("creme fraiche","creme-fraiche",
gsub("creme anglaise","creme-anglaise",
gsub("french toast","french-toast",
gsub("pastry cream","pastry-cream",
gsub("petite fours","petite-fours",
gsub("milk chocolate","milk-chocolate",
gsub("dark chocolate","dark-chocolate",
gsub("white chocolate","white-chocolate",
gsub("muffins","muffin",
gsub("rice pudding","rice-pudding",
gsub("whipped cream","whipped-cream",
gsub("whipping cream","whipping-cream",
cleandishes$dish)))))))))))))))))))))))))))))

# seafood
cleandishes$dish <-gsub("anchovies","anchovy",
gsub("angulas","angula",
gsub("alaskan cod","alaskan-cod",
gsub("crabs","crab",
gsub("eels","eel",
gsub("diver-scallops","diver-scallop",
gsub("deep fried","deep-fried",
gsub("diver scallop","diver-scallop",
gsub("dover sole","dover-sole",
gsub("dungeness crab","dungeness-crab",
gsub("clams","clam",
gsub("cockles","cockle",
gsub("manila clam","manila-clam",
gsub("mussels","mussel",
gsub("king salmon","king-salmon",
gsub("king crab","king-crab",
gsub("rainbow trout","rainbow-trout",
gsub("sea bass","sea-bass",
gsub("sea bream","sea-bream",
gsub("scallops","scallop",
gsub("squid ink","squid-ink",
cleandishes$dish)))))))))))))))))))))

# Mushrooms
cleandishes$dish <- gsub(" chanterelles"," chanterelle",
gsub("chanterelles","chanterelle",
gsub("button mushroom","button-mushroom",
gsub("chanterelle mushroom","chanterelle-mushroom",
gsub("cremini mushroom","cremini-mushroom",
gsub("king trumpet mushroom","king-trumpet-mushroom",
gsub("maitake mushroom","maitake-mushroom",
gsub("morel mushroom","morel-mushroom",
gsub("oyster mushroom","oyster-mushroom",
gsub("portobello mushroom","portobello-mushroom",
gsub("shitake mushroom","shitake-mushroom",
gsub("mushrooms","mushroom",
cleandishes$dish
))))))))))))


# produce
cleandishes$dish <- gsub("apples","apple",
gsub("artichokes","artichoke",
gsub("acorn squash","acorn-squash",
gsub(" a la "," a-la ",
gsub("bok choy","bok-choy",
gsub("butternut squash","butternut-squash",
gsub("bananas","banana",
gsub("banana lea","banana-lea",
gsub("ti lea","ti-lea",
gsub("bell pepper"," bell-pepper",
gsub("bell peppers"," bell-pepper",
gsub("bell-peppers","bell-pepper",
gsub("beets","beet",
gsub("beans","bean",
gsub("calabrian chil","calabrian-chil",
gsub("carrots","carrot",
gsub("cherries","cherry",
gsub("cheeses","cheese",
gsub("cornichons","cornichon",
gsub("cucumbers","cucumber",
gsub("endives","endive",
gsub("endives","endive",
gsub("figs","fig",
gsub("green bean","green-bean",
gsub("herbs","herb",
gsub("herbes fines","herb",
gsub("lotus root","lotus-root",
gsub("taro root","taro-root",
cleandishes$dish))))))))))))))))))))))))))))

cleandishes$dish <- gsub("aji amarillo","aji-amarillo",
gsub("brussel sprout","brussels-sprout",
gsub("brussels sprout","brussels-sprout",
gsub("bok choy","bok-choy",
gsub("capers","caper",
gsub("chiles","chile",
gsub("chilis","chili",
gsub("chives","chive",
gsub("fingerling potat","fingerling-potat",
gsub("fruits","fruit",
gsub("grapes","grape",
gsub("heads","head",
gsub("hearts","heart",
gsub("hops","hop",
gsub("kim chee","kimchi",
gsub("kimchee","kimchi",
gsub("leeks","leek",
gsub("leaves","leaf",
gsub("lentils","lentil",
gsub("gran marnier","gran-marnier",
gsub("mojitos","mojito",
gsub("napa cabbage","napa-cabbage",
gsub("peaches"," peach",
gsub("bamboo shoot","bamboo-shoot",
gsub("scallions","scallion",
gsub("sprouts","sprout",
gsub("shallots","shallot",
cleandishes$dish)))))))))))))))))))))))))))

cleandishes$dish <- gsub("kalamata olive","kalamata-olive",
gsub("asian pear","asian-pear",
gsub("mandarin orange","mandarin-orange",
gsub("collard greens","collard-greens",
gsub("collards","collard-greens",
gsub("celeriac root","celeriac-root",
gsub("kernels","kernel",
gsub("berries","berry",
gsub("olive oil","olive-oil",
gsub("passion fruit","passionfruit",
gsub("pears","pear",
gsub("plums","plum",
gsub("plantains","plantain",
gsub("scotch bonnet","scotch-bonnet",
gsub("serrano chile","serrano",
gsub("sea beans","sea-bean",
gsub("sweet potato","sweet-potato",
gsub("omatoes","omato",
gsub("omatos","omato",
gsub("otatoes","otato",
gsub("otatos","otato",
gsub("wood ","wood-",
cleandishes$dish))))))))))))))))))))))

# colors
cleandishes$dish <- gsub("black pepper","black-pepper",
gsub("black chicken","black-chicken",
gsub("black cod","black-cod",
gsub("black garlic","black-garlic",
gsub("black forest","black-forest",
gsub("black truffle","black-truffle",
gsub("black bass","black-bass",
gsub("black tea","black-tea",
gsub("black olive","black-olive",
gsub("black bean","black-bean",
gsub("black rice","black-rce",
gsub("black sesame","black-sesame",
gsub("blood orange","blood-orange",
gsub("oranges","orange",
gsub("red snapper","red-snapper",
gsub("red miso","red-miso",
gsub("red pepper","red-pepper",
gsub("white truffle","white-truffle",
gsub("white pepper","white-pepper",
gsub("white miso","white-miso",
gsub("white fish","white-fish",
gsub("green chartreuse","green-chartreuse",
gsub("green goddess","green-goddess",
gsub("green olive","green-olive",
gsub("mole negro","mole-negro",
cleandishes$dish
)))))))))))))))))))))))))

# sauces
cleandishes$dish <- gsub("bechamel sauce","bechamel-sauce",
gsub("beurre blanc","beurre-blanc",
gsub("cream sauce","cream-sauce",
gsub("soy sauce","soy-sauce",
gsub("fish sauce","fish-sauce",
gsub("salsa verde","salsa-verde",
cleandishes$dish
))))))

## Create long form for analysis

cleandisheslong <- cleandishes %>%
  separate_longer_delim(dish, delim = " ") %>%
  # remove non-dishes
  filter(!(dish %in% c("1","10","14","15","15minute","2","20","3","30","4"
                       ,"40","5","a","","my","myself","-"
                       ,"including","en","an","the","not","shown","on","n/a"
                       ,"of","in","with","wtih","and"
                       ,"everything","but")))
  # remove punctuation
  cleandisheslong$dish <- gsub("\\\\","",cleandisheslong$dish)

tempy<-cleandisheslong %>%
  group_by(dish) %>%
  summarize(number=n())%>%
  arrange(desc(number),dish)
tempy %>% filter(number>=15) %>% print(n=200)


#table(cleandisheslong$dish)
sort(unique(cleandisheslong$dish))[1:20]
cleandisheslong$dish[1:200]
sort(unique(cleandisheslong$dish))
