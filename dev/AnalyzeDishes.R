## Analyze dish data

rm(list=ls())

library(tidyverse)
library(stringr)
library(topChef)
library(openxlsx)
library(ggplot2)

directory <- "/Users/carlylevitz/Documents/Data/"

dishesraw <- as_tibble(read.xlsx(paste(directory,"TopChefData.xlsx",sep="")
                              ,sheet=2)) %>%
  filter(inCompetition == TRUE & !(is.na(dish))) %>%
  select(series,season,seasonNumber,episode,chef,challengeType
         ,dish,outcome,notes) %>%
  mutate(dish = tolower(dish),notes=tolower(notes))


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
# Stay in wide form
###############################################################################
  disheswide <- dishesraw

  # Alcohol
    alcohols <- c("amaretto","baileys","beer","bourbon"
                  ,"brandy","brut","cachaca","champagne","chartreuse"
                  ,"cognac","grissini","guinness","gran marnier"
                  ,"liqueur","merlot","mezcal"
                  ,"mojito","moscato","pernod","rum","sake","shaoxing"
                  ,"sambuca","sherry","shaoxing","vermouth","wine")
    disheswide$alcohol <- 0
    for (a in alcohols) {
      disheswide$alcohol[grepl(a,disheswide$dish)  ] <- 1
    }

  # Aromatics
    aromatics <- c("chive","garlic","ginger","onion","scallion")
    disheswide$aromatics <- 0
    for (a in alcohols) {
      disheswide$aromatics[grepl(a,disheswide$dish)  ] <- 1
    }

  # Baked/fried pastry ish things
    bakedthingies <- c("arayes","beignet","biscuit","bread pudding","cornbread"
                       ,"donut","doughnut","empanada","foccacia","frittata"
                       ,"fritter","meringue","muffin","cobbler","phyllo"
                       ,"quiche","croquette")
    disheswide$baked <- 0
    for (b in bakedthingies) {
      disheswide$baked[grepl(b,disheswide$dish)  ] <- 1
    }

  # Beef/cattle
    beefs <- c("beef","cheesesteak","filet mignon","waygu","flank steak"
               ,"meatball","meatloaf","oxtail")
    disheswide$beef <- 0
    for (b in beefs) {
      disheswide$beef[grepl(b,disheswide$dish)  ] <- 1
    }

  # Cheese
    cheeses <- c("asiago","cheddar","cheese","chevre","bechamel","brie"
                 ,"burrata","camembert","dunbarton blue","feta","fromage"
                 ,"gouda","gorgonzola"
                 ,"mozzarella","d'ambert","halloumi","manchego","mascarpone"
                 ,"mornay","parmesan","pecorino")

    disheswide$cheese <- 0
    for (c in cheeses) {
      disheswide$cheese[grepl(c,disheswide$dish)  ] <- 1
    }

  # Citrus
  ## issue with searching for lemon is lemongrass
    citruses <- c("lime","lemon","grapefruit","orange","tangelo","satsuma"
                  ,"nectarine","yuzu","citrus")
    disheswide$citrus <- 0
    for (c in citruses) {
      disheswide$citrus[grepl(c,disheswide$dish)  ] <- 1
    }
    #disheswide$citrus[disheswide$dish == "lemongrass"] <- 0

  # Dessert
    # a few times cannolis are savory
    desserts <- c("ice cream","cannoli","gelato","haupia","poundcake"
                  ,"sorbet","sable","granita","upside down cake","pie"
                  ,"upside-down cake","dessert","sable","granita")
    disheswide$dessert <- 0
    for (d in desserts) {
      disheswide$dessert[grepl(d,disheswide$dish)  ] <- 1
      disheswide$dessert[grepl(d,disheswide$notes)  ] <- 1
    }

  # Dish includes a drink
    drinks <- c("chaser","cocktail","drink","nog")
    disheswide$drink <- 0
    for (d in drinks) {
      disheswide$drink[grepl(d,disheswide$dish)  ] <- 1
      disheswide$drink[grepl(d,disheswide$notes)  ] <- 1
    }

  # Duo or trio
    disheswide$duotrio <- 0
    disheswide$duotrio[grepl("duo",disheswide$dish) |
                       grepl("trio",disheswide$dish) |
                       grepl("duet",disheswide$dish) |
                       grepl("dual",disheswide$dish)] <- 1


  # Fish
    fishes <- c("fish","fishtail", "ahi","anchov","angulas","bass","bocarones"
            ,"branzino","calamari","sardine"
            ,"catfish","caviar","cod","dorade","dory","eel","escolar","flounder"
            ,"futomake","gravalax","grouper","halibut","hamachi","ikura"
            ,"lionfish","mackerel","monkfish","dover sole","octopus","perch"
            ,"rainbow-trout","king salmon","sea bass","snapper","squid"
            ,"trout","tuna","opah","opakapaka","poke","poi","sashimi","salmon"
            ,"osetra","walleye")

    disheswide$fish <- 0
    for (f in fishes) {
      disheswide$fish[grepl(f,disheswide$dish)  ] <- 1
    }

  # Fruit
    ## Not searching for "berries" in case it pulls up wheatberries
    ## not doing berry/berries because I don't want to deal w/ plurals
    fruits <- c("apple","cherry","date","lychee","fig","fruit","mango","melon"
                ,"pear","pomegranate","plum","pineapple"," berry ","cranberr"
                ,"strawberr","raspberr","blackberr","boisonberr","blueberr"
                ,"aronia berr","huckleberr","gooseberr","lingonberr","berries"
                ,"quince","berry","kiwi","nopal","papaya")
    disheswide$fruit <- 0
    for (f in fruits) {
      disheswide$fruit[grepl(f,disheswide$dish)  ] <- 1
    }

  # Game meet (including pheasant, goose)
    games <- c("alligator","antelope","buffalo","bison","boar","elk","frog"
               ,"goat","goose","kangaroo","lamb","pheasant","ostrich","squab"
               ,"quail")
    disheswide$game <- 0
    for (g in games) {
      disheswide$game[grepl(g,disheswide$dish)  ] <- 1
    }

  # Grains
    grains <- c("couscous","rice","congee","bulgar","wheatberr","farro"
                ,"barley","cornmeal","corn-meal","grits","oatmeal")
    disheswide$grain <- 0
    for (g in grains) {
      disheswide$grain[grepl(g,disheswide$dish)  ] <- 1
    }

  # Greens
    greens <- c("cabbage","chard","kale","dandelion","kombu","collard"
                ,"lettuce","micro-greens","nori","sea bean","seaweed")
    disheswide$green <- 0
    for (g in greens) {
      disheswide$green[grepl(g,disheswide$dish)  ] <- 1
    }

  # Herbs
    herbs <- c("cilantro","parsley","dill","caraway","herb","juniper","mint"
               ,"thyme","rosemary","cumin","oregano","lavendar"
               ,"basil","chimichurri","vadouvan","vaudouvan")
    disheswide$herb <- 0
    for (h in herbs) {
      disheswide$herb[grepl(h,disheswide$dish)  ] <- 1
    }

  # No heat was used
    noheatdishes <- c("aguachile","carpaccio","crudo","ceviche","crudite"
                      ,"futomake","leche de tigre","nigiri","poke","sashimi")
    disheswide$noheat <- 0
    for (nh in noheatdishes) {
      disheswide$noheat[grepl(nh,disheswide$dish)  ] <- 1
    }

  # Nuts & legumes
    nuts <- c("cannellini","macadamia","nut","edamame","pistachio"
              ,"peanut","pecan","chickpea","garbanzo","lentil","pepita"
              ,"almond")
    disheswide$nut <- 0
    for (n in nuts) {
      disheswide$nut[grepl(n,disheswide$dish)  ] <- 1
    }

  # Offal
  # issue with looking for heart - things like "artichoke heart"
    offals <- c("kidney","liver","sweetbread","sweetbreads","heart","tongue"
                ,"lengua","innard")
    disheswide$offal <- 0
    for (o in offals) {
      disheswide$offal[grepl(o,disheswide$dish)  ] <- 1
    }

  # Pasta & noodles
    pastas <- c("agnolotti","fettuccini","spaghetti"," mac "
                ,"gnocchi","gnudi","lasagna","linguine","macaroni","cannellonis"
                ,"cappellini","fettulini","orecchiette","pappardelle","noodle"
                ,"udon","ramen","tortellini","ravioli","raviolo","linguini"
                ,"fusilli","pasta","rigatoni","soba")

    disheswide$pasta <- 0
    for (p in pastas) {
      disheswide$pasta[grepl(p,disheswide$dish)  ] <- 1
    }

  # Peppers/chilis
    # issue with "pepper jack" cheese
    peppers <- c("scotch-bonnet","aji-amarillo","calabrian","guajillo","chili"
                 ,"chile","habanero","jalapeno","jalapeño","poblano","pepper"
                 ,"hatch ","shishito","serrano","fresno")
    disheswide$pepper <- 0
    for (p in peppers) {
      disheswide$pepper[grepl(p,disheswide$dish)  ] <- 1
    }

  # Pickles
    disheswide$pickle <- 0
    disheswide$pickle[grepl("pickle",disheswide$dish) |
                        grepl(" epi ",disheswide$dish) |
                        grepl(" epis ",disheswide$dish) |
                        grepl("piklz",disheswide$dish)] <- 1

  # Pork
    pig <- c("bacon","bratwurst","ham","hotdog","pancetta"
             ,"chorizo","karabuto","mortadella","speck","pork","pork-leg"
             ,"porketta","porkloin","salami","sausage")

    disheswide$pork <- 0
    for (p in pig) {
      disheswide$pork[grepl(p,disheswide$dish)  ] <- 1
    }

  # Poultry (non-game)
    fowl <- c("chicken","duck","foie","foie-gras","fois-gras","turkey")

    disheswide$poultry <- 0
    for (f in fowl) {
      disheswide$poultry[grepl(f,disheswide$dish)  ] <- 1
    }

  # Sauce types
    disheswide$sauce <- NA
    disheswide$sauce[grepl("bolognese",disheswide$dish)] <- "bolognese"
  disheswide$sauce[grepl("bechamel sauce",disheswide$dish)] <- "bechamel sauce"
    disheswide$sauce[grepl("bernaise",disheswide$dish) |
                       grepl("bearnaise",disheswide$dish)  ] <- "bearnaise"
    disheswide$sauce[grepl("beurre blanc",disheswide$dish)] <- "beurre blanc"
    disheswide$sauce[grepl("cream sauce",disheswide$dish)] <- "cream sauce"
    disheswide$sauce[grepl("creme anglais",disheswide$dish)] <- "creme anglais"
    disheswide$sauce[grepl("gremolata",disheswide$dish)] <- "gremolata"
    disheswide$sauce[grepl("mornay",disheswide$dish)] <- "mornay"
    disheswide$sauce[grepl(" mole",disheswide$dish)] <- "mole"
    disheswide$sauce[grepl("pesto",disheswide$dish)] <- "pesto"
    disheswide$sauce[grepl("pipian",disheswide$dish)] <- "pipian"
    disheswide$sauce[grepl("pistou",disheswide$dish)] <- "pistou"
    disheswide$sauce[grepl("ponzo",disheswide$dish)] <- "ponzo"
    disheswide$sauce[grepl("ponzu",disheswide$dish)] <- "ponzu"
    disheswide$sauce[grepl("sambal",disheswide$dish)] <- "sambal"
    disheswide$sauce[grepl("veloute",disheswide$dish)] <- "veloute"
    disheswide$sauce[grepl("aioli",disheswide$dish) |
                       grepl("mayo",disheswide$dish)] <- "aioli/mayo"
    disheswide$sauce[grepl("sauce",disheswide$dish) &
                       is.na(disheswide$dish)] <- "sauce"


  # Shellfish
  # Issue with searching for oyster --- cuz of oyster mushrooms
  ## and when people just put a dish into the shell but don't use it
    shellfishes <- c("abalone","clam","cockle","conch","crab","langoustine"
                     ,"crawfish","crayfish" ,"crustacean","scallop","geoduck"
                     ,"hama","lobster","langoustine","oyster","shrimp","prawn")

    disheswide$shellfish <- 0
    for (sf in shellfishes) {
      disheswide$shellfish[grepl(sf,disheswide$dish)  ] <- 1
    }

  # Soups, broths, brodos
    soups <- c("avgolemono","bisque","brodo","broth","chowder","dashi"
               ,"gazpacho","gumbo","pozole")
    disheswide$soup <- 0
    for (s in soups) {
      disheswide$soup[grepl(s,disheswide$dish)  ] <- 1
    }

  # Starch (vegetables)
    starches <- c("beet","squash","corn","potato","lotus","pomme","taro"
                  ,"plantain","platano","yucca")
    disheswide$starch <- 0
    for (s in starches) {
      disheswide$starch[grepl(s,disheswide$dish)  ] <- 1
    }

  # Uses tea
    teas <- c("cha","chai","tea")
    disheswide$tea <- 0
    for (t in teas) {
      disheswide$tea[grepl(t,disheswide$dish)  ] <- 1
    }

  # Vegetables (other)
    veggies <- c("celery","celeriac root","mushroom","daikon","radish"
                 ,"coleslaw","cucumber","green bean","avocado","eggplant"
                 ,"maitake","pea","pea shoot","okra","artichoke","sunchoke"
                 ,"parsnip","zucchini")

    disheswide$vegetable <- 0
    for (v in veggies) {
      disheswide$vegetable[grepl(v,disheswide$dish)  ] <- 1
    }

  # What do I think are the trendy things?
    trends <- c("confit","conserva","consomme","deconstructed","emulsion"
                ,"espuma","foam","gel","mousse","risotto","scallop"
                ,"ceviche","aguachile","croquette","sous vide","tartare")
    disheswide$trend <- NA
    for (t in trends) {
      disheswide$trend[grepl(t,disheswide$dish)  ] <- t
      disheswide$trend[grepl(t,disheswide$notes)  ] <- t
    }

###############################################################################
# Long form: to help with counts of sub-categories in larger categories
###############################################################################

## Clean the dish data:
cleandishes <- disheswide %>%
  select(!c(outcome,challengeType,notes))

  # for things that are often together (e.g., ____ chip), combine them
  # remove plurals
  # combine things like flambee/flambeed
  # have to split up the gsubs cuz it got too long

    # Nuts
    cleandishes$dish <-gsub("almonds ","almond ",
                       gsub("pistachios","pistachio",
                       gsub("hazelnuts","hazelnut",
                       gsub("pecans","pecan",
                       gsub("peanuts","peanut",
                       gsub("nuts","nut",
                      cleandishes$dish))))))

    # meat
    cleandishes$dish <-gsub("flank steak","flank-stead",
                        gsub("rib eye","rib-eye",
                        gsub("au jus","au-jus",
                        gsub("coq au vin","coq-au-vin",
                        gsub("au vin","au-vin",
                        gsub(" balls","-balls",
                        gsub(" ball","-ball",
                        gsub("eggs","egg",
                        gsub("dogs","dog",
                        gsub(" feet","-feet",
                        gsub("foie gras","foie-gras" ,
                        gsub("foie gras","foie-gras" ,
                        gsub("filet mignon","filet-mignon",
                        gsub("kidneys","kidney",
                        gsub(" leg","-leg",
                        gsub("legs","leg",
                        gsub("meatballs","meatball",
                        gsub(" pork bell"," pork-bell",
                        gsub("pork bell","pork-bell",
                        gsub("short rib","short-rib",
                      cleandishes$dish))))))))))))))))))))

    # pastry, baked stuff, cooking methods
    cleandishes$dish <- gsub(" cakes"," cake",
                        gsub("cakes","cake",
                        gsub("chips","chip",
                        gsub("croquettes","croquette",
                        gsub("croutons","crouton",
                        gsub("crumbles","crumble",
                        gsub("deep fried","deep-fried",
                        gsub(" biscuits"," biscuit",
                        gsub("biscuits","biscuit",
                        gsub(" chip","-chip",
                        gsub("empanadas","empanada",
                        gsub("enchiladas","enchilada",
                        gsub("leche de tigre","leche-de-tigre",
                        gsub("petite fours","petite-fours",
                        gsub("flambe","flambeed",
                        gsub("fritters","fritter",
                        gsub("foamed","foam",
                        gsub("glazed","glaze",
                        gsub("hoe cake","hoe-cake",
                        gsub("seeds","seed",
                        gsub("pizzas","pizza",
                        gsub("purees","puree",
                        gsub("sous vide","sous-vide",
                        gsub("angel hair","angel-hair",
                        gsub("sauces","sauce",
                       cleandishes$dish)))))))))))))))))))))))))

    # desserts
      cleandishes$dish <- gsub("bread pudding","bread-pudding",
                          gsub("panna cotta","panna-cotta",
                          gsub("ice cream","ice-cream",
                          gsub("funnel cake","funnel-cake",
                          gsub(" desserts"," dessert",
                          gsub("desserts","dessert",
                          gsub("banana foster","banana-foster",
                          gsub("bananas foster","bananas-foster",
                          gsub("cookies","cookie",
                          gsub("french toast","french-toast",
                          gsub("pastry cream","pastry-cream",
                          gsub("milk chocolate","milk-chocolate",
                          gsub("dark chocolate","dark-chocolate",
                          gsub("white chocolate","white-chocolate",
                          gsub("rice pudding","rice-pudding",
                          gsub("whipped cream","whipped-cream",
                          gsub("whipping cream","whipping-cream",
                          gsub("clotted cream","clotted-cream",
                          gsub("condensed milk","condensed-milk",
                          gsub("creme fraiche","creme-fraiche",
                          gsub("creme anglaise","creme-anglaise",
                         cleandishes$dish)))))))))))))))))))))

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
                              cleandishes$dish))))))))))))))))))))

    # Mushrooms
      cleandishes$dish <- gsub(" chanterelles"," chanterelle",
                          gsub("chanterelles","chanterelle",
                          gsub("button mushroom","button-mushroom",
                          gsub("chanterelle mushroom","chanterelle-mushroom",
                          gsub("cremini mushroom","cremini-mushroom",
                          gsub("maitake mushroom","maitake-mushroom",
                          gsub("morel mushroom","morel-mushroom",
                          gsub("oyster mushroom","oyster-mushroom",
                          gsub("portobello mushroom","portobello-mushroom",
                          gsub("shitake mushroom","shitake-mushroom",
                          gsub("mushrooms","mushroom",
                          cleandishes$dish
                          )))))))))))

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
                gsub("taro root","taro-root"
               ,cleandishes$dish))))))))))))))))))))))))))

        cleandishes$dish <- gsub("aji amarillo","aji-amarillo",
                gsub("capers","caper",
                gsub("chiles","chile",
                gsub("chives","chive",
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
               cleandishes$dish)))))))))))))))))))

        cleandishes$dish <- gsub("kalamata olive","kalamata-olive",
                      gsub("mandarin orange","mandarin-orange",
                      gsub("collard greens","collard-greens",
                      gsub("collards","collard-greens",
                      gsub("celeriac root","celeriac-root",
                      gsub("kernels","kernel",
                      gsub("berries","berry",
                      gsub("olive oil","olive-oil",
                      gsub("plums","plum",
                      gsub("plantains","plantains",
                      gsub("scotch bonnet","scotch-bonnet",
                      gsub("sea beans","sea-bean",
                      gsub("sweet potato","sweet-potato",
                      gsub("omatoes","omato",
                      gsub("omatos","omato",
                      gsub(" wood "," wood-",
                     cleandishes$dish))))))))))))))))

    # colors
    cleandishes$dish <- gsub("black pepper","black-pepper",
                      gsub("black chicken","black-chicken",
                      gsub("black garlic","black-garlic",
                      gsub("black forest","black-forest",
                      gsub("black truffle","black-truffle",
                      gsub("black bass","black-bass",
                      gsub("black tea","black-tea",
                      gsub("black olive","black-olive",
                      gsub("black bean","black-bean",
                      gsub("black rice","black-rce",
                      gsub("black sesame","black-sesame",
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
      cleandishes$dish
      )))))))))))))))))))))

    # sauces
    cleandishes$dish <- gsub("bechamel sauce","bechamel-sauce",
            gsub("beurre blanc","beurre-blanc",
            gsub("cream sauce","cream-sauce",
            gsub("soy sauce","soy-sauce",
            gsub("fish sauce","fish-sauce",
            cleandishes$dish
            )))))
    # Get rid of things like "with" "and"
    # need to figure out how to remove backslashes
        cleandishes$dish <- gsub(" including "," ",
                            gsub(" en "," ",
                            gsub(" an "," ",
                            gsub("\\:","",
                            gsub("\\.","",
                            gsub("\\;","",
                            gsub(" the "," ",
                            gsub("not shown","",
                            gsub("\\)","",
                            gsub("\\(","",
                            gsub(" on "," ",
                            gsub("n/a","",
                            gsub(" of "," ",
                            gsub(" a "," ",
                            gsub(" in "," ",
                            gsub(", "," ",
                            gsub(" with "," ",
                            gsub(" wtih "," ",
                            gsub("& ","",
                            gsub("and ","",cleandishes$dish
                            ))))))))))))))))))))


## Prep the data for analysis
  # reshape the data
    cleandisheslong <- cleandishes %>%
      separate_longer_delim(dish, delim = " ")

  # check for words that aren't categorized in the previous section
    cleandisheslong %>%
      filter(alcohol == 0 & aromatics == 0 & baked == 0 & beef == 0 &
               cheese == 0 & citrus == 0 & dessert == 0 & drink == 0 &
               duotrio == 0 & fish == 0 & fruit == 0 & game == 0 &
               grain == 0 & green == 0 & herb == 0 & noheat == 0 & nut == 0 &
               offal == 0 & pasta == 0 & pepper == 0 & pickle == 0 & pork == 0 &
               poultry == 0 & sauce == 0 & shellfish == 0 & soup == 0 &
               starch == 0 & tea == 0 & vegetable == 0 & is.na(trend)) %>%
      select(dish) %>%
      distinct()
  # clean the data
    cleandisheslong <- cleandisheslong %>%
      filter(!(dish %in% c("-","3","1","10","+","15","15-minute","2","20"
                           ,"30","4","40","5","a"," ")) & !(is.na(dish)))

# ## Summary analyses
## Specific word
    #   # Number of times a word shows up
        wordtimes <- cleandisheslong %>%
          group_by(series,dish) %>%
          summarise(totaltimesused=n()) %>%
          arrange(desc(totaltimesused))

    #   # number of seasons in which a word shows up
        wordinseason <- cleandisheslong %>%
          select(series,season,seasonNumber,dish) %>%
          distinct() %>%
          group_by(series,dish) %>%
          summarise(numberofseasonsusedin = n()) %>%
          arrange(desc(numberofseasonsusedin))

    #   # number of episodes a word is used in
        wordinepisodes <- cleandisheslong %>%
          select(series,season,seasonNumber,episode,dish) %>%
          distinct() %>%
          group_by(series,dish) %>%
          summarise(numberofepisodesusedin = n()) %>%
          arrange(desc(numberofepisodesusedin))

    #   # Number of episodes within each season
        wordinseasonnumberofepis <- cleandisheslong %>%
          select(series,season,seasonNumber,episode,dish) %>%
          distinct() %>%
          group_by(series,season,dish) %>%
          summarise(numberofepisodesusedin = n()) %>%
          arrange(desc(numberofepisodesusedin)) %>%
          pivot_wider(names_from=season,values_from=numberofepisodesusedin)

    #   # first time a word shows up
        firstappearance <- cleandisheslong %>%
          group_by(dish) %>%
          left_join(topChef::episodeinfo %>%
                  select(series,seasonNumber,episode,overallEpisodeNumber)) %>%
          mutate(minep = min(overallEpisodeNumber)) %>%
          filter(minep == overallEpisodeNumber) %>%
          select(season,seasonNumber,episode,dish) %>%
          distinct()
##############################################################################
## Trends of words
    # Time trend
        timetrend <- cleandisheslong %>%
          group_by(seasonNumber,dish) %>%
          summarise(n=n()) %>%
          # how many times does this actually show up?
          ungroup() %>% group_by(dish) %>%
          mutate(N=sum(n)) %>%
          arrange(seasonNumber,desc(N),dish) %>%
          filter(dish != "") %>%
          pivot_wider(names_from=seasonNumber,values_from=n)


        timetrend %>%
          ggplot(aes(x=seasonNumber,y=n)) +
          geom_point()

## General type of food
  #     # Dishes wide
        # cleandisheslong %>%
        #   pivot_longer(!c(series,season,seasonNumber,episode,chef,dish)
        #                ,names_to = "category")

## Season 21
  s21 <- cleandisheslong %>%
    filter(seasonNumber == 21)

  s21 %>%
    group_by(dish) %>%
    summarise(n=n()) %>%
    arrange(desc(n)) %>%
    print(n=100)


  sort(unique(s21$dish))

  # trends
    trendynumbers <- disheswide %>%
      filter(!is.na(trend) & seasonNumber %in% c(1,2,21)) %>%
      select(seasonNumber,episode,chef,dish)

    for (t in trends) {
      trendynumbers[,t] <- 0
      trendynumbers[grepl(t,trendynumbers$dish),t] <- 1
    }


    trendynumbers <- trendynumbers %>%
      select(!dish) %>%
      pivot_longer(!c(seasonNumber,episode,chef)
                   ,names_to = "trend"
                   ,values_to = "yesno") %>%
      group_by(seasonNumber,trend) %>%
      summarise(numberofdishes=sum(yesno)) %>%
      pivot_wider(names_from =seasonNumber,values_from = numberofdishes)

    table(disheswide$seasonNumber[disheswide$noheat == 1])
    table(disheswide$seasonNumber[disheswide$duotrio == 1])

      # compared to total # of dishes
        totaldishes <- disheswide %>%
          filter(seasonNumber %in% c(1,2,21)) %>%
          group_by(seasonNumber) %>%
          summarise(n=n())

      # aguachile, confit, croquette, duo/trio, emulsion, foam, gel, mousse
        #, No heat , risotto, scallop






