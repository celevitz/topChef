## Analyze dish data

rm(list=ls())

library(tidyverse)
library(stringr)
library(topChef)

directory <- "/Users/carlylevitz/Documents/Data/"

dishesraw <- as_tibble(read.xlsx(paste(directory,"TopChefData.xlsx",sep="")
                              ,sheet=2)) %>%
  filter(inCompetition == TRUE & !(is.na(dish))) %>%
  select(series,season,seasonNumber,episode,chef,challengeType,dish,outcome) %>%
  mutate(dish = tolower(dish))


## Clean the dish data:
cleandishes <- dishesraw %>%
  select(!c(outcome,challengeType))
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

  # spelling mistakes:
  # tahni suace tartar souop brocoll ,chee fettulini appleauce bourbon-flambeeded
  # chateaubrinoodle, choclate, cremeau/cremeux/cremeaux? cripsy englclam
  # flambeeded fois glace glambe grmarnier hroll linguini mouse mussles
  # chantrelle ong loing portabello saurkraut

  # for things that are often together (e.g., ____ chip), combine them
  # remove plurals
  # combine things like flambee/flambeed
  # have to split up the gsubs cuz it got too long
    cleandishes$dish <-
        gsub(" almonds "," almond ",
        gsub(" anchovies "," anchovy ",
        gsub("apples","apple",
        gsub(" artichokes"," artichoke",
        gsub(" acorn squash"," acorn-squash",
        gsub(" a la "," a-la ",
        gsub("au jus","au-jus",
        gsub(" balls","-balls",
        gsub(" ball","-ball",
        gsub("bok choy","bok-choy",
        gsub("banana foster","banana-foster",
        gsub("bananas foster","bananas-foster",
        gsub(" butternut squash"," butternut-squash",
        gsub(" bananas"," banana",
        gsub(" bell pepper"," bell-pepper",
        gsub(" bell-peppers"," bell-pepper",
        gsub(" biscuits"," biscuit",
        gsub(" beets"," beet ",
        gsub(" cakes"," cake",
        gsub(" carrots"," carrot",
        gsub(" chanterelles"," chanterelle",
        gsub(" cherries"," cherry",
        gsub(" cheeses"," cheese",
        gsub(" clams"," clam",
        gsub(" cockles"," cockle",
        gsub(" cookies"," cookie",
        gsub(" cornichons"," cornichon",
        gsub(" crabs"," crab",
        gsub("croquettes","croquette",
        gsub("croutons","crouton",
        gsub("crumbles","crumble",
        gsub("cucumbers","cucumber",
        gsub(" creme fraiche"," creme-fraice",
        gsub(" chip","-chip",
        gsub(" desserts"," dessert",
        gsub("diver-scallops","diver-scallop",
        gsub("deep fried","deep-fried",
        gsub("diver scallop","diver-scallop",
        gsub("dover sole","dover-sole",
        gsub("dungeness crab","dungeness-crab",
        gsub("endives","endive",
        gsub("endives","endive",
        gsub(" eels"," eel",
        gsub(" eggs"," egg",
        gsub(" dogs"," dog",
        gsub(" feet","-feet",
        gsub(" figs"," fig",
        gsub("funnel cake"," funnel-cake",
        gsub("green bean","green-bean"
     ,cleandishes$dish)))))))))))))))))))))))))))))))))))))))))))))))))

    # Mushrooms
      cleandishes$dish <- gsub("button mushroom","button-mushroom",
                          gsub("chanterelle mushroom","chanterelle-mushroom",
                          gsub("cremini mushroom","cremini-mushroom",
                          gsub("maitake mushroom","maitake-mushroom",
                          gsub("morel mushroom","morel-mushroom",
                          gsub("oyster mushroom","oyster-mushroom",
                          gsub("portobello mushroom","portobello-mushroom",
                          gsub("shitake mushroom","shitake-mushroom",
                          cleandishes$dish
                          ))))))))

    cleandishes$dish <- gsub("empanadas","empanada",
            gsub("aji amarillo","aji-amarillo",
            gsub("bread pudding","bread-pudding",
            gsub("capers","caper",
            gsub("chiles","chile",
            gsub("chives","chive",
            gsub("enchiladas","enchilada",
            gsub("filet mignon","filet-mignon",
            gsub("flambe","flambeed",
            gsub("foamed","foam",
            gsub("fois gras","fois-gras" ,
            gsub("fruits","fruit",
            gsub("french toast","french-toast",
            gsub("glazed","glaze",
            gsub("grapes","grape",
            gsub("hazelnuts","hazelnut",
            gsub("heads","head",
            gsub("hearts","heart",
            gsub("hoe cake","hoe-cake",
            gsub("hotdogs","hotdog",
            gsub(" ice cream"," ice-cream",
            gsub("kidneys","kidney",
            gsub("kim chee","kimchi",
            gsub("kimchee","kimchi",
            gsub(" leg ","-leg ",
            gsub("legs","leg",
            gsub("leeks","leek",
            gsub("leaves","leaf",
            gsub("lentils","lentil",
            gsub("manila clam","manila-clam",
            gsub("gran marnier","gran-marnier",
            gsub("meatballs","meatball",
            gsub("mojitos","mojito",
            gsub("mushrooms","mushroom",
            gsub("mussels","mussel",
            gsub("napa cabbage","napa-cabbage",
            gsub("alaskan cod","alaskan-cod",
            gsub("panna cotta","panna-cotta",
            gsub("peaches"," peach",
            gsub(" pork bell"," pork-bell",
            gsub("petite fours","petite-fours",
            gsub("peanuts","peanut",
            gsub("rainbow trout","rainbow-trout",
            gsub("scotch bonnet","scotch-bonnet",
            gsub(" sea bass"," sea-bass",
            gsub(" sea bream"," sea-bream",
            gsub("sea beans","sea-bean",
            gsub(" sweet potato"," sweet-potato",
            gsub(" wood "," wood-",
            gsub("white fish","white-fish",
            cleandishes$dish
            ))))))))))))))))))))))))))))))))))))))))))))))))))

    cleandishes$dish <- gsub("kalamata olive","kalamata-olive",
                             gsub("king salmon","king-salmon",
                             gsub("king crab","king-crab",
                            gsub("mandarin orange","mandarin-orange",
                           gsub("collard greens","collard-greens",
                            gsub("celeriac root","celeriac-root",
                           gsub("cranberries","cranberry",
                          gsub("plums","plum",
                           gsub("pistachios","pistachio",
                            gsub("plantains","plantains",
                           gsub("pecans","pecan",
                          gsub("scallops","scallop",
      cleandishes$dish
      ))))))))))))

## Prep the data for analysis
  # reshape the data
    cleandisheslong <- cleandishes %>%
      separate_longer_delim(dish, delim = " ")

  # create categories
    cleandisheslong$descriptor <- 0
    cleandisheslong$descriptor[cleandisheslong$dish %in% c("american","asian"
         ,"appetizers","awakening","baked","barbacoa","barbecue","barbeque"
         ,"battered","bbq","beer-battered","best","big","blackened"
         ,"braised","brulee","bruleed","burnt","buttery","butter-poached"
         ,"cajun","carmelized","chargrilled","charred","cheesy","classic"
         ,"chilled","chinese"
         ,"compressed","confit","conserva","consomme","cooked","creamed"
         ,"creamy","creamed","crispy","crunchy","crusted","dark","decadent"
         ,"deconstructed","dehydrated","dipped","dredged","dried","ecuadoran"
         ,"diced","dish","dishes","deviled","deliciosa","deep-fried","dry"
         ,"emulsion","espuma","filled","flourless","foam","foamed","frozen"
         ,"edible","fresh","filet","hot","ganache","general","electric"
         ,"fried","garnish","gastrique","gel","gelee","gravy","greek","grilled"
         ,"filling","fingerling","fire","five","finished"
         ,"infused","italian","italian-style","jamaican","japanese","jelly"
         ,"hand-rolled","head","heart","heavy","heirloom","homemade"
         ,"jerk","jerky","macerated","marinated","mediterranean","melted"
         ,"mousse","colorful","covered","ground","gratin","grass-fed","kebab"
         ,"katsu","kobe","korean","loaf","loin","lollipop","brunch","breakfast"
         ,"luncheon","hash","hashed","mash","mashed","medley","mexican"
         ,"marcona","marmalade","medallion"
         ,"meyer","minted","mint-infused","mole","moroccan"
         ,"paella","old-fashioned","natural","organic"
         ,"rolls","rubbed","shards","warm","salad","puree","roasted","powder"
         ,"red","gold","lust","gluttony","envy","sloth"
         ,"style","smashed","slow-grilled","skinless","sour","spice"
         ,"seared","scramble","scrambled"
         ,"toasted","topping"
         ,"whole","white","wild")] <- 1

    cleandisheslong$fish <- 0
    cleandisheslong$fish[cleandisheslong$dish %in% c("fish","fishtail",
       "ahi","anchovy","angulas","bass","bocarones","branzino","calamari"
       ,"catfish","caviar","cod","dorade","dory","eel","escolar","flounder"
       ,"futomake","gravalax","grouper","halibut","hamachi","ikura"
       ,"lionfish","mackerel","monkfish","dover-sole","octopus","perch"
       ,"rainbow-trout","king-salmon","sea-bass","snapper","squid"
       ,"trout","tuna","opah","opakapaka","poke","poi","sashimi","salmon"
       ,"osetra")] <- 1

    cleandisheslong$shellfish <- 0
    cleandisheslong$shellfish[cleandisheslong$dish %in% c("abalone","clam"
       ,"clams","cockle","conch","crab","crawfish","crayfish","dungeness-crab"
       ,"crustacean","diver-scallop","geoduck","hama","king-crab","lobster"
       ,"lobster-umeboshi","scallop")] <- 1

    cleandisheslong$poultry <- 0
    cleandisheslong$poultry[cleandisheslong$dish %in% c("chicken","duck"
      ,"foie","foie-gras","fois-gras","goose")] <- 1

    cleandisheslong$pork <- 0
    cleandisheslong$pork[cleandisheslong$dish %in% c("bacon","bacon-grilled"
       ,"bacon-roasted","bacon-wrapped","buffalo","bratwurst","ham","hotdog"
       ,"chorizo","karabuto","mortadella","speck","pork","pork-leg"
       ,"porketta","porkloin","salami","sausage")] <- 1

    cleandisheslong$citrus <- 0
    cleandisheslong$citrus[grepl("lime",cleandisheslong$dish) |
                           grepl("lemon",cleandisheslong$dish) |
                           grepl("grapefruit",cleandisheslong$dish) |
                           grepl("tangelo",cleandisheslong$dish) |
                           grepl("orange",cleandisheslong$dish) |
                           grepl("mandarin-orange",cleandisheslong$dish) |
                       cleandisheslong$dish %in% c("satsuma","nectarine")] <- 1
    cleandisheslong$citrus[cleandisheslong$dish == "lemongrass"] <- 0

    cleandisheslong$fruit <- 0
    cleandisheslong$fruit[cleandisheslong$dish %in% c("apple","cherry","dates"
      ,"lychee","fig","fruit","mango","melon","pear","pomegranate","plum"
      ,"craberry","cranberry-nori","blackberry","raspberry","strawberry"
      ,"pineapple")] <- 1

    cleandisheslong$starch <- 0
    cleandisheslong$starch[cleandisheslong$dish %in% c("acorn-squash","beet"
      ,"butternut-squash","corn","potato","sweet-potato","potato-chips"
      ,"potatoes","lotus","lotus-chips","plantain","platano")] <- 1

  # greens & sea weed
    cleandisheslong$greens <- 0
    cleandisheslong$greens[cleandisheslong$dish %in% c("cabbage","chard","kale"
       ,"dandelion","kombu","collard-greens","lettuce","micro-greens"
       ,"cranberry-nori","nori","sea-bean")] <- 1

  # other vegetables
    cleandisheslong$veg <- 0
    cleandisheslong$veg[grepl("mushroom",cleandisheslong$veg)] <- 1
    cleandisheslong$veg[cleandisheslong$dish %in% c("celery","celeriac-root"
      ,"coleslaw","cucumber","green-beans","avocado","eggplant","eggplant-chip"
      ,"maitake","pea","pea-shoot","okra","artichoke","sunchoke")] <- 1

  # aromatics
    cleandisheslong$aromatics <- 0
    cleandisheslong$aromatics[grepl("onion",cleandisheslong$dish) |
                                grepl("garlic",cleandisheslong$dish) |
                                grepl("chive",cleandisheslong$dish) |
                                grepl("scallion",cleandisheslong$dish) |
                                grepl("ginger",cleandisheslong$dish) ] <- 1

  # beef
    cleandisheslong$beef <- 0
    cleandisheslong$beef[cleandisheslong$dish %in% c("beef","beefs"
       ,"cheesesteak","filet-mignon","waygu","meatball","meatloaf"
       ,"oxtail")] <- 1

  # game
    cleandisheslong$game <- 0
    cleandisheslong$game[cleandisheslong$dish %in% c("alligator","bison","boar"
      ,"elk","goat","kangaroo","lamb","pheasant","ostrich")] <- 1
    cleandisheslong$game[grepl("frog",cleandisheslong$dish)] <- 1

  # offal
    cleandisheslong$offal <- 0
    cleandisheslong$offal[cleandisheslong$dish %in% c("kidney","liver"
      ,"sweetbread","sweetbreads")] <- 1

  # pastas (should this be noodles more generally?)
    cleandisheslong$pasta <- 0
    cleandisheslong$pasta[cleandisheslong$dish %in% c("agnolotti","fettuccini"
      ,"gnocchi","gnudi","lasagna","linguine","macaroni","cannellonis"
      ,"cappellini","fettulini","orecchiette")] <- 1

  # things that were baked
    cleandisheslong$baked <- 0
    cleandisheslong$baked[cleandisheslong$dish %in% c("arayes","beignets"
      ,"biscuit","biscuits","bread-pudding","cornbread","donut","doughnut"
      ,"empanada","foccacia","frittata","fritter","meringue","muffins","cobbler"
      ,"phyllo")] <- 1

  # desserts
    cleandisheslong$dessert <- 0
    cleandisheslong$dessert[cleandisheslong$dish %in% c("ice-cream","cannoli"
      ,"gelato","haupia","poundcake")] <- 1

  # soups, broths, brodos
    cleandisheslong$soup <- 0
    cleandisheslong$soup[cleandisheslong$dish %in% c("avgolemono","bisque"
      ,"brodo","broth","chowder","gazpacho","gumbo")] <- 1

  # alcohol or liqueur as ingredient
    cleandisheslong$alcohol <- 0
    cleandisheslong$alcohol[cleandisheslong$dish %in% c("amaretto","baileys"
      ,"beer","bourbon","bourbon-flambeed","brandy","brut","cachaca","champagne"
      ,"cognac","grissini","guinness","liqueur","merlot","mezcal"
      ,"mojito","moscato","pernod","sambuca")] <- 1

  # didn't involve any heat
    cleandisheslong$noheat <- 0
    cleandisheslong$noheat[cleandisheslong$dish %in% c("aguachile","carpaccio"
      ,"crudo","ceviche","crudites","futomake","poke","sashimi")] <- 1

  # tea as an ingredient
    cleandisheslong$tea <- 0
    cleandisheslong$tea[cleandisheslong$dish %in% c("cha","chai","tea")] <- 1

  # sauces
    cleandisheslong$sauce <- 0
    cleandisheslong$sauce[cleandisheslong$dish %in% c("sauce","sambal"
        ,"pipian","ponzo")] <- 1

  # involved a drink as a dish
    cleandisheslong$drink <- 0
    cleandisheslong$drink[cleandisheslong$dish %in% c("chaser","cocktail"
      ,"drink","nog")] <- 1

  # nuts, legumes
    cleandisheslong$nut <- 0
    cleandisheslong$nut[cleandisheslong$dish %in% c("cannellini","macadamia"
       ,"peanut","pecan","chickpea","garbanzo","lentil","pepita"
       ,"almond")] <- 1
    cleandisheslong$nut[grepl("nut",cleandisheslong$dish)] <- 1

  #peppers & chilis
    cleandisheslong$pepper <- 0
    cleandisheslong$pepper[cleandisheslong$dish %in% c("scotch-bonnet"
       ,"aji-amarillo","calabrian","guajillo","habanero","jalapeno"
       ,"jalapeño","poblano")] <- 1
    cleandisheslong$pepper[grepl("pepper",cleandisheslong$dish)] <- 1

  # Herbs & spices
    cleandisheslong$herb <- 0
    cleandisheslong$herb[cleandisheslong$dish %in% c("cilantro","parsley","dill"
      ,"caraway","juniper" ,"thyme","rosemary","cumin","oregano")] <- 1
    cleandisheslong$herb[grepl("herb",cleandisheslong$dish)] <- 1

  # pickles
    cleandisheslong$pickle <- 0
    cleandisheslong$pickle[cleandisheslong$dish %in% c("epi","epis")] <- 1
    cleandisheslong$pickle[grepl("pickle",cleandisheslong$dish)] <- 1

  # cheeses
    cleandisheslong$cheese <- 0
    cleandisheslong$cheese[cleandisheslong$dish %in% c("asiago","cheddar"
      ,"cheese","chevre","bechamel","brie","camembert","feta","fromage","gouda"
      ,"mozzarella","d'ambert","halloumi","manchego","mascarpone"
      ,"pecorino")] <- 1

    cleandisheslong$duotrio <- 0
    cleandisheslong$duotrio[cleandisheslong$dish %in% c("duo","trio"
                                                        ,"dual","duet")] <- 1

    cleandisheslong %>%
        filter(descriptor == 0 & fish == 0 & shellfish == 0 & poultry == 0 &
                 pork == 0 & beef == 0 & game == 0 & offal == 0 & baked == 0 &
                 soup == 0 & alcohol == 0 & noheat == 0 & cheese == 0 &
                 tea == 0 & drink == 0 & dessert == 0 & pasta == 0 &
                 duotrio == 0 & nut == 0 & pepper == 0 & herb == 0 &
                 pickle == 0 & citrus == 0 & fruit == 0 & starch == 0 &
                 greens == 0 & veg == 0 & sauce == 0) %>%
        select(dish) %>%
        distinct() %>%
        arrange(dish) %>%
      print(n=700)
#
# ## Summary analyses
#   # Number of times a word shows up
#     wordtimes <- cleandisheslong %>%
#       group_by(series,dish) %>%
#       summarise(totaltimesused=n()) %>%
#       arrange(desc(totaltimesused))
#
#   # number of seasons in which a word shows up
#     wordinseason <- cleandisheslong %>%
#       select(series,season,seasonNumber,dish) %>%
#       distinct() %>%
#       group_by(series,dish) %>%
#       summarise(numberofseasonsusedin = n()) %>%
#       arrange(desc(numberofseasonsusedin))
#
#   # number of episodes a word is used in
#     wordinepisodes <- cleandisheslong %>%
#       select(series,season,seasonNumber,episode,dish) %>%
#       distinct() %>%
#       group_by(series,dish) %>%
#       summarise(numberofepisodesusedin = n()) %>%
#       arrange(desc(numberofepisodesusedin))
#
#
# tail(wordinseason)
#
# sort(unique(cleandisheslong$dish))[1:500]
# sort(unique(cleandisheslong$dish))[501:1000]
# sort(unique(cleandisheslong$dish))[1001:1495]
#
#
#
#
#
#
#
#
#
#
