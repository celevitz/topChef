# topChef 0.2.0

* Edits to 'chefdetails': added occupation category and added data for 
season 21
* Edits to 'challengedescriptions': added data for season 21. Updated historical
data if it was missing, particularly for seasons 1-3
* Edits to 'challengewins': added data for season 21. Updated historical
data, particularly the quickfire challenges to have low/high data. So far, the
edits have only been for seasons 1-3
* Edits to 'judges': added data for season 21 and added variables for gender and 
whether the judge was a person of color. 
* Edits to 'rewards': added data for season 21, and added a variable for 
categorizing reward types.
* Edits to 'episodeinfo': added data for season 21
* Made tests more rigorous
* Edits to index function: 1) simplified it to not use tidy, and instead pull in
from dplyr; 2) fixed an error that was making the index pull in data for all 
challenges prior to a specific number of challenges instead of pulling in JUST 
the specific number of each type of challenge; 3) updated description to say 
that you cannot have 0 as an input; 4) fix an error that was leading to people 
getting an incorrect amount of points in episodes where there was an elimination
challenge AND a (sudden death quickfire challenge OR a quickfire elimination).
