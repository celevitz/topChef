#' chefdetails
#'
#' A dataset containing information on each Chef for each season. As of now,
#' it has data for all Top Chef US seasons, Top Chef Masters (US), and one
#' season of Top Chef Canada.
#'
#' @docType data
#'
#' @usage data(chefdetails)
#'
#' @format This data frame contains the following columns:
#' \describe{
#'   \item{\code{name}}{Chef name (full name)}
#'   \item{\code{chef}}{Shorter version of the chef's name}
#'   \item{\code{hometown}}{Chef's hometown, if known}
#'   \item{\code{city}}{City in which the Chef lived at the time of show}
#'   \item{\code{state}}{State in which the Chef lived at the time of the show}
#'   \item{\code{age}}{Age of Chef at the time of the show}
#'   \item{\code{season}}{Name of season}
#'   \item{\code{seasonNumber}}{Season number}
#'   \item{\code{series}}{Top Chef US (listed as US); Top Chef US Masters
#'                        (listed as US Masters); Top Chef Canada (listed
#'                        as Canada)}
#'   \item{\code{placement}}{Final result of the Chef.}
#'   \item{\code{personOfColor}}{Flag for whether the Chef is a person of color.
#'                    Will be blank if they are not}
#'   \item{\code{occupation}}{Occupation of Chef at time of show, if known}
#'   \item{\code{occupation_category}}{Categorization of occupation}
#'   \item{\code{gender}}{Gender of Chef}
#' }
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr arrange
#' @importFrom dplyr summarise
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr pivot_longer
#'
#' @source \url{https://en.wikipedia.org/wiki/Top_Chef}
#' @examples
#' library(dplyr)
#' library(tidyr)
#' chefdetails %>%
#'   filter(season == "World All Stars")
"chefdetails"


#' challengedescriptions
#'
#' A dataset containing information about each challenge that the
#' Chefs compete in
#'
#' @docType data
#'
#' @usage data(challengedescriptions)
#'
#' @format This data frame contains the following columns:
#' \describe{
#'   \item{\code{season}}{Name of season}
#'   \item{\code{seasonNumber}}{Season number}
#'   \item{\code{series}}{Top Chef US (listed as US); Top Chef US Masters
#'             (listed as US Masters); Top Chef Canada (listed as Canada)}
#'   \item{\code{episode}}{Episode number}
#'   \item{\code{challengeType}}{Challenge type: qualifying challenge,
#'              elimination, quickfire, sudden death quickfire, quickfire
#'              elimination, battle of the sous chefs}
#'   \item{\code{outcomeType}}{Is the challenge run as a team or as an
#'              individual?}
#'   \item{\code{challengeDescription}}{Description of the challenge}
#'   \item{\code{shopTime}}{If they go shopping, how long do they have?
#'              Unit is minutes}
#'   \item{\code{shopBudget}}{If they go shopping, what is their budget?
#'              Unit is dollars unless otherwise specified.}
#'   \item{\code{prepTime}}{If they have prep time, how long do they have?
#'              Unit is minutes}
#'   \item{\code{cookTime}}{How long they have to cook (in minutes)}
#'   \item{\code{productPlacement}}{List of products promoted in the
#'              challenge, other than the usual series-wide product placement.
#'              Will be blank if none were mentioned}
#'   \item{\code{advantage}}{If an advantage is offered to the winner of the
#'            challenge, it will be listed here: e.g., Immunity, choosing
#'            a protein in the elimination challenge, choosing your team in
#'            the elimination challenge. Will be blank if none were mentioned.}
#'   \item{\code{lastChanceKitchenWinnerEnters}}{If someone comes in from
#'       Last Chance Kitchen at this challenge, their name will be listed here.
#'       Will be blank for all other challenges.}
#'   \item{\code{restaurantWarWinner}}{Role played by the winner of
#'              restaurant wars: Executive Chef, Front of House, the full team,
#'              Line Cook, Roles Rotated, or No one won. Will only have values
#'              for Restaurant War episodes.}
#'   \item{\code{restaurantWarEliminated}}{Role played by the Chef eliminated
#'             after restaurant wars: Executive Chef, Front of House, the full
#'             team, Line Cook, Roles Rotated. Will only have values for
#'             Restaurant War episodes.}
#'   \item{\code{didJudgesVisitWinningTeamFirst}}{Categorical variable of
#'   which team was shown serving the judges first. Will only have values for
#'   Restaurant Wars episodes.}
#' }
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr arrange
#' @importFrom dplyr summarise
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr pivot_longer
#'
#' @source \url{https://en.wikipedia.org/wiki/Top_Chef}
#' @examples
#' library(dplyr)
#' library(tidyr)
#' challengedescriptions %>%
#'    group_by(series,season,outcomeType) %>%
#'    summarise(n=n()) %>%
#'    pivot_wider(names_from=outcomeType,values_from=n)
"challengedescriptions"



#' challengewins
#'
#' A dataset containing win and loss data for each chef in each episode
#'
#' @docType data
#'
#' @usage data(challengewins)
#'
#' @format This data frame contains the following columns:
#' \describe{
#'   \item{\code{season}}{Name of season}
#'   \item{\code{seasonNumber}}{Season number}
#'   \item{\code{series}}{Top Chef US (listed as US); Top Chef US Masters
#'                        (listed as US Masters); Top Chef Canada (listed
#'                        as Canada)}
#'   \item{\code{episode}}{Episode number}
#'   \item{\code{inCompetition}}{True / false for whether the Chef was still
#'                                in the competition at the time of the
#'                                challenge}
#'   \item{\code{immune}}{True / false for whether that Chef was immune from
#'                            being eliminated for challenge}
#'   \item{\code{chef}}{Name of chef}
#'   \item{\code{challengeType}}{Challenge type: qualifying challenge,
#'                              elimination, quickfire, sudden death quickfire,
#'                              quickfire elimination, battle of the sous
#'                              chefs}
#'   \item{\code{outcome}}{Result for each Chef in the competition for that
#'                        challenge}
#'   \item{\code{rating}}{Numeric rating provided to chefs in Top Chef US
#'                        Masters Seasons 1 and 2. Will be blank for all
#'                        other seasons.}
#' }
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr arrange
#' @importFrom dplyr summarise
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr pivot_longer
#'
#' @source \url{https://en.wikipedia.org/wiki/Top_Chef}
#' @examples
#' library(dplyr)
#' library(tidyr)
#' challengewins %>%
#'   group_by(outcome) %>%
#'   summarise(n=n())
"challengewins"




#' episodeinfo
#'
#' A dataset containing information about each episode
#'
#' @docType data
#'
#' @usage data(episodeinfo)
#'
#' @format This data frame contains the following columns:
#' \describe{
#'   \item{\code{season}}{Name of season}
#'   \item{\code{seasonNumber}}{Season number}
#'   \item{\code{series}}{Top Chef US (listed as US); Top Chef US Masters
#'                        (listed as US Masters); Top Chef Canada (listed as
#'                        Canada)}
#'   \item{\code{overallEpisodeNumber}}{Running number of episode within
#'                                        the series}
#'   \item{\code{episode}}{Episode number}
#'   \item{\code{episodeName}}{Name of episode}
#'   \item{\code{airDate}}{Date the episode originally aired}
#'   \item{\code{nCompetitors}}{Number of Chefs still in the competition}
#' }
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr arrange
#' @importFrom dplyr summarise
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr pivot_longer
#'
#' @source \url{https://en.wikipedia.org/wiki/Top_Chef}
#' @examples
#' library(dplyr)
#' library(tidyr)
#' episodeinfo %>% filter(season=="World All Stars")
"episodeinfo"



#' judges
#'
#' A dataset containing information about who were the guest judges for
#' each challenge
#'
#' @docType data
#'
#' @usage data(judges)
#'
#' @format This data frame contains the following columns:
#' \describe{
#'   \item{\code{season}}{Name of season}
#'   \item{\code{seasonNumber}}{Season number}
#'   \item{\code{series}}{Top Chef US (listed as US); Top Chef US Masters
#'            (listed as US Masters); Top Chef Canada (listed as Canada)}
#'   \item{\code{episode}}{Episode number}
#'   \item{\code{challengeType}}{Challenge type: qualifying challenge,
#'              elimination, quickfire, sudden death quickfire, quickfire
#'              elimination, battle of the sous chefs}
#'   \item{\code{outcomeType}}{Is the challenge run as a team or as an
#'   individual?}
#'   \item{\code{guestJudge}}{Name of guest judge}
#'   \item{\code{gender}}{Gender of Chef}
#'   \item{\code{personOfColor}}{Flag for whether the Chef is a person of color.
#'                    Will be blank if they are not}
#'   \item{\code{competedOnTC}}{Will have a value of Yes if they competed
#'   on a season of Top Chef}
#'   \item{\code{otherShows}}{Information about other shows that this
#'   individual has appeared on}
#' }
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr arrange
#' @importFrom dplyr summarise
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr pivot_longer
#'
#' @source \url{https://en.wikipedia.org/wiki/Top_Chef}
#' @examples
#' library(dplyr)
#' library(tidyr)
#' judges %>%
#'   filter(guestJudge == "Eric Ripert") %>%
#'   group_by(challengeType) %>%
#'   summarise(n=n())
"judges"



#' rewards
#'
#' A dataset containing information about rewards and prizes won by challenge
#'
#' @docType data
#'
#' @usage data(rewards)
#'
#' @format This data frame contains the following columns:
#' \describe{
#'   \item{\code{season}}{Name of season}
#'   \item{\code{seasonNumber}}{Season number}
#'   \item{\code{series}}{Top Chef US (listed as US); Top Chef US Masters
#'   (listed as US Masters); Top Chef Canada (listed as Canada)}
#'   \item{\code{episode}}{Episode number}
#'   \item{\code{challengeType}}{Challenge type: qualifying challenge,
#'   elimination, quickfire, sudden death quickfire, quickfire elimination,
#'   battle of the sous chefs}
#'   \item{\code{outcomeType}}{Is the challenge run as a team or as an
#'   individual?}
#'   \item{\code{rewardType}}{Variable describing whether the reward is
#'   money or a prize}
#'   \item{\code{reward}}{Description of the full reward}
#'   \item{\code{chef}}{Name of chef}
#' }
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr arrange
#' @importFrom dplyr summarise
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr pivot_longer
#'
#' @source \url{https://en.wikipedia.org/wiki/Top_Chef}
#' @examples
#' library(dplyr)
#' library(tidyr)
#' rewards %>%
#'   filter(rewardType == "Money") %>%
#'   mutate(reward=as.numeric(reward)) %>%
#'   group_by(season) %>%
#'   summarise(total=sum(reward))
"rewards"
