library(Lahman)
library(tidyverse)


#' Calculate Batter similarity score
#'
#' Given an initial batter find the top 10 players with highest similarity scores
#'
#' @param p1 Initial Player
#'
#' @return The similarity score
#' @export
#'
#' @examples
battersimscore <- function(p1, year) {
  batting <- Batting %>%
    replace_na(list(SF = 0, HBP = 0)) 
  
  
  
  
  Batting %>% filter(playerID == p1 & yearID == year) -> P
  Batting %>%
    filter(G>5) %>%
    mutate(sim_score = 1000 -
        (abs(G - P$G) / 20) -
        (abs(AB - P$AB) / 75) -
        (abs(R - P$R) / 10) -
        (abs(H - P$H) / 15) -
        (abs(X2B - P$X2B) / 5) -
        (abs(X3B - P$X3B) / 4) -
        (abs(HR - P$HR) / 2) -
        (abs(RBI - P$RBI) / 10) -
        (abs(BB - P$BB) / 25) -
        (abs(SO - P$SO) / 150) -
        (abs(SB - P$SB) / 20)) %>%
    arrange(desc(sim_score)) %>%
    head(10)
}
?floor()

head(Batting)
battersimscore("aaronha01", 1958)
head(Positions)
head(Fielding)
head(Appearances)
h <- Appearances

fieldtotal <- Fielding %>%
  group_by(playerID, yearID) %>%
  summarize(g = sum(G))

fieldtotal %>%
  filter(playerID == "addybo01")

