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
    replace_na(list(SF = 0, HBP = 0)) %>%
    mutate(AVG = H / AB,
           SLG = (H - X2B - X3B - HR + 2 * X2B + 3 * X3B + 4 * HR) / AB)
  
  positions <- Fielding %>%
    group_by(playerID, POS) %>%
    summarize(Games = sum(G)) %>%
    arrange(playerID, desc(Games)) %>%
    filter(POS == first(POS)) 
  
  batting <- batting %>%
    inner_join(Positions, by = "playerID") %>%
    mutate(Value.POS = case_when(
      POS == "C" ~ 240,
      POS == "SS" ~ 168,
      POS == "2B" ~ 132,
      POS == "3B" ~ 84,
      POS == "OF" ~ 48,
      POS == "1B" ~ 12,
      TRUE ~ 0))
  
  
  batting %>% filter(playerID == p1 & yearID == year) -> P
  
  if(P$POS == "P") {
    Pitching %>% filter(playerID == p1 & yearID == year) -> P
    Pitching %>%
      filter(G>5) %>%
      mutate(sim_score = 1000 -
               (abs(W - P$W)) -
               (abs(L - P$L)/ 2) - 
               min((abs((W/G) - (P$W/P$G))/0.002), 100) -
               min(abs(ERA - P$ERA) /0.02,100) -
               (abs(G - P$G) / 10) -
               (abs(GS - P$GS) / 20) -
               (abs(CG - P$CG) / 20) -
               (abs(IPouts - P$IPouts) / 50) -
               (abs(H - P$H) / 50) -
               (abs(SO - P$SO) / 30) -
               (abs(BB - P$BB) / 10) -
               (abs(SHO - P$SHO) / 5) -
               (abs(SV - P$SV) / 3)) %>%
      arrange(desc(sim_score)) %>%
      head(10)
  } else {
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
}
?floor()

head(Batting)
battersimscore("aaronha01", 1958)
battersimscore("aardsda01", 2008)
head(Positions)
head(Pitching)
head(Fielding)
head(Appearances)
h <- Appearances

fieldtotal <- Fielding %>%
  group_by(playerID, yearID) %>%
  summarize(g = sum(G))

fieldtotal %>%
  filter(playerID == "addybo01")

Batting %>% filter(playerID == "aaronha01" & yearID == 1958)

