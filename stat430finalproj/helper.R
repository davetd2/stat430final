library(Lahman)
library(tidyverse)
library(dplyr)

# The following commented out code generates and writes the batting and pitching dataframes we use for our calculations

# bat_war = readr::read_csv("https://www.baseball-reference.com/data/war_daily_bat.txt", na = "NULL") 
# bwar_pit = readr::read_csv("https://www.baseball-reference.com/data/war_daily_pitch.txt", na = "NULL")
# 
# namedat = People %>% mutate(name = paste(nameFirst, nameLast)) %>% select(playerID, name)
# 
# batting <- Batting %>%
#   replace_na(list(SF = 0, HBP = 0, IBB = 0, SH = 0)) %>%
#   group_by(playerID, yearID) %>% 
#   summarize_at(colnames(Batting)[6:22], sum)
# 
# batting <- merge(batting, namedat, by = 'playerID')
#   
# batting <- batting %>%
#   mutate(AVG = H / AB,
#          SLG = (H - X2B - X3B - HR + 2 * X2B + 3 * X3B + 4 * HR) / AB)
# 
# positions <- Fielding %>%
#   group_by(playerID, POS) %>%
#   summarize(Games = sum(G)) %>%
#   arrange(playerID, desc(Games)) %>%
#   filter(POS == first(POS)) 
# 
# batting <- batting %>%
#   inner_join(positions, by = "playerID") %>%
#   mutate(Value.POS = case_when(
#     POS == "C" ~ 240,
#     POS == "SS" ~ 168,
#     POS == "2B" ~ 132,
#     POS == "3B" ~ 84,
#     POS == "OF" ~ 48,
#     POS == "1B" ~ 12,
#     TRUE ~ 0))
# 
# temp = bat_war %>%
#   replace_na(list(WAR = 0)) %>%
#   mutate(playerID = player_ID, yearID = year_ID) %>%
#   group_by(playerID, yearID) %>%
#   summarize(totalWAR = sum(WAR))
# 
# batting <- batting %>%
#   inner_join(temp, by = c("playerID", "yearID"))
# 
# batting <- batting %>%
#   group_by(playerID) %>%
#   mutate(szn = rank(yearID))
# 
# pitching <- Pitching %>%
#   replace_na(list(ERA = 0)) %>%
#   group_by(playerID, yearID) %>% 
#   summarize_at(colnames(Pitching)[6:27], sum)
# 
# temp2 = bwar_pit %>%
#   replace_na(list(WAR = 0)) %>%
#   mutate(playerID = player_ID, yearID = year_ID) %>%
#   group_by(playerID, yearID) %>%
#   summarize(totalWAR = sum(WAR))
# 
# pitching <- pitching %>%
#   inner_join(temp2, by = c("playerID", "yearID")) %>%
#   group_by(playerID) %>%
#   mutate(szn = rank(yearID))
# 
# pitching <- merge(pitching, namedat, by = 'playerID')
# 
# write_csv(batting, file = "batterdata.csv")
# write_csv(pitching, file = "pitcherdata.csv")

batting <- read_csv(file = "batterdata.csv")
pitching <- read_csv(file = "pitcherdata.csv")


#' Calculate Batter similarity score
#'
#' Given an initial batter find the top 5 players with highest similarity scores given an initial season.
#'
#' @param p1 Initial Player
#' @param num The season number to compare against
#'
#' @return The similarity score
#' @export
#'
#' @examples
battersimscore <- function(p1, num) {
  
  batting %>% filter(playerID == p1 & szn == num) -> P
  
  if(P$POS == "P") {
    pitching %>% filter(playerID == p1 & szn == num) -> P
    result <- pitching %>%
      filter(G>5 & szn == num) %>%
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
      arrange(desc(sim_score))
  } else {
    result <- batting %>%
      filter(G > 5 & szn == num) %>%
      mutate(
        sim_score = 1000 -
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
          (abs(SB - P$SB) / 20) -
          (abs(AVG - P$AVG) / 0.001) -
          (abs(SLG - P$SLG) / 0.002) -
          abs(Value.POS - P$Value.POS)) %>%
      arrange(desc(sim_score))
  }
}


#' Compare all seasons of the 5 most similar batter from the originally selected season
#'
#' @param p1 the player to compare against
#' @param num The initial season to compare against
#' @param df1 
#'
#' @return
#' @export
#'
#' @examples
simchartdata <- function(p1, num, df1) {
  batting %>% filter(playerID == p1 & szn == num) -> P
  
  if (P$POS == "P") {
    playercomp <- df1[1:6, ] %>%
      select(playerID)
    maxszn <- max((pitching %>% filter(playerID == p1))$szn)
    dat <- NULL
    for (i in 1:maxszn) {
      temp <- battersimscore(p1, i)
      temp <- temp %>% filter(playerID %in% playercomp$playerID)
      dat <- rbind(dat, temp)
    }
    return(dat)
  } else{
    playercomp <- df1[1:6, ] %>%
      select(playerID)
    maxszn <- max((batting %>% filter(playerID == p1))$szn)
    dat <- NULL
    for (i in 1:maxszn) {
      temp <- battersimscore(p1, i)
      temp <- temp %>% filter(playerID %in% playercomp$playerID)
      dat <- rbind(dat, temp)
    }
    return(dat)
  }
}
system.time(b <- simchartdata("aaronha01", 2, battersimscore("aaronha01", 2)))
system.time(c <- simchartdata("goodedw01", 2, battersimscore("goodedw01", 2)))
f <- pitching %>% filter(playerID == "fordru01")
