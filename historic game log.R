# load libraries
pacman::p_load(dplyr, fflr, tidyverse)

# specify fantasy league id
ffl_id(1470089)

# specify starting season for historic collection
start_season <- 2020

# create empty df to rbind data to later
game_log <- NULL

# loop through each season
for(i in start_season:2023){
  # specify fantasy league id
  ffl_id(1470089)
  
  # collect team ids and names from ith season
  teams <- league_teams(leagueId = ffl_id(), seasonId = i) |>
    dplyr::left_join(league_members(leagueId = ffl_id(), seasonId = i), by = "memberId") |>
    dplyr::mutate(
      season = 2023
    ) 
  
  # collect the games from the ith season
  games <- as.data.frame(tidy_scores(leagueId = ffl_id(), seasonId = i)) |>
    dplyr::mutate(
      isWinner = ifelse(isWinner, 1, 0),
      isHome = ifelse(isHome, "home", "away"),
      tW = percent_rank(totalPoints)
    ) |>
    dplyr::rename(season = seasonId, week = matchupPeriodId, pf = totalPoints,
                  wW = expectedWins, win = isWinner) |>
    dplyr::select(season, week, matchupId, isHome, 
                  teamId, abbrev, win, pf, tW, wW)
  
  # pivot wider to get home/away scores from each matchup
  scores <- games |>
    dplyr::select(season, week, matchupId, isHome, pf) |>
    pivot_wider(
      names_from = isHome,
      values_from = pf
    )
  
  # get the schedule
  schedule <- tidy_schedule(leagueId = ffl_id(), seasonId = i) |>
    dplyr::left_join(teams, by = c("opponent" = "abbrev")) |>
    dplyr::rename("week" = "matchupPeriodId", "teamId" = "teamId.x", "oppAbbrev" = "opponent", 
                  "oppId" = "teamId.y", "opponent" = "firstName", "oppTeam" = "name") |>
    dplyr::select(season, week, matchupId, teamId, abbrev, oppAbbrev, oppId, oppTeam, opponent) 
  
  combined <- left_join(games, scores, by = c("season", "week", "matchupId")) |>
    left_join(teams, by = "teamId") |>
    left_join(schedule, by = c("matchupId", "teamId")) |>
    rename("home_score" = "home", "away_score" = "away", "week" = "week.x", "team" = "name", 
           "owner" = "firstName") |>
    mutate(
      pa = ifelse(isHome == "home", away_score, home_score)
    ) |>
    select(season, week, matchupId, teamId, abbrev.x, team, owner, oppId, oppAbbrev, oppTeam,
           opponent, win, pf, pa, tW, wW) |>
    dplyr::rename("abbrev" = "abbrev.x")
  
  game_log <- rbind(game_log, combined)
  
  return(i)
  
}
