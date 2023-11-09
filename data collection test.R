# collect team ids and names from ith season
teams <- league_teams() |>
  dplyr::left_join(league_members(), by = "memberId") |>
  dplyr::mutate(
    season = 2023
  ) 

# collect the games from the ith season
games <- as.data.frame(tidy_scores()) |>
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
schedule <- tidy_schedule() |>
  dplyr::left_join(teams, by = c("opponent" = "abbrev")) |>
  dplyr::rename("week" = "matchupPeriodId", "teamId" = "teamId.x", "oppAbbrev" = "opponent", 
                "oppId" = "teamId.y", "opponent" = "firstName", "oppTeam" = "name") |>
  dplyr::select(season, week, matchupId, teamId, abbrev, oppAbbrev, oppId, oppTeam, opponent) 

game_log <- left_join(games, scores, by = c("season", "week", "matchupId")) |>
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

# make a loop to go through best ball lineups for each week/season
# bind them together after each iteration
best_roster <- as.data.frame(do.call(rbind, best_roster(useScore = "actualScore",
                                                        scoringPeriodId = ))
) |>
  dplyr::filter(lineupSlot != "BE") |>
  dplyr::group_by(teamId, scoringPeriodId) |>
  summarise(
    best_ball = sum(actualScore)
  )