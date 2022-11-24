# Setup -------------------------------------------------------------------

source("init.R")

# Read data ---------------------------------------------------------------

data_team_predicted_summary_mean_prob_result =
  readRDS("files/dataset/output/c02_data_team_predicted_summary_mean_prob_result.RDS")

#  Expected --------------------------------------------------------------

team_score =
  data_team_predicted_summary_mean_prob_result %>%
  mutate(
    away_mean_win_prob = 1 - mean_win_prob,
    away_result = case_when(
      home_team_result == 'win' ~  'lose',
      home_team_result == 'draw' ~ 'draw',
      home_team_result == 'lose' ~ 'win'
    ),
    home_score =  case_when(
      home_team_result == 'win' ~ 3,
      home_team_result == "draw" ~ 1,
      home_team_result == "lose" ~ 0
    ),
    away_score = case_when(
      away_result == 'win' ~ 3,
      away_result == "draw" ~ 1,
      away_result == "lose" ~ 0
    )
  ) %>%
  mutate(
    home_cond_score = 3 * mean_win_prob,
    away_cond_score = 3 * away_mean_win_prob
  )

# Save dataset ------------------------------------------------------------

team_score |>
  data.table::fwrite("files/dataset/output/c03_team_score.csv")
team_score |>
  saveRDS("files/dataset/output/c03_team_score.RDS")