# Setup -------------------------------------------------------------------

source("init.R")
library(h2o)
h2o.init()

# Read data ---------------------------------------------------------------

home_team_win_drf = 
  h2o.loadModel(path = "files/modeling_output/model_fit/b01_rf.model")

data_team_cross = 
  readRDS("files/dataset/intermedia/b01_data_team_cross.RDS")

data_team_predicted_summary =
  readRDS("files/dataset/output/c01_data_team_predicted_summary.RDS")

# Mean score per match ----------------------------------------------------

data_team_predicted_mean_score =
  data_team_predicted_summary |>
  select(-result) |>
  left_join(
    data_team_predicted_summary |>
      select(-result,
             away_team_win_prob = home_team_win_prob),
    by = c("home_team" = "away_team",
           "away_team" = "home_team")
  ) |>
  mutate(mean_win_prob = (home_team_win_prob + (1 - away_team_win_prob)) /
           2)

data_team_home_away =
  data_team_predicted_mean_score |>
  mutate(
    home_away_team = str_c(home_team, away_team),
    away_home_team = str_c(away_team, home_team)
  ) |>
  # mutate(flag_home_away_duplicated = home_away_team == away_home_team) |>
  # View()
  pivot_longer(
    cols =
      c(home_away_team,
        away_home_team),
    names_to = "home_away_team",
    values_to = "value"
  )

data_team_home_away_duplicate_value =
  data_team_home_away |>
  select(value) |>
  duplicated()

data_team_home_away_no_duplicate_value =
  data_team_home_away[data_team_home_away_duplicate_value, ]

data_team_home_away_duplicate_home_away =
  data_team_home_away_no_duplicate_value |>
  select(home_team, away_team) |>
  duplicated()

data_team_home_away_no_duplicate_home_away =
  data_team_home_away_no_duplicate_value[data_team_home_away_duplicate_home_away, ]

data_team_predicted_summary_mean_prob =
  data_team_home_away_no_duplicate_home_away |>
  select(home_team,
         away_team,
         mean_win_prob)

data_team_predicted_summary_mean_prob_result =
  data_team_predicted_summary_mean_prob |>
  mutate(
    home_team_result = case_when(
      mean_win_prob < threshold_lose ~ "lose",
      mean_win_prob < threshold_win ~ "draw",
      TRUE ~ "win"
    )
  )

# Save data ---------------------------------------------------------------

data_team_predicted_summary_mean_prob_result |>
  data.table::fwrite("files/dataset/output/c02_data_team_predicted_summary_mean_prob_result.csv")
data_team_predicted_summary_mean_prob_result |>
  saveRDS("files/dataset/output/c02_data_team_predicted_summary_mean_prob_result.RDS")

