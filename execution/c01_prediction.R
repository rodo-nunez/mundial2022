# Setup -------------------------------------------------------------------

source("init.R")
library(h2o)
h2o.init()

# Read data ---------------------------------------------------------------

home_team_win_drf = 
  h2o.loadModel(path = "files/modeling_output/model_fit/b01_rf.model")

data_team_cross = 
  readRDS("files/dataset/intermedia/b01_data_team_cross.RDS")

# Prediction --------------------------------------------------------------

home_team_win_predinction =
  home_team_win_drf |>
  h2o::h2o.predict(newdata =
                     data_team_cross |>
                     as.h2o()) |>
  as.data.frame() |>
  select(home_team_win_prob = TRUE.)

data_team_predicted =
  data_team_cross |>
  bind_cols(home_team_win_predinction) |>
  mutate(
    result = case_when(
      home_team_win_prob < threshold_lose ~ "lose",
      home_team_win_prob < threshold_win ~ "draw",
      TRUE ~ "win"
    )
  )

data_team_predicted_summary =
  data_team_predicted |>
  select(home_team,
         away_team,
         home_team_win_prob,
         result)
# filter(home_team == "Mexico")

data_team_predicted_summary |>
  data.table::fwrite("files/dataset/output/c01_data_team_predicted_summary.csv")
data_team_predicted_summary |>
  saveRDS("files/dataset/output/c01_data_team_predicted_summary.RDS")
