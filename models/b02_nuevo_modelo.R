# Setup -------------------------------------------------------------------

source("init.R")
library(h2o)
h2o.init()

# Read data ---------------------------------------------------------------

file = paste0(path_imput,
              "international_matches.csv")
data = data.table::fread(file)

data_team = 
  readRDS("files/modeling_output/figures/a01_data_team.RDS")

# Data of interest with home and away -------------------------------------

data_team_of_interest_home_away =
  data |>
  filter((home_team %in% teams_in_analysis) |
           (away_team %in% teams_in_analysis),
         date > lubridate::ymd(last_years_threshold)
  ) |>
  mutate(bool_home_team_won = home_team_result == "Win")

# Defining training set ---------------------------------------------------

data_train =
  data_team_of_interest_home_away |>
  select(-date,
         -home_team_score ,
         -away_team_score) |>
  mutate(
    diff_fifa_rank = home_team_fifa_rank - away_team_fifa_rank,
    diff_fifa_points = home_team_total_fifa_points - away_team_total_fifa_points
  )

train_h2o =
  data_train |>
  as.h2o()

predictors =
  data_train |>
  select(ends_with("_score") |
           ends_with("_fifa_points") |
           ends_with("_fifa_rank")) |>
  names()

response = "bool_home_team_won"


# Training model ----------------------------------------------------------

home_team_win_drf =
  h2o.randomForest(
    x = predictors,
    y = response,
    training_frame = train_h2o,
    nfolds = 4,
    seed = 1234,
    ntrees = 200,
    max_depth = 3,
    model_id = "RF_Home_Win"
    # mtries = 6
    # (dim(train_h2o)[2] / 5) |>
    # sqrt() |>
    # round()
  )

var_imp =
  home_team_win_drf |>
  h2o.varimp()

var_imp_plot =
  home_team_win_drf |>
  h2o.varimp_plot()

var_imp_plot

home_team_win_drf |>
  h2o.performance()

home_team_win_drf |> 
  h2o.saveModel(path = "files/modeling_output/model_fit", 
                force = T, 
                filename = "b01_rf.model")
  
# Prediction Dataset ------------------------------------------------------

data_team_mean =
  data_team |>
  filter(date > "2021-01-01") |>
  # filter(team %in% teams_in_analysis) |>
  select(-c(date,
            team_continent,
            team_result,
            team_score)) |>
  group_by(team) |>
  summarise_all(.funs = mean)

data_team_mean_home =
  data_team_mean |>
  rename_at(
    .vars = vars(starts_with("team")),
    .funs = function(x) {
      str_replace_all(string = x,
                      pattern = "team",
                      replacement = "home_team")
    }
  )

data_team_mean_away =
  data_team_mean |>
  rename_at(
    .vars = vars(starts_with("team")),
    .funs = function(x) {
      str_replace_all(string = x,
                      pattern = "team",
                      replacement = "away_team")
    }
  )

data_team_cross =
  data_team_mean_home |>
  crossing(data_team_mean_away) |>
  filter(home_team != away_team) |>
  mutate(
    diff_fifa_rank = home_team_fifa_rank - away_team_fifa_rank,
    diff_fifa_points = home_team_total_fifa_points - away_team_total_fifa_points
  )

data_team_cross |> 
  saveRDS("files/dataset/intermedia/b01_data_team_cross.RDS")