# Load libraries ----------------------------------------------------------

library(tidyverse)
library(h2o)
h2o.init()
source("functions/get_plots_fifa_rank.R")

# Path definitions --------------------------------------------------------

path_imput = "files/dataset/input/"
path_figures = "files/modeling_output/figures/"

# Read data ---------------------------------------------------------------

file = paste0(path_imput,
              "international_matches.csv")
data = data.table::fread(file)

data |>
  head()

file_world_cups = paste0(path_imput,
                         "WorldCupMatches.csv")
data_world_cups = data.table::fread(file_world_cups)

last_years_threshold = "2017-01-01"

# Create team dataset -----------------------------------------------------

data_home_team =
  data |>
  select_at(vars("date" | starts_with("home_team"))) |>
  rename_at(
    .vars = vars(starts_with("home_team")),
    .funs = function(x) {
      str_replace_all(string = x,
                      pattern = "home_team",
                      replacement = "team")
    }
  )

data_away_team =
  data |>
  select_at(vars("date" | starts_with("away_team"))) |>
  rename_at(
    .vars = vars(starts_with("away_team")),
    .funs = function(x) {
      str_replace_all(string = x,
                      pattern = "away_team",
                      replacement = "team")
    }
  )

data_team =
  data_home_team |>
  bind_rows(data_away_team)


# Analysis group ----------------------------------------------------------

teams_group_C = c("Mexico", "Argentina", "Saudi Arabia", "Poland")
teams_group_A = c("Qatar", "Ecuador", "Senegal", "Netherlands")
teams_in_analysis = c(teams_group_C, teams_group_A)

# plots_fifa_rank groups C and A ------------------------------------------

plots_groups_A_C =
  get_plots_fifa_rank(
    data_team = data_team,
    teams_in_analysis = teams_in_analysis,
    path_figures = teams_in_analysis,
    last_years_threshold = last_years_threshold
  )

plots_groups_A_C$plot_fifa_rank_evolution

jpeg(paste0(path_figures,
            "plot_fifa_rank_evolution_A_C.jpeg"))
plots_groups_A_C$plot_fifa_rank_evolution
dev.off()

pdf(paste0(path_figures,
           "plot_fifa_rank_evolution_A_C.pdf"))
plots_groups_A_C$plot_fifa_rank_evolution
dev.off()

plots_groups_A_C$plot_team_metrics

jpeg(paste0(path_figures,
            "plot_team_metrics_A_C.jpeg"))
plots_groups_A_C$plot_team_metrics
dev.off()

pdf(paste0(path_figures,
           "plot_team_metrics_A_C.pdf"))
plots_groups_A_C$plot_team_metrics
dev.off()

# Find teams at stage -----------------------------------------------------

teams_at_phase =
  data_world_cups |>
  filter(Stage == "Quarter-finals") |>
  filter(`Home Team Name` == "Mexico" |
           `Away Team Name` == "Mexico") |>
  select(Datetime,
         Year,
         `Home Team Name`,
         `Away Team Name`)

teams_at_phase_simplified =
  teams_at_phase |>
  select(-Datetime,
         -Year)

teams_at_phase_vector =
  teams_at_phase_simplified |>
  select(`Home Team Name`) |>
  unlist() |>
  unname() |>
  c(teams_at_phase_simplified |>
      select(`Away Team Name`) |>
      unlist() |>
      unname()) |>
  unique()

teams_at_phase_vector =
  teams_at_phase_vector |>
  str_remove(" FR")

plots_groups_quarter =
  get_plots_fifa_rank(
    data_team = data_team,
    teams_in_analysis = teams_at_phase_vector,
    path_figures = teams_in_analysis,
    last_years_threshold = last_years_threshold
  )

plots_groups_quarter$plot_fifa_rank_evolution

jpeg(paste0(path_figures,
            "plot_fifa_rank_evolution_quarters.jpeg"))
plots_groups_quarter$plot_fifa_rank_evolution
dev.off()

pdf(paste0(path_figures,
           "plot_fifa_rank_evolution_quarters.pdf"))
plots_groups_quarter$plot_fifa_rank_evolution
dev.off()

plots_groups_quarter$plot_team_metrics

jpeg(paste0(path_figures,
            "plot_team_metrics_quarters.jpeg"))
plots_groups_quarter$plot_team_metrics
dev.off()

pdf(paste0(path_figures,
           "plot_team_metrics_quarters.pdf"))
plots_groups_quarter$plot_team_metrics
dev.off()
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

# Prediction Dataset ------------------------------------------------------

data_team_mean =
  data_team |>
  filter(date > "2021-01-01") |>
  filter(team %in% teams_in_analysis) |>
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

# Prediction --------------------------------------------------------------

threshold_lose = 0.4
threshold_win = 0.6

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
  mutate(result = case_when(home_team_win_prob < threshold_lose ~ "lose",
                            home_team_win_prob < threshold_win ~ "draw",
                            TRUE ~ "win"))

data_team_predicted_summary =
  data_team_predicted |>
  select(home_team,
         away_team,
         home_team_win_prob,
         result) 
  # filter(home_team == "Mexico")

data_team_predicted_summary |> 
  data.table::fwrite("files/dataset/output/data_team_predicted_summary.csv")


# Getting last performance metrics ----------------------------------------

last_years_threshold = "2021-01-01"

mean_stats_teams_in_analysis =
  data_team |>
  select(date,
         team,
         predictors |>
           str_remove("home_") |>
           str_remove("away_")) |>
  filter(team %in% teams_in_analysis,
         date > (last_years_threshold |>
                   lubridate::ymd())) |>
  select(-date) |>
  group_by(team) |>
  summarise_all( ~ round(mean(., na.rm = T),
                         digits = 2)) |>
  arrange(team_fifa_rank) |>
  select(team,
         team_fifa_rank,
         team_total_fifa_points,
         everything())

mean_stats_teams_in_analysis_table =
  mean_stats_teams_in_analysis |>
  gt::gt()

mean_stats_teams_in_analysis_table

mean_stats_teams_in_analysis_table |>
  gt::gtsave(filename = "mean_stats_teams_in_analysis_table.pdf",
             path = path_figures)
