# Setup -------------------------------------------------------------------

source("init.R")

# Read data ---------------------------------------------------------------

file = paste0(path_imput,
              "international_matches.csv")
data = data.table::fread(file)

file_world_cups = paste0(path_imput,
                         "WorldCupMatches.csv")
data_world_cups = data.table::fread(file_world_cups)

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

data_team |> 
  saveRDS("files/modeling_output/figures/a01_data_team.RDS")

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
            "a01_plot_fifa_rank_evolution_A_C.jpeg"))
plots_groups_A_C$plot_fifa_rank_evolution
dev.off()

pdf(paste0(path_figures,
           "a01_plot_fifa_rank_evolution_A_C.pdf"))
plots_groups_A_C$plot_fifa_rank_evolution
dev.off()

plots_groups_A_C$plot_team_metrics

jpeg(paste0(path_figures,
            "a01_plot_team_metrics_A_C.jpeg"))
plots_groups_A_C$plot_team_metrics
dev.off()

pdf(paste0(path_figures,
           "a01_plot_team_metrics_A_C.pdf"))
plots_groups_A_C$plot_team_metrics
dev.off()

plots_groups_A_C |> 
  saveRDS("files/modeling_output/figures/a01_plots_groups_A_C.RDS")

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
            "a01_plot_fifa_rank_evolution_quarters.jpeg"))
plots_groups_quarter$plot_fifa_rank_evolution
dev.off()

pdf(paste0(path_figures,
           "a01_plot_fifa_rank_evolution_quarters.pdf"))
plots_groups_quarter$plot_fifa_rank_evolution
dev.off()

plots_groups_quarter$plot_team_metrics

jpeg(paste0(path_figures,
            "a01_plot_team_metrics_quarters.jpeg"))
plots_groups_quarter$plot_team_metrics
dev.off()

pdf(paste0(path_figures,
           "a01_plot_team_metrics_quarters.pdf"))
plots_groups_quarter$plot_team_metrics
dev.off()

plots_groups_quarter |> 
  saveRDS("files/modeling_output/figures/a01_plots_groups_quarter.RDS")