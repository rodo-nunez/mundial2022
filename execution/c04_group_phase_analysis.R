# Setup -------------------------------------------------------------------

source("init.R")

# Read data ---------------------------------------------------------------

team_score =
  readRDS("files/dataset/output/c03_team_score.RDS")

# Group phase Analysis -----------------------------------------------------

team_final_score =
  team_score %>%
  filter(home_team %in% teams_group_C) %>%
  filter(away_team %in% teams_group_C) %>%
  pivot_longer(cols = c(home_team, away_team),
               values_to = "team_name") %>%
  mutate(
    final_score = if_else(name == 'away_team', away_score, home_score),
    final_cond_score = if_else(name == 'away_team', away_cond_score, home_cond_score)
  )

team_final_group_phase_score =
  team_final_score %>%
  group_by(team_name) %>%
  summarize(
    total_score = sum(final_score),
    total_cond_score = sum(final_cond_score)
  )

# Save data ---------------------------------------------------------------

team_final_group_phase_score |>
  data.table::fwrite("files/dataset/output/c04_team_final_group_phase_score.csv")
team_final_group_phase_score |>
  saveRDS("files/dataset/output/c04_team_final_group_phase_score.RDS")

