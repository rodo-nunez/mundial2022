# Useful options ----------------------------------------------------------

options("h2o.use.data.table" = TRUE)
options(stringsAsFactors = FALSE)
options(java.parameters = "-Xmx4096m")
options(dplyr.width = Inf)
Sys.setenv(TZ = 'GMT')
if ("data.table" %in% rownames(installed.packages())) {
  data.table::setDTthreads(0)
}
if ("tidyverse" %in% rownames(installed.packages())) {
  options(tidyverse.quiet = TRUE)
}

# Proyect Parameters ------------------------------------------------------

project_src_path = paste0(getwd(), "/") # Si se pone rstudioapi::, solo se puede usar desde rstudio, no desde shiny
project_files.path = paste0(project_src_path, "files/")
project_name = basename(project_src_path)

# Path definitions --------------------------------------------------------

path_imput = "files/dataset/input/"
path_figures = "files/modeling_output/figures/"

# Libraries ---------------------------------------------------------------

library(tidyverse)

# Source custom functions  ------------------------------------------------

source("functions/get_plots_fifa_rank.R")

# Analysis group ----------------------------------------------------------

teams_group_C = c("Mexico", "Argentina", "Saudi Arabia", "Poland")
teams_group_A = c("Qatar", "Ecuador", "Senegal", "Netherlands")
teams_in_analysis = c(teams_group_C, teams_group_A)
last_years_threshold = "2017-01-01"
threshold_lose = 0.4
threshold_win = 0.6