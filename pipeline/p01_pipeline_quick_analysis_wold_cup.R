# Setup -------------------------------------------------------------------

source("init.R")


# Pipeline ----------------------------------------------------------------

source("preprocessing/a01_read_data_and_EDA.R")

source("models/b02_nuevo_modelo")

source("execution/c01_prediction.R")

source("execution/c02_mean_score_per_match.R")

source("execution/c03_expected_match_result.R")

source("execution/c04_group_phase_analysis.R")


