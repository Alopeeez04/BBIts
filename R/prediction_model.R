# =========================
# PREDICTION MODEL FUNCTIONS
# Functions for scoring new patients using the empirical model
# =========================

# Load required libraries
suppressPackageStartupMessages({
  library(dplyr)
})

# =========================
# SCORE NEW PATIENT FUNCTION
# =========================
score_new_patient <- function(new_patient, empirical_table, cluster_cols) {
  
  # Ensure input is one-row dataframe
  stopifnot(nrow(new_patient) == 1)
  
  # Filter empirical table to matching variable-value pairs
  scores <- empirical_table %>%
    filter(
      variable %in% names(new_patient)
    ) %>%
    rowwise() %>%
    mutate(
      match = as.character(new_patient[[variable]]) == value
    ) %>%
    ungroup() %>%
    filter(match)
  
  # Aggregate cluster probabilities
  result <- scores %>%
    summarise(
      across(all_of(cluster_cols), ~ mean(.x, na.rm = TRUE))
    )
  
  # Normalize to 100%
  result <- result %>%
    mutate(
      total = rowSums(across(all_of(cluster_cols))),
      across(all_of(cluster_cols), ~ round(100 * .x / total, 1))
    ) %>%
    select(-total)
  
  return(result)
}

# =========================
# LOAD MODEL OBJECTS
# =========================
load_model_objects <- function(path = "data/model_objects.RData") {
  load(path, envir = environment())
  list(
    empirical_summary_wide = empirical_summary_wide,
    cluster_columns = cluster_columns
  )
}

# =========================
# PREDICT FROM INPUTS
# =========================
predict_from_inputs <- function(new_patient_row) {
  objs <- load_model_objects()
  score_new_patient(
    new_patient = new_patient_row,
    empirical_table = objs$empirical_summary_wide,
    cluster_cols = objs$cluster_columns
  )
}
