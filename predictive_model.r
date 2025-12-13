library(dplyr)
library(lubridate)
library(survival)
library(cmprsk)

## ---------------------------------------------------------
## load_and_prepare_patient_dataset()
## ---------------------------------------------------------
## Purpose
## - Reads raw CSV and applies the same standardization you already use:
##   * Parses dates
##   * Renames columns to a consistent English naming convention
##   * Creates age_group (categorical)
##   * Applies basic inclusion rules (diagnostic_date/last_date present; recurrence != 2)
##
## Inputs
## - csv_path: path to your CSV file (e.g. "data_casted.csv")
##
## Output
## - df2: cleaned dataset ready for downstream filtering & analysis
##

load_and_prepare_patient_dataset <- function(csv_path) {
  datos <- read.csv2(csv_path, sep = ",")
  df <- datos %>%
    filter(!is.na(f_diag)) %>%
    mutate(
      f_diag          = dmy(f_diag),
      Ultima_fecha    = dmy(Ultima_fecha),
      fecha_de_recidi = dmy(fecha_de_recidi),
      f_muerte        = dmy(f_muerte),
      visita_control  = dmy(visita_control),
      edad = as.integer(edad)
    ) %>%
    rename(
      participant_code   = codigo_participante,
      rec_date_raw       = fecha_de_recidi,
      death_date         = f_muerte,
      last_date          = Ultima_fecha,
      diagnostic_date    = f_diag,
      recurrence         = recidiva,
      recurrence_exitus  = recidiva_exitus,
      age                = edad
    )
  
  core_cols <- c(
    "participant_code",
    "diagnostic_date",
    "rec_date_raw",
    "death_date",
    "last_date",
    "visita_control",
    "recurrence",
    "recurrence_exitus"
  )
  
  covariates_set <- c(
    "Grado",
    "libre_enferm",
    "beta_cateninap",
    "mlh1",
    "grupo_de_riesgo_definitivo",
    "Tributaria_a_Radioterapia",
    "afectacion_linf",
    "grado_histologi",
    "AP_centinela_pelvico",
    "AP_ganPelv",
    "tipo_histologico"
  )
  
  df2 <- df %>%
    select(all_of(unique(c(core_cols,covariates_set)))) %>%
    filter(!is.na(diagnostic_date), !is.na(last_date)) %>%
    filter(recurrence != 2)
  
  df2
}

## ---------------------------------------------------------
## build_competing_risk_dataset()
## ---------------------------------------------------------
## Purpose
## - Converts a cleaned patient dataset (df2) into an analysis dataset (df_cr)
##   containing:
##   * time_days: time from diagnosis to first event/censoring
##   * status:   event type indicator with competing risks:
##       0 = censored (no recurrence or death before admin censor)
##       1 = recurrence happens first (before death and before admin censor)
##       2 = death happens first (before recurrence and before admin censor)
##
## Inputs
## - df_in: a df2-like dataset (must contain the standardized columns)
##
## Output
## - df_cr: dataset ready for cuminc() and cause-specific Cox / KM
##
build_competing_risk_dataset <- function(df_in) {
  df_in %>%
    mutate(
      # recurrence date only defined if recurrence==1
      rec_date = if_else(recurrence == 1L, rec_date_raw, as.Date(NA)),
      admin_censor = last_date,
      
      # determine first event/censor date
      first_event_date = pmin(rec_date, death_date, admin_censor, na.rm = TRUE),
      first_event_date = if_else(is.infinite(first_event_date), admin_censor, first_event_date),
      
      # event type
      status = case_when(
        !is.na(rec_date)   & rec_date   <= admin_censor & (is.na(death_date) | rec_date   <= death_date) ~ 1L,
        !is.na(death_date) & death_date <= admin_censor & (is.na(rec_date)   | death_date <  rec_date)   ~ 2L,
        TRUE ~ 0L
      ),
      
      time_days = as.numeric(first_event_date - diagnostic_date)
    ) %>%
    filter(!is.na(time_days) & time_days >= 0)
}

## ---------------------------------------------------------
## filter_dataset_by_fields()
## ---------------------------------------------------------
## Purpose
## - Applies structured filtering using:
##   * values: exact matching sets (e.g., age_group in c("<50","≥70"))
##   * ranges: inclusive numeric range filters (e.g., age between 30 and 85)
##   * drop_na_cols: optionally remove rows with NA in specified columns
##
## Inputs
## - df: input dataset (usually df2)
## - values: named list(col = c(v1, v2, ...))
## - ranges: named list(col = c(min, max))
## - drop_na_cols: character vector of columns for which NA rows should be removed
##
## Output
## - filtered dataset
##
filter_dataset_by_fields <- function(df,
                                     values = list(),
                                     ranges = list(),
                                     drop_na_cols = character()) {
  out <- df
  
  if (length(drop_na_cols) > 0) {
    out <- out %>% filter(if_all(all_of(drop_na_cols), ~ !is.na(.x)))
  }
  
  if (length(values) > 0) {
    for (nm in names(values)) {
      out <- out %>% filter(.data[[nm]] %in% values[[nm]])
    }
  }
  
  if (length(ranges) > 0) {
    for (nm in names(ranges)) {
      rng <- ranges[[nm]]
      out <- out %>% filter(.data[[nm]] >= rng[1], .data[[nm]] <= rng[2])
    }
  }
  
  out
}

## ---------------------------------------------------------
## filter_dataset_with_dplyr_conditions()
## ---------------------------------------------------------
## Purpose
## - Flexible filtering using raw dplyr conditions.
##
## Example
## - filter_dataset_with_dplyr_conditions(df2, age_group %in% c("<50","≥70"), age >= 40)
##
filter_dataset_with_dplyr_conditions <- function(df, ...) {
  df %>% filter(...)
}

## ---------------------------------------------------------
## create_output_directory()
## ---------------------------------------------------------
## Purpose
## - Creates a directory for plots and summaries.
## - Tags are appended to the folder name to make outputs easy to track.
##
## Inputs
## - prefix: base folder name (e.g., "plots_optionA")
## - tags: character vector used to create a unique suffix
##
## Output
## - out_dir: path to created directory
##
create_output_directory <- function(prefix, tags = NULL) {
  tag_str <- if (is.null(tags)) "" else paste0("_", gsub("[^A-Za-z0-9]+", "_", paste(tags, collapse = "_")))
  out_dir <- paste0(prefix, tag_str)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  out_dir
}

plot_file_png <- function(out_dir, name) file.path(out_dir, paste0(name, ".png"))

## ---------------------------------------------------------
## plot_cumulative_incidence_overall()
## ---------------------------------------------------------
## Purpose
## - Plots overall CIF in the subset (no stratification).
##
plot_cumulative_incidence_overall <- function(df_cr_subset, out_dir, file = "02_cif_overall") {
  ci <- cmprsk::cuminc(ftime = df_cr_subset$time_days, fstatus = df_cr_subset$status)
  
  png(plot_file_png(out_dir, file), width = 1400, height = 900, res = 140)
  par(mar = c(5, 5, 3, 1))
  plot(ci, xlab = "Days since diagnosis", ylab = "Cumulative incidence",
       main = "Cumulative incidence (overall in subset)")
  dev.off()
  
  invisible(ci)
}

## ---------------------------------------------------------
## plot_cumulative_incidence_by_group()
## ---------------------------------------------------------
## Purpose
## - CIF by a grouping column (default age_group) within the subset.
##
plot_cumulative_incidence_overall <- function(df_cr_subset,
                                              out_dir,
                                              file = "03_cif_overall") {
  
  ci <- cmprsk::cuminc(
    ftime   = df_cr_subset$time_days,
    fstatus = df_cr_subset$status
  )
  
  png(plot_file_png(out_dir, file), width = 1400, height = 900, res = 140)
  apply_base_theme()
  
  plot(
    ci,
    col  = c("#1b9e77", "#d95f02"),  # recurrence / death
    lwd  = 2,
    xlab = "Days since diagnosis",
    ylab = "Cumulative incidence",
    main = "Cumulative incidence (overall)"
  )
  
  grid(col = "grey90", lty = "dotted")
  legend(
    "topleft",
    legend = names(ci),
    col    = c("#1b9e77", "#d95f02"),
    lwd    = 2,
    bty    = "n"
  )
  
  dev.off()
  
  invisible(ci)
}

## ---------------------------------------------------------
## plot_km_time_to_recurrence_by_group()
## ---------------------------------------------------------
## Purpose
## - Cause-specific Kaplan–Meier for recurrence:
##   event = (status==1), death treated as censoring.
##
plot_km_time_to_recurrence <- function(df_cr_subset,
                                       out_dir,
                                       file = "04_km_recurrence_overall",
                                       conf_int = FALSE) {
  
  km <- survfit(
    Surv(time_days, status == 1L) ~ 1,
    data = df_cr_subset
  )
  
  png(plot_file_png(out_dir, file), width = 1400, height = 900, res = 140)
  par(mar = c(5, 5, 3, 1))
  plot(
    km,
    xlab = "Days since diagnosis",
    ylab = "Survival (no recurrence yet)",
    main = "Kaplan–Meier: Time to recurrence (overall)",
    lwd = 2,
    conf.int = conf_int
  )
  dev.off()
  
  invisible(km)
}

## ---------------------------------------------------------
## plot_km_time_to_death_by_group()
## ---------------------------------------------------------
## Purpose
## - Cause-specific Kaplan–Meier for death:
##   event = (status==2), recurrence treated as censoring.
##
plot_km_time_to_death <- function(df_cr_subset,
                                  out_dir,
                                  file = "05_km_death_overall",
                                  conf_int = FALSE) {
  
  km <- survfit(
    Surv(time_days, status == 2L) ~ 1,
    data = df_cr_subset
  )
  
  png(plot_file_png(out_dir, file), width = 1400, height = 900, res = 140)
  par(mar = c(5, 5, 3, 1))
  plot(
    km,
    xlab = "Days since diagnosis",
    ylab = "Survival (alive)",
    main = "Kaplan–Meier: Time to death (overall)",
    lwd = 2,
    conf.int = conf_int
  )
  dev.off()
  
  invisible(km)
}

## ---------------------------------------------------------
## fit_cause_specific_cox_models()
## ---------------------------------------------------------
## Purpose
## - Fits the same cause-specific Cox models you used:
##   * recurrence model: event=(status==1)
##   * death model:      event=(status==2)
##

fit_cause_specific_cox_models <- function(df_cr_subset) {
  
  fit_recur <- coxph(
    Surv(time_days, status == 1L) ~
      Grado +
      libre_enferm +
      beta_cateninap +
      mlh1 +
      grupo_de_riesgo_definitivo +
      Tributaria_a_Radioterapia +
      afectacion_linf +
      grado_histologi +
      AP_centinela_pelvico +
      AP_ganPelv +
      tipo_histologico,
    data = df_cr_subset
  )
  
  fit_death <- coxph(
    Surv(time_days, status == 2L) ~
      Grado +
      libre_enferm +
      beta_cateninap +
      mlh1 +
      grupo_de_riesgo_definitivo +
      Tributaria_a_Radioterapia +
      afectacion_linf +
      grado_histologi +
      AP_centinela_pelvico +
      AP_ganPelv +
      tipo_histologico,
    data = df_cr_subset
  )
  
  list(
    recurrence = fit_recur,
    death = fit_death
  )
  
  
}


## ---------------------------------------------------------
## write_model_summaries_to_file()
## ---------------------------------------------------------
## Purpose
## - Writes model summaries to a local .txt file.
##
write_model_summaries_to_file <- function(fits, out_dir, file = "model_summaries.txt") {
  sink(file.path(out_dir, file))
  cat("Cause-specific Cox: recurrence (event=status==1)\n")
  print(summary(fits$recurrence))
  cat("\n\nCause-specific Cox: death (event=status==2)\n")
  print(summary(fits$death))
  sink()
}

## ---------------------------------------------------------
## run_survival_analysis_pipeline()
## ---------------------------------------------------------
## Purpose
## - End-to-end runner:
##   * load → filter → build df_cr → plot → fit models → save summaries
##
## Filtering modes
## - Structured filtering:
##     values = list(age_group = c("<50","≥70"))
##     ranges = list(age = c(30, 85))
## - Raw dplyr filtering:
##     use_raw_filter = TRUE
##     ... = any filter expressions (e.g., age_group %in% c("<50","≥70"), age >= 40)
##
## Returns
## - a list with df2_subset, df_cr_subset, fitted models, output directory path
##
run_survival_analysis_pipeline <- function(csv_path,
                                           values = list(),
                                           ranges = list(),
                                           use_raw_filter = FALSE,
                                           ...,
                                           out_prefix = "plots_optionA",
                                           out_tags = NULL) {
  
  df2 <- load_and_prepare_patient_dataset(csv_path)
  
  df2_subset <- df2
  
  df_cr_subset <- build_competing_risk_dataset(df2_subset)
  
  out_dir <- create_output_directory(out_prefix, tags = out_tags)
  
  # plots
  plot_cumulative_incidence_overall(df_cr_subset, out_dir = out_dir)
  plot_km_time_to_recurrence(df_cr_subset, out_dir = out_dir)
  plot_km_time_to_death(df_cr_subset, out_dir = out_dir)
  
  # models + summaries
  fits <- fit_cause_specific_cox_models(df_cr_subset)
  write_model_summaries_to_file(fits, out_dir)
  
  message("Saved outputs to: ", normalizePath(out_dir))
  
  invisible(list(df2_subset = df2_subset, df_cr_subset = df_cr_subset, fits = fits, out_dir = out_dir))
}

## =========================================================
## Example usage
## =========================================================
# Example A: filter by two age groups (structured)
 res <- run_survival_analysis_pipeline(
   csv_path = "data_casted.csv"
 )

# Example B: raw dplyr conditions
# res <- run_survival_analysis_pipeline(
#   csv_path = "data_casted.csv",
#   use_raw_filter = TRUE,
#   age_group %in% c("<50", "≥70"),
#   age >= 40,
#   out_tags = c("raw_filter")
# )

