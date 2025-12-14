library(dplyr)

compute_relapse_time <- function(data_rec,
                                 surgery_col = "fecha_qx",
                                 relapse_flag_col = "dx_recidiva",
                                 followup_col = "Ultima_fecha",
                                 relapse_date_col = "fecha_de_recidi",
                                 date_format_surgery = "%d/%m/%Y") {
  data_rec %>%
    mutate(
      surgery_date  = as.Date(.data[[surgery_col]], format = date_format_surgery),
      followup_date = as.Date(gsub(" UTC","", .data[[followup_col]])),
      relapse_date  = as.Date(gsub(" UTC","", .data[[relapse_date_col]])),
      relapse_event = !is.na(.data[[relapse_flag_col]]) & .data[[relapse_flag_col]] == 1,
      event_date    = case_when(
        relapse_event ~ relapse_date,
        TRUE          ~ followup_date
      ),
      tte_days      = as.numeric(event_date - surgery_date)
    )
}

summarize_relapse_by_cluster <- function(df, cluster_col = "cluster_label",
                                         stat = c("median","mean")) {
  stat <- match.arg(stat)
  df %>%
    filter(!is.na(.data[[cluster_col]]), !is.na(tte_days), tte_days >= 0) %>%
    group_by(.data[[cluster_col]]) %>%
    summarise(
      n_events = sum(relapse_event),
      days_median = median(tte_days),
      days_mean   = mean(tte_days),
      .groups = "drop"
    ) %>%
    mutate(relapse_days = if (stat == "median") days_median else days_mean)
}

combine_probs_to_days <- function(probs_named, cluster_stats,
                                  cluster_map = c("Alto riesgo" = "Alto riesgo",
                                                  "Buen pronóstico" = "Buen pronóstico",
                                                  "Mixto" = "Mixto"),
                                  probs_in_percent = TRUE) {
  p <- probs_named
  if (probs_in_percent) p <- p / 100
  combo <- tibble(
    prob_name   = names(p),
    prob_value  = as.numeric(p),
    cluster_lab = unname(cluster_map[prob_name])
  ) %>%
    left_join(cluster_stats %>% dplyr::select(cluster_label, relapse_days),
              by = c("cluster_lab" = "cluster_label")) %>%
    filter(!is.na(relapse_days), !is.na(prob_value))
  if (nrow(combo) == 0) return(NA_real_)
  sum(combo$prob_value * combo$relapse_days) / sum(combo$prob_value)
}

predict_relapse_days <- function(new_patient_df,
                                 empirical_table,
                                 cluster_cols = c("Alto riesgo","Buen pronóstico","Mixto"),
                                 data_rec = NULL,
                                 surgery_date = Sys.Date(),
                                 cluster_col = "cluster_label",
                                 stat = c("median","mean")) {
  stat <- match.arg(stat)
  
  if (!exists("score_new_patient")) {
    source("R/prediction_model.R", local = TRUE)
  }
  
  probs_tbl <- score_new_patient(new_patient_df, empirical_table, cluster_cols)
  if (nrow(probs_tbl) == 0 || anyNA(probs_tbl[cluster_cols])) {
    return(list(pred_days = NA_real_, pred_date = as.Date(NA), probs = probs_tbl, cluster_stats = NULL))
  }
  
  probs_named <- c(
    `Alto riesgo` = probs_tbl$`Alto riesgo`,
    `Buen pronóstico` = probs_tbl$`Buen pronóstico`,
    `Mixto` = probs_tbl$Mixto
  )
  
  if (is.null(data_rec)) {
    return(list(pred_days = NA_real_, pred_date = as.Date(NA), probs = probs_named, cluster_stats = NULL))
  }
  
  required_cols <- c("fecha_qx", "Ultima_fecha", "fecha_de_recidi", "dx_recidiva", cluster_col)
  if (!all(required_cols %in% names(data_rec))) {
    return(list(pred_days = NA_real_, pred_date = as.Date(NA), probs = probs_named, cluster_stats = NULL))
  }
  
  df_tte <- compute_relapse_time(data_rec)
  cl_stats <- summarize_relapse_by_cluster(df_tte, cluster_col = cluster_col, stat = stat)
  pred_days <- combine_probs_to_days(probs_named, cl_stats, probs_in_percent = TRUE)
  pred_date <- if (!is.na(pred_days)) as.Date(surgery_date) + round(pred_days) else as.Date(NA)
  list(pred_days = pred_days, pred_date = pred_date,
       probs = probs_named, cluster_stats = cl_stats)
}
