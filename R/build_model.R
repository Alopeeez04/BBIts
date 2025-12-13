# =========================
# MODEL BUILDING SCRIPT
# Builds empirical summary and cluster columns from raw data
# =========================
build_model_objects <- function() {
  
  # Load required libraries
  suppressPackageStartupMessages({
    library(readxl)
    library(dplyr)
    library(tidyr)
    library(forcats)
    library(FactoMineR)
  })
  
  # =========================
  # 1. LOAD AND PREPARE DATA
  # =========================
  cat("Loading data from data/data_hack.xlsx...\n")
  data <- read_excel("data/data_hack.xlsx")
  
  # Columns to keep
  columnas_a_conservar <- c(
    "codigo_participante", "recidiva", "recidiva_exitus", "diferencia_dias_reci_exit",
    "causa_muerte", "f_diag", "fecha_de_recidi", "f_muerte", "Ultima_fecha",
    "edad", "imc", "tipo_histologico", "Grado", "valor_de_ca125", "ecotv_infiltsub",
    "metasta_distan", "libre_enferm", "numero_de_recid", "recid_super_1", "dx_recidiva",
    "num_recidiva", "loc_recidiva_r01", "loc_recidiva_r03", "loc_recidiva_r04",
    "loc_recidiva_r05", "loc_recidiva_r06", "tto_recidiva", "Tt_recidiva_qx",
    "otro_ttIQ_recid", "Reseccion_macroscopica_complet", "Tratamiento_RT",
    "Tratamiento_sistemico", "estado", "tto_1_quirugico", "fecha_qx",
    "recep_est_porcent", "rece_de_Ppor", "p53_molecular", "p53_ihq", "mut_pole",
    "beta_cateninap", "msh2", "msh6", "pms2", "mlh1", "estudio_genetico_r01",
    "estudio_genetico_r02", "estudio_genetico_r03", "estudio_genetico_r04",
    "estudio_genetico_r05", "estudio_genetico_r06", "Tratamiento_sistemico_realizad",
    "grupo_de_riesgo_definitivo", "Tributaria_a_Radioterapia", "asa", "histo_defin",
    "afectacion_linf", "grado_histologi", "tamano_tumoral", "AP_centinela_pelvico",
    "AP_ganPelv"
  )
  
  data_reducido <- data %>% select(all_of(columnas_a_conservar))
  
  # Fix inconsistencies between death_status and f_muerte
  data_reducido <- data_reducido %>%
    mutate(
      death_status = ifelse(recidiva_exitus == 1, "Deceased", "Alive"),
      death_status = case_when(
        !is.na(f_muerte) ~ "Deceased",
        is.na(f_muerte)  ~ "Alive"
      ),
      recidiva_exitus = ifelse(death_status == "Deceased", 1, 0)
    )
  
  # Remove columns with >50% missing
  data_limpia <- data_reducido %>%
    select(where(~ mean(is.na(.)) < 0.5))
  
  # Prepare outcome variable
  data_rec <- data_limpia %>%
    mutate(recidiva_bin = ifelse(recidiva %in% c(0,1), recidiva, NA)) %>%
    filter(!is.na(recidiva_bin))
  
  # Collapse tipo_histologico
  data_rec <- data_rec %>%
    mutate(tipo_histologico_collapsed = fct_lump_min(factor(tipo_histologico), min = 5))
  
  # Convert to factors with proper labels
  data_rec <- data_rec %>%
    mutate(
      recidiva_bin = factor(recidiva_bin, levels = c(0,1), labels = c("NoRecidiva","Recidiva")),
      Grado = factor(Grado, levels = c(1,2), labels = c("Bajo","Alto")),
      grado_histologi = factor(grado_histologi, levels = c(1,2), labels = c("Bajo","Alto")),
      libre_enferm = factor(libre_enferm, levels = c(0,1,2), labels = c("No","Si","Desconocido")),
      metasta_distan = factor(metasta_distan, levels = c(0,1), labels = c("No","Si")),
      Tratamiento_RT = factor(Tratamiento_RT, levels = c(0,1), labels = c("No","Si")),
      Tratamiento_sistemico = factor(Tratamiento_sistemico, levels = c(0,1), labels = c("No","Si")),
      Tributaria_a_Radioterapia = factor(Tributaria_a_Radioterapia, levels = c(0,1), labels = c("No","Si")),
      asa = factor(asa, levels = c(0,1,2,3,4,5,6),
                   labels = c("ASA1","ASA2","ASA3","ASA4","ASA5","ASA6","Desconocido")),
      afectacion_linf = factor(afectacion_linf, levels = c(0,1), labels = c("No","Si")),
      AP_centinela_pelvico = factor(AP_centinela_pelvico, levels = c(0,1,2,3,4),
                                    labels = c("pN0","pN0(i+)","pN1(mi)","pN1","pNx")),
      AP_ganPelv = factor(AP_ganPelv, levels = c(0,1,2,3),
                          labels = c("Negativo","Cels_aisladas","Micrometastasis","Macrometastasis")),
      grupo_de_riesgo_definitivo = factor(grupo_de_riesgo_definitivo, levels = c(1,2,3,4,5),
                                          labels = c("Bajo","Intermedio","Intermedio_alto","Alto","Avanzados"))
    )
  
  # =========================
  # 2. CLUSTERING ANALYSIS
  # =========================
  cat("Performing clustering analysis...\n")
  
  # Variables for clustering
  vars_sig <- c("Grado", "libre_enferm", "beta_cateninap", "mlh1", 
                "grupo_de_riesgo_definitivo", "Tributaria_a_Radioterapia",
                "afectacion_linf", "grado_histologi", "AP_centinela_pelvico",
                "AP_ganPelv", "tipo_histologico_collapsed")
  
  data_sig <- data_rec %>%
    select(all_of(vars_sig))
  
  # Convert to factors
  data_sig <- data_sig %>% mutate(across(where(is.character), as.factor))
  
  # Handle missing values for FAMD
  data_sig <- data_sig %>% mutate(across(where(is.factor), ~addNA(.)))
  
  # Perform FAMD (Factor Analysis of Mixed Data)
  famd_res <- FAMD(data_sig, graph = FALSE)
  
  # Get PCA scores (first 2 dimensions)
  pca_scores <- famd_res$ind$coord[,1:2]
  
  # K-means clustering on PCA scores
  set.seed(123)
  km_res <- kmeans(pca_scores, centers = 3, nstart = 25)
  
  # Add cluster labels to data_rec
  clusters_full <- km_res$cluster
  data_rec <- data_rec %>%
    mutate(cluster = factor(clusters_full),
           cluster_label = case_when(
             cluster == 1 ~ "Buen pronóstico",
             cluster == 2 ~ "Alto riesgo",
             cluster == 3 ~ "Mixto"
           ))
  
  # =========================
  # 3. CREATE EMPIRICAL SUMMARY
  # =========================
  cat("Creating empirical summary table...\n")
  
  # Variables for empirical summary (note: using tipo_histologico, not collapsed)
  vars_sig_empirical <- c("Grado", "libre_enferm", "beta_cateninap", "mlh1",
                          "grupo_de_riesgo_definitivo", "Tributaria_a_Radioterapia",
                          "afectacion_linf", "grado_histologi", "AP_centinela_pelvico",
                          "AP_ganPelv", "tipo_histologico")
  
  empirical_results <- list()
  
  for (var in vars_sig_empirical) {
    df <- data_rec %>%
      group_by(.data[[var]], cluster_label) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(.data[[var]]) %>%
      mutate(prob = round(100 * n / sum(n), 1),
             variable = var) %>%
      mutate(value = as.character(.data[[var]])) %>%
      select(variable, value, cluster_label, prob)
    
    empirical_results[[var]] <- df
  }
  
  # Combine into one dataframe
  empirical_summary <- bind_rows(empirical_results)
  
  # Pivot wider: cluster probabilities side by side
  empirical_summary_wide <- empirical_summary %>%
    pivot_wider(names_from = cluster_label, values_from = prob)
  
  # =========================
  # 4. DEFINE CLUSTER COLUMNS
  # =========================
  cluster_columns <- c("Alto riesgo", "Buen pronóstico", "Mixto")
  
  cat("Model building complete!\n")
  cat("  - Empirical summary rows:", nrow(empirical_summary_wide), "\n")
  cat("  - Cluster columns:", paste(cluster_columns, collapse = ", "), "\n")
  
  # =========================
  # 5. SAVE OBJECTS
  # =========================
  save(empirical_summary_wide, cluster_columns,
       file = "data/model_objects.RData")
  
  cat("Objects saved to data/model_objects.RData\n")
  
  return(list(
    empirical_summary_wide = empirical_summary_wide,
    cluster_columns = cluster_columns
  ))
}

# Run the function when script is sourced
build_model_objects()
