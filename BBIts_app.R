# install.packages(c("shiny","readxl","survival","survminer","glmnet","shinythemes","DT","forcats","lubridate"))

library(shiny)
library(readxl)
library(survival)
library(survminer)
library(glmnet)
library(shinythemes)
library(DT)
library(dplyr)
library(forcats)
library(lubridate)
library(tidyr)

# UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(tags$style(HTML("body { background-color: #efefef"))),
  titlePanel(
    div(
      style = "display: flex; justify-content: space-between; align-items: center;",
      h2("Predicció de Recidiva en Càncer d'Endometri", style = "margin: 0; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; font-size: 50px; font-weight: 600; color: #2c3e50;"),
      img(src = "https://res.cloudinary.com/teepublic/image/private/s--A6Tks3E1--/t_Resized%20Artwork/c_fit,g_north_west,h_1054,w_1054/co_ffffff,e_outline:53/co_ffffff,e_outline:inner_fill:53/co_bbbbbb,e_outline:3:1000/c_mpad,g_center,h_1260,w_1260/b_rgb:eeeeee/c_limit,f_auto,h_630,q_auto:good:420,w_630/v1630462262/production/designs/24030149_0.jpg", 
          height = "150px")
    )
  ),
  sidebarLayout(
    sidebarPanel(
      style = "background-color: #f2b29c;",
      fileInput("file", "Puja l'Excel (.xlsx)", accept = ".xlsx"),
      selectInput("sheet", "Hoja a usar", choices = NULL),
      actionButton("train", "Entrenar model"),
      hr(),
      h4("Introdueix un cas clínic"),
      # Choices populated dynamically after loading data
      selectInput('Grado_in','Grado', choices = NULL),
      selectInput('libre_enferm_in','Libre de enfermedad', choices = NULL),
      selectInput('beta_cateninap_in','Beta catenina', choices = NULL),
      selectInput('mlh1_in','MLH1', choices = NULL),
      selectInput('grupo_riesgo_in','Grupo de riesgo definitivo', choices = NULL),
      selectInput('Tributaria_in','Tributaria a Radioterapia', choices = NULL),
      selectInput('afectacion_linf_in','Afectación linfática', choices = NULL),
      selectInput('grado_histologi_in','Grado histológico', choices = NULL),
      selectInput('AP_centinela_in','AP centinela pélvico', choices = NULL),
      selectInput('AP_ganPelv_in','AP ganglios pélvicos', choices = NULL),
      selectInput('tipo_histologico_in','Tipo histológico', choices = NULL),
      dateInput("surgery_date", "Data de la cirurgia", 
                value = Sys.Date(), format = "dd/mm/yyyy"),
      actionButton("predict_btn", "Calcular probabilitat")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Preview dades", DTOutput("table")),
        tabPanel("Model",
                 verbatimTextOutput("model_summary"),
                 plotOutput("km_plot")),
        tabPanel("Predicció",
                 verbatimTextOutput("prediction_text"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  rv <- reactiveValues(data = NULL, model = NULL, empirical_table = NULL)
  
  # Leer Excel y actualizar hoja
  observeEvent(input$file, {
    req(input$file)
    sheets <- excel_sheets(path = input$file$datapath)
    updateSelectInput(session, "sheet", choices = sheets)
  })
  
  # Cargar datos, recodificar como en el pipeline, construir tabla empírica y clusters
  observeEvent(input$sheet, {
    req(input$file, input$sheet)
    df <- read_excel(input$file$datapath, sheet = input$sheet)
    df <- as.data.frame(df)
    
    # Recodificación alineada con el pipeline (asegura etiquetas exactas)
    df <- df %>%
      mutate(
        # Factors con etiquetas
        Grado = factor(Grado, levels = c(1,2), labels = c("Bajo","Alto")),
        grado_histologi = factor(grado_histologi, levels = c(1,2), labels = c("Bajo","Alto")),
        libre_enferm = factor(libre_enferm, levels = c(0,1,2), labels = c("No","Si","Desconocido")),
        Tributaria_a_Radioterapia = factor(Tributaria_a_Radioterapia, levels = c(0,1), labels = c("No","Si")),
        afectacion_linf = factor(afectacion_linf, levels = c(0,1), labels = c("No","Si")),
        AP_centinela_pelvico = factor(AP_centinela_pelvico, levels = c(0,1,2,3,4),
                                      labels = c("pN0","pN0(i+)","pN1(mi)","pN1","pNx")),
        AP_ganPelv = factor(AP_ganPelv, levels = c(0,1,2,3),
                            labels = c("Negativo","Cels_aisladas","Micrometastasis","Macrometastasis")),
        grupo_de_riesgo_definitivo = factor(grupo_de_riesgo_definitivo, levels = c(1,2,3,4,5),
                                            labels = c("Bajo","Intermedio","Intermedio_alto","Alto","Avanzados"))
      )
    
    # PCA + clustering sobre numéricas (simple y reproducible)
    numeric_vars <- df %>% dplyr::select(where(is.numeric)) %>% tidyr::drop_na()
    # Si no hay suficientes columnas numéricas, evitar fallo
    if (ncol(numeric_vars) >= 2 && nrow(numeric_vars) >= 3) {
      numeric_scaled <- scale(numeric_vars)
      pca_res <- prcomp(numeric_scaled, center = TRUE, scale. = TRUE)
      set.seed(123)
      km_res <- kmeans(pca_res$x[,1:2], centers = 3, nstart = 25)
      clusters_full <- km_res$cluster
    } else {
      # Fallback: todos al mismo cluster si los datos no permiten PCA/Kmeans
      clusters_full <- rep(1, nrow(df))
    }
    
    df <- df %>%
      mutate(cluster = factor(clusters_full),
             cluster_label = case_when(
               cluster == 1 ~ "Buen pronóstico",
               cluster == 2 ~ "Alto riesgo",
               cluster == 3 ~ "Mixto",
               TRUE ~ "Buen pronóstico"
             ))
    
    rv$data <- df
    
    # Poblar dinámicamente los selectInput con niveles reales
    updateSelectInput(session, "Grado_in", choices = levels(rv$data$Grado))
    updateSelectInput(session, "libre_enferm_in", choices = levels(rv$data$libre_enferm))
    updateSelectInput(session, "beta_cateninap_in", choices = levels(as.factor(rv$data$beta_cateninap)))
    updateSelectInput(session, "mlh1_in", choices = levels(as.factor(rv$data$mlh1)))
    updateSelectInput(session, "grupo_riesgo_in", choices = levels(rv$data$grupo_de_riesgo_definitivo))
    updateSelectInput(session, "Tributaria_in", choices = levels(rv$data$Tributaria_a_Radioterapia))
    updateSelectInput(session, "afectacion_linf_in", choices = levels(rv$data$afectacion_linf))
    updateSelectInput(session, "grado_histologi_in", choices = levels(rv$data$grado_histologi))
    updateSelectInput(session, "AP_centinela_in", choices = levels(rv$data$AP_centinela_pelvico))
    updateSelectInput(session, "AP_ganPelv_in", choices = levels(rv$data$AP_ganPelv))
    updateSelectInput(session, "tipo_histologico_in", choices = levels(as.factor(rv$data$tipo_histologico)))
    
    # Construir tabla empírica desde los mismos factores usados en UI
    vars_sig <- c("Grado","libre_enferm","beta_cateninap","mlh1",
                  "grupo_de_riesgo_definitivo","Tributaria_a_Radioterapia",
                  "afectacion_linf","grado_histologi","AP_centinela_pelvico",
                  "AP_ganPelv","tipo_histologico")
    
    empirical_results <- list()
    for (var in vars_sig) {
      if (!var %in% names(df)) next
      tmp <- df %>%
        group_by(.data[[var]], cluster_label) %>%
        summarise(n = n(), .groups = "drop") %>%
        group_by(.data[[var]]) %>%
        mutate(prob = round(100 * n / sum(n), 1),
               variable = var,
               value = as.character(.data[[var]])) %>%
        dplyr::select(variable, value, cluster_label, prob)
      empirical_results[[var]] <- tmp
    }
    empirical_summary <- bind_rows(empirical_results)
    rv$empirical_table <- empirical_summary %>%
      tidyr::pivot_wider(names_from = cluster_label, values_from = prob)
  })
  
  output$table <- renderDT({
    if(is.null(rv$data)){
      datatable(data.frame(Missatge="Puja un arxiu per a veure dades"))
    } else {
      datatable(rv$data)
    }
  })
  
  # Entrenar modelo Cox (tal cual)
  observeEvent(input$train, {
    req(rv$data)
    df <- rv$data
    df$Tiempo <- as.numeric(ifelse(!is.na(df$Fecha_recidiva),
                                   df$Fecha_recidiva - df$Fecha_cirugia,
                                   df$Fecha_ultimo_seguimiento - df$Fecha_cirugia))
    df$Evento <- ifelse(!is.na(df$Fecha_recidiva),1,0)
    factor_vars <- c('Tipo_hist_biopsia','Grado_biopsia','Infiltracion_miometrial','Metastasis_distancia','Riesgo_preIQ','PR','ER','Betacatenina')
    for(v in factor_vars) if(v %in% names(df)) df[[v]] <- as.factor(df[[v]])
    preds <- c('Tipo_hist_biopsia','Grado_biopsia','Infiltracion_miometrial','CA125','Metastasis_distancia','Riesgo_preIQ','Edad','PR','ER','Betacatenina')
    fmla <- as.formula(paste("Surv(Tiempo,Evento) ~", paste(preds, collapse = "+")))
    cox_model <- coxph(fmla, data=df)
    rv$model <- cox_model
    showNotification("Model entrenat", type="message")
  })
  
  output$model_summary <- renderPrint({
    req(rv$model)
    summary(rv$model)
  })
  
  output$km_plot <- renderPlot({
    req(rv$model)
    ggsurvplot(survfit(rv$model), conf.int=TRUE, risk.table=TRUE, ggtheme=theme_minimal())
  })
  
  # Probabilidades de cluster para un paciente
  score_new_patient <- function(new_patient, empirical_table, cluster_cols) {
    stopifnot(nrow(new_patient) == 1)
    # Filtrar por variables presentes
    scores <- empirical_table %>%
      filter(variable %in% names(new_patient)) %>%
      rowwise() %>%
      mutate(match = as.character(new_patient[[variable]]) == value) %>%
      ungroup() %>%
      filter(match)
    if (nrow(scores) == 0) {
      # Devuelve NA si no hay coincidencias
      return(tibble(!!cluster_cols[1] := NA_real_,
                    !!cluster_cols[2] := NA_real_,
                    !!cluster_cols[3] := NA_real_))
    }
    result <- scores %>%
      summarise(across(all_of(cluster_cols), ~ mean(.x, na.rm = TRUE)))
    result <- result %>%
      mutate(total = rowSums(across(all_of(cluster_cols))),
             across(all_of(cluster_cols), ~ round(100 * .x / total, 1)))
    result$total <- NULL
    result
  }
  
  # Tiempo hasta recidiva usando fecha de recidiva para eventos
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
                                   data_rec,
                                   surgery_date = Sys.Date(),
                                   cluster_col = "cluster_label",
                                   stat = c("median","mean")) {
    stat <- match.arg(stat)
    probs_tbl <- score_new_patient(new_patient_df, empirical_table, cluster_cols)
    if (nrow(probs_tbl) == 0 || anyNA(probs_tbl[cluster_cols])) {
      return(list(pred_days = NA_real_, pred_date = as.Date(NA), probs = probs_tbl, cluster_stats = NULL))
    }
    probs_named <- c(
      `Alto riesgo` = probs_tbl$`Alto riesgo`,
      `Buen pronóstico` = probs_tbl$`Buen pronóstico`,
      `Mixto` = probs_tbl$Mixto
    )
    df_tte <- compute_relapse_time(data_rec)
    cl_stats <- summarize_relapse_by_cluster(df_tte, cluster_col = cluster_col, stat = stat)
    pred_days <- combine_probs_to_days(probs_named, cl_stats, probs_in_percent = TRUE)
    pred_date <- if (!is.na(pred_days)) as.Date(surgery_date) + round(pred_days) else as.Date(NA)
    list(pred_days = pred_days, pred_date = pred_date,
         probs = probs_named, cluster_stats = cl_stats)
  }
  
  # Predicción paciente
  observeEvent(input$predict_btn, {
    req(rv$empirical_table, rv$data)
    new_patient <- data.frame(
      Grado = input$Grado_in,
      libre_enferm = input$libre_enferm_in,
      beta_cateninap = input$beta_cateninap_in,
      mlh1 = input$mlh1_in,
      grupo_de_riesgo_definitivo = input$grupo_riesgo_in,
      Tributaria_a_Radioterapia = input$Tributaria_in,
      afectacion_linf = input$afectacion_linf_in,
      grado_histologi = input$grado_histologi_in,
      AP_centinela_pelvico = input$AP_centinela_in,
      AP_ganPelv = input$AP_ganPelv_in,
      tipo_histologico = input$tipo_histologico_in,
      stringsAsFactors = FALSE
    )
    
    cluster_columns <- c("Alto riesgo","Buen pronóstico","Mixto")
    
    pred <- predict_relapse_days(
      new_patient_df = new_patient,
      empirical_table = rv$empirical_table,
      cluster_cols = cluster_columns,
      data_rec = rv$data,
      surgery_date = input$surgery_date,
      cluster_col = "cluster_label",
      stat = "median"
    )
    
    output$prediction_text <- renderPrint({
      cat("Probabilidad de pertenencia a clusters:\n",
          sprintf("Alto riesgo: %s%%\nBuen pronóstico: %s%%\nMixto: %s%%",
                  ifelse(is.na(pred$probs["Alto riesgo"]), "NA", pred$probs["Alto riesgo"]),
                  ifelse(is.na(pred$probs["Buen pronóstico"]), "NA", pred$probs["Buen pronóstico"]),
                  ifelse(is.na(pred$probs["Mixto"]), "NA", pred$probs["Mixto"])))
      cat("\n\nPredicción temporal:\n")
      cat(sprintf("Días estimados hasta recidiva: %s\n",
                  ifelse(is.na(pred$pred_days), "NA", round(pred$pred_days))))
      cat(sprintf("Fecha estimada de recidiva: %s\n",
                  ifelse(is.na(pred$pred_date), "NA", format(pred$pred_date, "%d/%m/%Y"))))
    })
  })
}

# Run app
shinyApp(ui, server)

