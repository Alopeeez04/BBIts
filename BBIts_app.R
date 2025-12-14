# Instala paquetes si no los tienes
# install.packages(c("shiny","readxl","survival","survminer","glmnet","shinythemes","DT"))

library(shiny)
library(readxl)
library(survival)
library(survminer)
library(glmnet)
library(shinythemes)
library(DT)
library(dplyr)

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
      selectInput('Grado_in','Grado', choices = c('Bajo','Alto')),
      selectInput('libre_enferm_in','Libre de enfermedad', choices = c('Si','No','Desconocido')),
      selectInput('beta_cateninap_in','Beta catenina', choices = c('0','1','2')),
      selectInput('mlh1_in','MLH1', choices = c('0','2')),
      selectInput('grupo_riesgo_in','Grupo de riesgo definitivo', 
                  choices = c('Bajo','Intermedio','Intermedio_alto','Alto','Avanzados')),
      selectInput('Tributaria_in','Tributaria a Radioterapia', choices = c('Si','No')),
      selectInput('afectacion_linf_in','Afectación linfática', choices = c('Si','No')),
      selectInput('grado_histologi_in','Grado histológico', choices = c('Bajo','Alto')),
      selectInput('AP_centinela_in','AP centinela pélvico', 
                  choices = c('pN0','pN0(i+)','pN1(mi)','pN1','pNx')),
      selectInput('AP_ganPelv_in','AP ganglios pélvicos', 
                  choices = c('Negativo','Cels_aisladas','Macrometastasis')),
      selectInput('tipo_histologico_in','Tipo histológico', 
                  choices = c('1','2','3','4','5','7','8','9','10','12','88')),
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
  
  # Cargar datos
  observeEvent(input$sheet, {
    req(input$file, input$sheet)
    df <- read_excel(input$file$datapath, sheet = input$sheet)
    df <- as.data.frame(df)
    
    # --- replicate your pipeline steps here ---
    # Select numeric variables for PCA
    numeric_vars <- df %>%
      dplyr::select(where(is.numeric)) %>%
      na.omit()
    
    numeric_scaled <- scale(numeric_vars)
    
    # PCA
    pca_res <- prcomp(numeric_scaled, center = TRUE, scale. = TRUE)
    
    # K-means clustering on first 2 PCs
    set.seed(123)
    pca_scores <- pca_res$x[,1:2]
    km_res <- kmeans(pca_scores, centers = 3, nstart = 25)
    
    clusters_full <- km_res$cluster
    
    # Add cluster and cluster_label to df
    df <- df %>%
      mutate(cluster = factor(clusters_full),
             cluster_label = case_when(
               cluster == 1 ~ "Buen pronóstico",
               cluster == 2 ~ "Alto riesgo",
               cluster == 3 ~ "Mixto"
             ))
    
    rv$data <- df
    
    # Build empirical_summary_wide from df (as in your pipeline)
    vars_sig <- c("Grado","libre_enferm","beta_cateninap","mlh1",
                  "grupo_de_riesgo_definitivo","Tributaria_a_Radioterapia",
                  "afectacion_linf","grado_histologi","AP_centinela_pelvico",
                  "AP_ganPelv","tipo_histologico")
    
    empirical_results <- list()
    for (var in vars_sig) {
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
  
  # Entrenar modelo Cox
  observeEvent(input$train, {
    req(rv$data)
    df <- rv$data
    # Crear tiempo y evento
    df$Tiempo <- as.numeric(ifelse(!is.na(df$Fecha_recidiva),
                                   df$Fecha_recidiva - df$Fecha_cirugia,
                                   df$Fecha_ultimo_seguimiento - df$Fecha_cirugia))
    df$Evento <- ifelse(!is.na(df$Fecha_recidiva),1,0)
    
    # Convertir variables a factor/numeric
    factor_vars <- c('Tipo_hist_biopsia','Grado_biopsia','Infiltracion_miometrial','Metastasis_distancia','Riesgo_preIQ','PR','ER','Betacatenina')
    for(v in factor_vars) if(v %in% names(df)) df[[v]] <- as.factor(df[[v]])
    
    # Fórmula
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
  
  # Función para calcular probabilidades de cluster
  score_new_patient <- function(new_patient, empirical_table, cluster_cols) {
    
    # Ensure input is one-row dataframe
    stopifnot(nrow(new_patient) == 1)
    
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
      )
    
    # Drop the helper column safely
    result$total <- NULL
    
    return(result)
  }
  
  # === Add these functions inside server or globally ===
  compute_relapse_time <- function(data_rec,
                                   surgery_col = "fecha_qx",
                                   relapse_flag_col = "dx_recidiva",
                                   followup_col = "Ultima_fecha",
                                   date_format_surgery = "%d/%m/%Y") {
    data_rec %>%
      mutate(
        surgery_date  = as.Date(.data[[surgery_col]], format = date_format_surgery),
        followup_date = as.Date(gsub(" UTC","",.data[[followup_col]])),
        relapse_event = !is.na(.data[[relapse_flag_col]]) & .data[[relapse_flag_col]] == 1,
        tte_days = as.numeric(followup_date - surgery_date)
      )
  }
  
  summarize_relapse_by_cluster <- function(df, cluster_col = "cluster_label",
                                           stat = c("median","mean")) {
    stat <- match.arg(stat)
    df %>%
      filter(relapse_event, !is.na(.data[[cluster_col]]), !is.na(tte_days), tte_days >= 0) %>%
      group_by(.data[[cluster_col]]) %>%
      summarise(
        n_events = n(),
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
      return(list(pred_days = NA_real_, pred_date = as.Date(NA)))
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
      surgery_date = input$surgery_date,   # <-- use chosen date
      cluster_col = "cluster_label",
      stat = "median"
    )
    
    
    output$prediction_text <- renderPrint({
      cat("Probabilidad de pertenencia a clusters:\n",
          sprintf("Alto riesgo: %.1f%%\nBuen pronóstico: %.1f%%\nMixto: %.1f%%",
                  pred$probs["Alto riesgo"], pred$probs["Buen pronóstico"], pred$probs["Mixto"]))
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

