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
    rv$data <- as.data.frame(df)
    
    # Aquí deberías construir empirical_summary_wide a partir de rv$data
    # Por simplicidad asumimos que ya lo tienes preparado en tu entorno
    rv$empirical_table <- empirical_summary_wide
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
    scores <- empirical_table %>%
      filter(variable %in% names(new_patient)) %>%
      rowwise() %>%
      mutate(match = as.character(new_patient[[variable]]) == value) %>%
      ungroup() %>%
      filter(match)
    
    result <- scores %>%
      summarise(across(all_of(cluster_cols), ~ mean(.x, na.rm = TRUE)))
    
    result <- result %>%
      mutate(total = rowSums(across(all_of(cluster_cols))),
             across(all_of(cluster_cols), ~ round(100 * .x / total, 1))) %>%
      select(-total)
    
    return(result)
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
    
    result <- score_new_patient(new_patient, rv$empirical_table, cluster_columns)
    
    output$prediction_text <- renderPrint({
      cat("Probabilidad de pertenencia a clusters:\n",
          sprintf("Alto riesgo: %.1f%%\nBuen pronóstico: %.1f%%\nMixto: %.1f%%",
                  result$`Alto riesgo`, result$`Buen pronóstico`, result$Mixto))
    })
  })
}

# Run app
shinyApp(ui, server)
