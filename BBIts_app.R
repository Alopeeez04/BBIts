# Instala paquetes si no los tienes
# install.packages(c("shiny","readxl","survival","survminer","glmnet","shinythemes","DT"))

library(shiny)
library(readxl)
library(survival)
library(survminer)
library(glmnet)
library(shinythemes)
library(DT)

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
      selectInput('Tipo_hist_in','Tipus histològic', choices = c('Endometrioide','Seroso','Claro')),
      selectInput('Grado_in','Grau (biopsia)', choices = c('G1','G2','G3')),
      selectInput('Infiltracion_in','Infiltració miometrial', choices = c('<50%','>=50%')),
      numericInput('CA125_in','CA125', value = 20, min = 0),
      selectInput('Metastasis_in','Metàstasi a distància', choices = c(0,1)),
      selectInput('Riesgo_preIQ_in','Risc preIQ', choices = c('Bajo','Intermedio','Alto')),
      numericInput('Edad_in','Edat', value = 65, min = 18),
      selectInput('PR_in','Receptor Progesterona', choices = c('Positivo','Negativo')),
      selectInput('ER_in','Receptor Estrogen', choices = c('Positivo','Negativo')),
      selectInput('Beta_in','Betacatenina', choices = c('Positivo','Negativo')),
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
  rv <- reactiveValues(data = NULL, model = NULL)
  
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
  
  # Predicción paciente
  observeEvent(input$predict_btn, {
    req(rv$model)
    newcase <- data.frame(
      Tipo_hist_biopsia = input$Tipo_hist_in,
      Grado_biopsia = input$Grado_in,
      Infiltracion_miometrial = input$Infiltracion_in,
      CA125 = input$CA125_in,
      Metastasis_distancia = input$Metastasis_in,
      Riesgo_preIQ = input$Riesgo_preIQ_in,
      Edad = input$Edad_in,
      PR = input$PR_in,
      ER = input$ER_in,
      Betacatenina = input$Beta_in
    )
    
    for(v in names(newcase)) if(is.character(newcase[[v]])) newcase[[v]] <- as.factor(newcase[[v]])
    
    # Predecir riesgo relativo
    lp <- predict(rv$model, newdata = newcase, type="lp")
    # Baseline survival a 1,3,5 años (aprox en días)
    base <- basehaz(rv$model, centered=FALSE)
    S1 <- exp(-approx(base$time, base$hazard, xout=365)$y)
    S3 <- exp(-approx(base$time, base$hazard, xout=365*3)$y)
    S5 <- exp(-approx(base$time, base$hazard, xout=365*5)$y)
    
    # Ajustar con el linear predictor
    prob1 <- 1 - S1^exp(lp)
    prob3 <- 1 - S3^exp(lp)
    prob5 <- 1 - S5^exp(lp)
    
    output$prediction_text <- renderPrint({
      cat(sprintf("Probabilidad de recidiva:\n1 año: %.1f%%\n3 años: %.1f%%\n5 años: %.1f%%", prob1*100, prob3*100, prob5*100))
    })
  })
}

# Run app
shinyApp(ui, server)
