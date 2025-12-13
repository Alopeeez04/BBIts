library(shiny)
library(shinythemes)
library(readxl)
library(dplyr)
library(ggplot2)
library(DT)
library(survival)
library(shinyWidgets)   # <-- THIS LINE
# =========================
# UI
# =========================
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Predicción de Recidiva en Cáncer de Endometrio"),
  
  tags$head(
    tags$style(HTML("
      /* ----------Sidebar layout: fixed header + scrollable body + fixed footer ---------- */

      /* Remove default padding that can cause awkward scroll behavior */
      .well { padding: 12px; }

      /* The sidebar panel becomes a flex column: header (mode selector), body (scroll), footer (visualizacion) */
      .sidebar-flex {
        height: calc(100vh - 140px);  /* tweak if your titlePanel height differs */
        display: flex;
        flex-direction: column;
      }

      /* Scroll only the long form section */
      .sidebar-scroll {
        flex: 1 1 auto;
        overflow-y: auto;
        overflow-x: hidden;
        padding-right: 6px; /* space for scrollbar */
      }

      /* Keep bottom section always visible */
      .sidebar-footer {
        flex: 0 0 auto;
        border-top: 1px solid rgba(0,0,0,0.08);
        padding-top: 10px;
        margin-top: 10px;
      }

      /* Slightly tighter spacing between inputs */
      .sidebar-scroll .form-group { margin-bottom: 10px; }

      /* ---------- Prettify shinyWidgets radioGroupButtons ---------- */
      /* Make buttons look like modern rounded pills */
      .radioGroupButtons .btn {
        border-radius: 999px !important;
        padding: 8px 12px !important;
        font-weight: 600 !important;
      }

      /* Reduce overall button group height a touch */
      .radioGroupButtons .btn-group { width: 100%; }

      /* Improve active button contrast (Flatly-friendly) */
      .radioGroupButtons .btn.active {
        box-shadow: 0 2px 10px rgba(0,0,0,0.12) !important;
      }

      /* Make the action button (Crear registro) stand out and full-width */
      #crear_manual {
        width: 100%;
        border-radius: 10px;
        font-weight: 700;
      }

      /* Make train button full-width too (optional) */
      #train {
        width: 100%;
        border-radius: 10px;
        font-weight: 700;
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(
        class = "sidebar-flex",
        
        # --- Header: mode selector always visible ---
        radioGroupButtons(
          inputId = "modo_datos",
          label = "Origen de los datos",
          choices = c(
            "Subir Excel" = "excel",
            "Introducir manualmente" = "manual"
          ),
          selected = "excel",
          status = "primary",
          size = "sm",
          justified = TRUE,
          checkIcon = list(yes = icon("check"))
        ),
        
        hr(),
        
        # --- Body: scrollable content (so footer stays visible) ---
        div(
          class = "sidebar-scroll",
          
          # Inputs para Excel
          conditionalPanel(
            condition = "input.modo_datos == 'excel'",
            fileInput("file", "Sube tu Excel (.xlsx)", accept = ".xlsx"),
            selectInput("sheet", "Hoja a usar", choices = NULL),
            actionButton("train", "Entrenar modelo")
          ),
          
          # Inputs manuales
          conditionalPanel(
            h4("Introduce un caso clínico manualmente"),
            condition = "input.modo_datos == 'manual'",
            numericInput("edad_m", "Edad", value = 60, min = 18),
            numericInput("imc_m", "IMC", value = 25, min = 10, max = 60),
            selectInput("tipo_histologico_m", "Tipo histológico",
                        choices = c("Endometrioide","Seroso","Claro","Mixto","Otro")),
            selectInput("histo_defin_m", "Histología definitiva",
                        choices = c("Endometrioide","Seroso","Claro","Mixto","Otro")),
            selectInput("Grado_m", "Grado (biopsia)", choices = c("G1","G2","G3")),
            selectInput("grado_histologi_m", "Grado histológico final", choices = c("G1","G2","G3")),
            numericInput("tamano_tumoral_m", "Tamaño tumoral (mm)", value = 30, min = 1),
            selectInput("ecotv_infiltsub_m", "Infiltración subserosa", choices = c("<50%","≥50%")),
            selectInput("metasta_distan_m", "Metástasis a distancia", choices = c(0,1)),
            selectInput("afectacion_linf_m", "Afectación linfática", choices = c(0,1)),
            selectInput("AP_centinela_pelvico_m", "AP centinela pélvico", choices = c(0,1)),
            selectInput("AP_ganPelv_m", "AP ganglio pélvico", choices = c(0,1)),
            selectInput("p53_molecular_m", "p53 molecular", choices = c("Positivo","Negativo")),
            selectInput("p53_ihq_m", "p53 IHQ", choices = c("Positivo","Negativo")),
            selectInput("mut_pole_m", "Mutación POLE", choices = c("Positivo","Negativo")),
            selectInput("beta_cateninap_m", "Beta catenina", choices = c("Positivo","Negativo")),
            selectInput("msh2_m", "MSH2", choices = c("Positivo","Negativo")),
            selectInput("msh6_m", "MSH6", choices = c("Positivo","Negativo")),
            selectInput("pms2_m", "PMS2", choices = c("Positivo","Negativo")),
            selectInput("mlh1_m", "MLH1", choices = c("Positivo","Negativo")),
            selectInput("grupo_de_riesgo_definitivo_m", "Grupo de riesgo definitivo",
                        choices = c("Bajo","Intermedio","Alto")),
            selectInput("asa_m", "ASA", choices = c("I","II","III","IV")),
            actionButton("crear_manual", "Crear registro")
          )
        ),
        
        # --- Footer: always visible (Visualización) ---
        div(
          class = "sidebar-footer",
          selectInput(
            "grafico_sel",
            "Visualización",
            choices = c(
              "Curva de supervivencia (KM)" = "km",
              "Distribución por grupos de edad" = "edad"
            )
          )
        )
      )
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "input.grafico_sel == 'km'",
        plotOutput("km_plot", height = "450px")
      ),
      conditionalPanel(
        condition = "input.grafico_sel == 'edad'",
        plotOutput("plot_edad", height = "450px")
      ),
      
      hr(),
      
      tabsetPanel(
        tabPanel("Preview datos", DTOutput("table")),
        tabPanel("Predicción", verbatimTextOutput("prediction_text"))
      )
    )
  )
)

# =========================
# SERVER
# =========================
server <- function(input, output, session) {
  
  rv <- reactiveValues(data = NULL, model = NULL)
  
  # -----------------------------
  # Excel: leer archivo
  # -----------------------------
  observeEvent(input$file, {
    req(input$file)
    sheets <- excel_sheets(input$file$datapath)
    updateSelectInput(session, "sheet", choices = sheets)
  })
  
  observeEvent(input$sheet, {
    req(input$file, input$sheet)
    rv$data <- as.data.frame(
      read_excel(input$file$datapath, sheet = input$sheet)
    )
  })
  
  # -----------------------------
  # Manual: crear dataset de una fila
  # -----------------------------
  observeEvent(input$crear_manual, {
    req(input$modo_datos == "manual")
    
    new_row <- data.frame(
      edad = input$edad_m,
      imc = input$imc_m,
      tipo_histologico = input$tipo_histologico_m,
      histo_defin = input$histo_defin_m,
      Grado = input$Grado_m,
      grado_histologi = input$grado_histologi_m,
      tamano_tumoral = input$tamano_tumoral_m,
      ecotv_infiltsub = input$ecotv_infiltsub_m,
      metasta_distan = input$metasta_distan_m,
      afectacion_linf = input$afectacion_linf_m,
      AP_centinela_pelvico = input$AP_centinela_pelvico_m,
      AP_ganPelv = input$AP_ganPelv_m,
      p53_molecular = input$p53_molecular_m,
      p53_ihq = input$p53_ihq_m,
      mut_pole = input$mut_pole_m,
      beta_cateninap = input$beta_cateninap_m,
      msh2 = input$msh2_m,
      msh6 = input$msh6_m,
      pms2 = input$pms2_m,
      mlh1 = input$mlh1_m,
      grupo_de_riesgo_definitivo = input$grupo_de_riesgo_definitivo_m,
      asa = input$asa_m,
      stringsAsFactors = FALSE
    )
    
    rv$data <- dplyr::bind_rows(rv$data, new_row)
    
    showNotification("Registro manual creado", type = "message")
    
  })
  
  # -----------------------------
  # Preview de datos
  # -----------------------------
  output$table <- renderDT({
    req(rv$data)
    datatable(rv$data)
  })

  # -----------------------------
  # Gráfico KM
  # -----------------------------
  output$km_plot <- renderPlot({
    req(rv$model)
    ggsurvplot(survfit(rv$model),
               conf.int = TRUE,
               risk.table = TRUE,
               ggtheme = theme_minimal())
  })
  
  # -----------------------------
  # Gráfico Edad
  # -----------------------------
  output$plot_edad <- renderPlot({
    req(rv$data)
    df <- rv$data
    if(!"edad" %in% names(df)) return(NULL)
    
    df_plot <- df %>%
      mutate(grupo_edad = cut(edad,
                              breaks = c(-Inf,29,49,Inf),
                              labels = c("0-29","30-49","+50"))) %>%
      group_by(grupo_edad) %>%
      summarise(Frecuencia = n())
    
    ggplot(df_plot, aes(x = grupo_edad, y = Frecuencia)) +
      geom_bar(stat = "identity", fill = "steelblue", color = "black") +
      labs(title = "Distribución por grupos de edad",
           x = "Grupo de edad", y = "Frecuencia") +
      theme_minimal()
  })
  
  # -----------------------------
  # Predicción
  # -----------------------------
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
    
    newcase[] <- lapply(newcase, as.factor)
    
    lp <- predict(rv$model, newdata = newcase, type = "lp")
    base <- basehaz(rv$model, centered = FALSE)
    
    S <- function(t) exp(-approx(base$time, base$hazard, xout = t)$y)
    
    p1 <- 1 - S(365)^exp(lp)
    p3 <- 1 - S(365*3)^exp(lp)
    p5 <- 1 - S(365*5)^exp(lp)
    
    output$prediction_text <- renderPrint({
      cat(sprintf(
        "Probabilidad de recidiva:\n1 año: %.1f%%\n3 años: %.1f%%\n5 años: %.1f%%",
        p1*100, p3*100, p5*100))
    })
  })
  
}

# =========================
# RUN APP
# =========================
shinyApp(ui, server)

