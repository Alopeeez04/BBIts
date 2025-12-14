library(shiny)
library(shinythemes)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(shinyWidgets)
library(plotly)

source("R/prediction_model.R")
source("R/predict_days.R")

load_value_mapping <- function() {
  mapping_paths <- c(
    "field_value_mapping.csv",
    file.path("www", "field_value_mapping.csv"),
    file.path(getwd(), "field_value_mapping.csv")
  )
  
  for (path in mapping_paths) {
    if (file.exists(path)) {
      tryCatch({
        mapping_df <- read_csv(path, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
        if ("DescripciÃ³n" %in% names(mapping_df)) {
          mapping_df <- mapping_df %>% rename(Descripción = "DescripciÃ³n")
        }
        mapping_df$valor <- as.character(mapping_df$valor)
        mapping_df <- mapping_df %>%
          filter(!is.na(valor) & valor != "")
        cat("Loaded value mapping from:", path, "\n")
        return(mapping_df)
      }, error = function(e) {
        cat("Error loading mapping from", path, ":", e$message, "\n")
      })
    }
  }
  
  warning("field_value_mapping.csv not found. Distribution charts will use raw values.")
  return(data.frame(variable = character(), valor = character(), texto = character(), stringsAsFactors = FALSE))
}

mapping_df <- load_value_mapping()

desc_lookup <- mapping_df %>%
  distinct(variable, Descripción) %>%
  filter(!is.na(Descripción) & Descripción != "" & !is.null(Descripción))

get_field_description <- function(field, desc_lookup) {
  matches <- desc_lookup %>% filter(variable == field)
  if (nrow(matches) > 0) {
    return(matches$Descripción[1])
  }
  return(field)
}

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  actionButton(
    "toggle_sidebar_floating",
    label = NULL,
    icon = icon("bars"),
    class = "btn-default sidebar-toggle-floating",
    title = "Mostrar panel lateral"
  ),
  
  div(
    class = "app-wrapper",
    div(
      class = "sidebar-panel",
      wellPanel(
        actionButton(
          "toggle_sidebar",
          label = NULL,
          icon = icon("times"),
          class = "btn-sm pull-right",
          title = "Ocultar panel lateral"
        ),
        h4("Controles"),
        
        radioGroupButtons(
          inputId = "modo_datos",
          label = "Origen de los datos",
          choices = c("Subir CSV" = "csv", "Introducir manualmente" = "manual"),
          selected = "csv",
          status = "primary",
          size = "sm",
          justified = TRUE
        ),
        
        hr(),
        
        conditionalPanel(
          condition = "input.modo_datos == 'csv'",
          fileInput("file", "Sube tu CSV (.csv)", accept = ".csv")
        ),
        
        conditionalPanel(
          condition = "input.modo_datos == 'manual'",
          h4("Introduce un caso clínico manualmente"),
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
          fluidRow(
            column(6,
              actionButton("crear_manual", "Crear registro", class = "btn-primary", style = "width: 100%;")
            ),
            column(6,
              actionButton("predict_manual", "Predict", class = "btn-success", style = "width: 100%; font-weight: bold;")
            )
          )
        ),
        
        conditionalPanel(
          condition = "output.data_loaded",
          hr(),
          div(
            class = "alert alert-success",
            HTML("<strong>✓ Datos cargados</strong>"),
            br(),
            textOutput("data_info", inline = FALSE)
          )
        ),
        
        conditionalPanel(
          condition = "output.data_loaded",
          div(
            class = "variable-selector-container",
            div(
              class = "variable-selector-header",
              h5(style = "margin: 0; font-weight: bold;", 
                 icon("list"), " Variables para gráficos")
            ),
            div(
              class = "variable-selector-scrollable",
              prettyCheckboxGroup(
                inputId = "dist_fields",
                label = NULL,
                choices = NULL,
                selected = NULL,
                status = "primary",
                icon = icon("check"),
                animation = "pulse",
                bigger = FALSE
              )
            )
          )
        ),
        
      )
    ),
    
    div(
      class = "main-content",
      div(
        class = "navbar-wrapper",
        navbarPage(
          title = "Predicción de Recidiva en Cáncer de Endometrio",
          id = "main_navbar",
          selected = "preview",
          
          tabPanel(
            "Preview datos",
            value = "preview",
            div(
              class = "tab-content-wrapper",
              conditionalPanel(
                condition = "!output.data_loaded",
                div(
                  class = "alert alert-info text-center",
                  style = "margin-top: 50px;",
                  h4("No hay datos cargados"),
                  p("Sube un archivo CSV o introduce datos manualmente desde el panel lateral")
                )
              ),
              conditionalPanel(
                condition = "output.data_loaded",
                div(
                  class = "table-container",
                  DTOutput("table")
                )
              )
            )
          ),
          
          tabPanel(
            "Distribución por grupos",
            value = "distribucion",
            div(
              class = "tab-content-wrapper",
              conditionalPanel(
                condition = "!output.data_loaded",
                div(
                  class = "alert alert-info text-center",
                  style = "margin-top: 50px;",
                  h4("No hay datos cargados"),
                  p("Carga datos primero para ver las distribuciones")
                )
              ),
              conditionalPanel(
                condition = "output.data_loaded",
                uiOutput("dist_plots")
              )
            )
          ),
          
          tabPanel(
            "Prediction",
            value = "prediction",
            div(
              class = "tab-content-wrapper",
              conditionalPanel(
                condition = "input.modo_datos == 'manual'",
                wellPanel(
                  style = "background-color: #FFFFFF; border: 1px solid var(--color-border); border-radius: 8px; padding: 25px;",
                  h4("Días hasta recidiva (estimación)", style = "color: var(--color-primary-dark); margin-bottom: 20px;"),
                  uiOutput("prediction_days_card"),
                  br(),
                  h5("Probabilidades de cluster (%)", style = "color: var(--color-text-secondary); margin-top: 25px; margin-bottom: 15px;"),
                  tableOutput("prediction_table"),
                  br(),
                  uiOutput("prediction_summary")
                )
              ),
              conditionalPanel(
                condition = "input.modo_datos != 'manual'",
                div(
                  class = "alert alert-info text-center",
                  style = "margin-top: 50px;",
                  h4("Modo manual requerido"),
                  p("Cambia a 'Introducir manualmente' en el panel lateral para ver las predicciones")
                )
              )
            )
          ),
          
          tabPanel(
            "Impacto por Cohortes",
            value = "impacto_cohortes",
            div(
              class = "tab-content-wrapper",
              div(
                class = "cohort-image-grid",
                div(
                  class = "cohort-image-block",
                  h5("Muertos — No recesiva", class = "cohort-image-title"),
                  tags$img(src = "muertos_sin_recesiva.jpeg", class = "cohort-image", alt = "Muertos no recesiva")
                ),
                div(
                  class = "cohort-image-block",
                  h5("Muertos — Con recesiva", class = "cohort-image-title"),
                  tags$img(src = "muertos_con_recesiva.jpeg", class = "cohort-image", alt = "Muertos con recesiva")
                ),
                div(
                  class = "cohort-image-block",
                  h5("Vivos — Sin recesiva", class = "cohort-image-title"),
                  tags$img(src = "vivos_sin_recesiva.jpeg", class = "cohort-image", alt = "Vivos sin recesiva")
                ),
                div(
                  class = "cohort-image-block",
                  h5("Vivos — Con recesiva", class = "cohort-image-title"),
                  tags$img(src = "vivos_con_recesiva.jpeg", class = "cohort-image", alt = "Vivos con recesiva")
                )
              )
            )
          )
        )
      )
    )
  ),
  
  tags$script(HTML("
    $(document).ready(function() {
      Shiny.addCustomMessageHandler('toggleSidebar', function(message) {
        document.body.classList.toggle('sidebar-collapsed');
        setTimeout(function() {
          window.dispatchEvent(new Event('resize'));
          if ($.fn.DataTable) {
            $('.dataTable').each(function() {
              if ($.fn.DataTable.isDataTable(this)) {
                $(this).DataTable().columns.adjust().draw();
              }
            });
          }
        }, 300);
      });
    });
  "))
)

server <- function(input, output, session) {
  
  data <- reactiveVal(NULL)
  
  rv <- reactiveValues(pred = NULL, pred_days = NULL)
  
  original_data <- reactiveVal(NULL)
  
  
  output$data_loaded <- reactive({
    !is.null(data())
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  output$data_info <- renderText({
    req(data())
    df <- data()
    paste0(
      "Datos: ", nrow(df), " filas, ", ncol(df), " columnas"
    )
  })
  
  observeEvent(input$file, {
    req(input$file)
    tryCatch({
      df <- read_csv(input$file$datapath, show_col_types = FALSE)
      df <- as.data.frame(df)
      
      categorical_cols <- c(
        "tipo_histologico", "histo_defin", "Grado", "grado_histologi",
        "ecotv_infiltsub", "metasta_distan", "afectacion_linf",
        "AP_centinela_pelvico", "AP_ganPelv", "p53_molecular", "p53_ihq",
        "mut_pole", "beta_cateninap", "msh2", "msh6", "pms2", "mlh1",
        "grupo_de_riesgo_definitivo", "asa"
      )
      
      for (col in categorical_cols) {
        if (col %in% names(df)) {
          df[[col]] <- as.character(df[[col]])
        }
      }
      
      data(df)
      original_data(df)
      showNotification(
        paste("Archivo cargado:", nrow(df), "filas,", ncol(df), "columnas"),
        type = "message",
        duration = 3
      )
    }, error = function(e) {
      showNotification(
        paste("Error al leer el archivo CSV:", e$message),
        type = "error",
        duration = 5
      )
    })
  })
  
  observe({
    if (file.exists("data/data_hack.xlsx") && is.null(original_data())) {
      tryCatch({
        if (requireNamespace("readxl", quietly = TRUE)) {
          library(readxl)
          df_orig <- read_excel("data/data_hack.xlsx")
          original_data(as.data.frame(df_orig))
        }
      }, error = function(e) {
      })
    }
  })
  
  observeEvent(input$crear_manual, {
    if (is.na(input$edad_m) || input$edad_m < 18) {
      showNotification("Edad debe ser mayor o igual a 18 años", type = "error")
      return()
    }
    if (is.na(input$imc_m) || input$imc_m < 10 || input$imc_m > 60) {
      showNotification("IMC debe estar entre 10 y 60", type = "error")
      return()
    }
    
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
    
    current_data <- data()
    if (is.null(current_data)) {
      data(new_row)
      showNotification("Primer registro creado", type = "message")
    } else {
      data(bind_rows(current_data, new_row))
      showNotification("Registro agregado correctamente", type = "message")
    }
  })
  
  observeEvent(input$toggle_sidebar, {
    session$sendCustomMessage("toggleSidebar", list())
  })
  
  observeEvent(input$toggle_sidebar_floating, {
    session$sendCustomMessage("toggleSidebar", list())
  })
  
  ALLOWED_VARIABLES <- c(
    "Grado", "libre_enferm", "beta_cateninap", "mlh1", 
    "grupo_de_riesgo_definitivo", "Tributaria_a_Radioterapia", 
    "afectacion_linf", "grado_histologi", "AP_centinela_pelvico", 
    "AP_ganPelv", "tipo_histologico_collapsed"
  )
  
  observeEvent(data(), {
    req(data())
    cols <- names(data())
    available_vars <- intersect(ALLOWED_VARIABLES, cols)
    
    if (length(available_vars) > 0) {
      updatePrettyCheckboxGroup(
        session,
        "dist_fields",
        choices = available_vars,
        selected = available_vars[1:min(3, length(available_vars))]
      )
    } else {
      updatePrettyCheckboxGroup(
        session,
        "dist_fields",
        choices = character(0),
        selected = NULL
      )
    }
  })
  
  MAX_PREVIEW_COLS <- 12
  
  preview_data <- reactive({
    req(data())
    df <- data()
    if (ncol(df) > MAX_PREVIEW_COLS) {
      df[, seq_len(MAX_PREVIEW_COLS), drop = FALSE]
    } else {
      df
    }
  })
  
  output$table <- renderDT({
    req(preview_data())
    df <- preview_data()
    
    datatable(
      df,
      rownames = FALSE,
      filter = "top",
      options = list(
        scrollX = TRUE,
        scrollY = "calc(100vh - 250px)",
        scrollCollapse = TRUE,
        paging = TRUE,
        pageLength = 15,
        lengthChange = TRUE,
        lengthMenu = c(10, 15, 25, 50, 100),
        dom = "lfrtip",
        autoWidth = FALSE,
        columnDefs = list(
          list(
            targets = "_all",
            className = "dt-center",
            width = "150px"
          )
        )
      ),
      selection = "none"
    )
  })
  
  CHART_CONFIG <- list(
    bar_color = "black",
    bar_alpha = 0.75,
    fill_palette = "Set3",
    
    title_hjust = 0.5,
    title_face = "bold",
    title_size = 15,
    title_margin = margin(b = 12),
    
    axis_title_size = 12,
    axis_title_face = "plain",
    
    axis_text_x_angle = 45,
    axis_text_x_hjust = 1,
    axis_text_x_size = 10.5,
    axis_text_y_size = 10.5,
    
    plot_margin = margin(10, 15, 10, 15),
    
    ylim_multiplier = 1.1
  )
  
  SPECIAL_CASES <- list(
    "tamano_tumoral" = list(
      axis_text_x_angle = 0,
      axis_text_x_size = 11,
      ylim_multiplier = 1.15
    )
  )
  
  map_values <- function(v, field, mapping_df) {
    v_char <- as.character(v)
    
    field_mappings <- mapping_df %>% filter(variable == field)
    
    if (nrow(field_mappings) == 0) {
      return(v_char)
    }
    
    field_mappings <- field_mappings %>%
      distinct(valor, .keep_all = TRUE)
    
    lookup <- setNames(field_mappings$texto, field_mappings$valor)
    
    mapped <- ifelse(
      v_char %in% names(lookup),
      lookup[v_char],
      v_char
    )
    
    return(mapped)
  }
  
  make_distribution_plot <- function(df, field) {
    if (!field %in% names(df)) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "Variable no encontrada", size = 5) +
          theme_void()
      )
    }
    
    data_clean <- df[!is.na(df[[field]]) & 
                     df[[field]] != "" & 
                     as.character(df[[field]]) != "NA", ]
    
    if (nrow(data_clean) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "Sin datos disponibles", size = 5) +
          theme_void()
      )
    }
    
    v_temp <- as.character(data_clean[[field]])
    v_temp <- v_temp[!is.na(v_temp) & v_temp != "" & v_temp != "NA"]
    
    if (length(v_temp) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "Sin datos disponibles", size = 5) +
          theme_void()
      )
    }
    
    v_mapped <- map_values(v_temp, field, mapping_df)
    
    frecuencia <- table(v_mapped)
    
    df_plot <- data.frame(
      Valor = names(frecuencia),
      Frecuencia = as.numeric(frecuencia),
      Porcentaje = round(as.numeric(prop.table(frecuencia)) * 100, 2),
      stringsAsFactors = FALSE
    )
    
    field_mappings <- mapping_df %>% filter(variable == field)
    if (nrow(field_mappings) > 0) {
      ordered_valors <- unique(field_mappings$valor)
      valor_to_texto <- field_mappings %>%
        select(valor, texto) %>%
        distinct(valor, .keep_all = TRUE)
      ordered_labels <- valor_to_texto$texto[match(ordered_valors, valor_to_texto$valor)]
      ordered_labels <- ordered_labels[ordered_labels %in% df_plot$Valor]
      unmapped <- setdiff(df_plot$Valor, ordered_labels)
      if (length(unmapped) > 0) {
        ordered_labels <- c(ordered_labels, unmapped)
      }
      df_plot$Valor <- factor(df_plot$Valor, levels = ordered_labels)
    } else {
      df_plot$Valor <- factor(df_plot$Valor, levels = df_plot$Valor[order(df_plot$Frecuencia, decreasing = TRUE)])
    }
    
    config <- CHART_CONFIG
    if (field %in% names(SPECIAL_CASES)) {
      special <- SPECIAL_CASES[[field]]
      for (key in names(special)) {
        config[[key]] <- special[[key]]
      }
    }
    
    max_pct <- max(df_plot$Porcentaje) * config$ylim_multiplier
    y_breaks <- pretty(c(0, max_pct), n = 5)
    y_max <- max(y_breaks)
    
    chart_title <- get_field_description(field, desc_lookup)
    
    p <- ggplot(df_plot, aes(x = Valor, y = Porcentaje, fill = Valor,
                              text = paste0(
                                "Categoría: ", Valor, "\n",
                                "Porcentaje: ", Porcentaje, "%\n",
                                "Frecuencia: ", Frecuencia
                              ))) +
      geom_bar(stat = "identity", color = config$bar_color, alpha = config$bar_alpha, width = 0.7) +
      labs(
        title = chart_title,
        x = field,
        y = "Percentatge (%)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          hjust = config$title_hjust, 
          face = config$title_face, 
          size = config$title_size,
          margin = config$title_margin
        ),
        axis.title = element_text(
          size = config$axis_title_size,
          face = config$axis_title_face
        ),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_text(
          angle = config$axis_text_x_angle, 
          hjust = config$axis_text_x_hjust, 
          size = config$axis_text_x_size,
          margin = margin(t = 5)
        ),
        axis.text.y = element_text(
          size = config$axis_text_y_size,
          margin = margin(r = 5)
        ),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        plot.margin = config$plot_margin,
        legend.position = "none"
      ) +
      scale_fill_brewer(palette = config$fill_palette) +
      scale_y_continuous(
        breaks = y_breaks,
        limits = c(0, y_max),
        expand = expansion(mult = c(0, 0.05))
      )
    
    return(p)
  }
  
  output$dist_plots <- renderUI({
    req(data())
    
    fields <- input$dist_fields
    if (is.null(fields) || length(fields) == 0) {
      return(
        div(
          class = "text-muted text-center p-4",
          "Selecciona una o más variables para visualizar"
        )
      )
    }
    
      tagList(
      lapply(fields, function(f) {
        wellPanel(
          class = "plot-card",
          plotlyOutput(
            outputId = paste0("dist_", f),
            height = "400px"
          )
        )
      })
    )
  })
  
  observe({
    req(data())
    fields <- input$dist_fields
    if (is.null(fields) || length(fields) == 0) return()
    
    for (f in fields) {
      local({
        field <- f
        output[[paste0("dist_", field)]] <- renderPlotly({
          p <- make_distribution_plot(data(), field)
          ggplotly(p, tooltip = "text") %>%
            config(displayModeBar = FALSE)
        })
      })
    }
  })
  
  
  get_manual_case <- function() {
    req(input$modo_datos == "manual")
    tryCatch({
      map_input_to_prediction()
    }, error = function(e) {
      NULL
    })
  }
  
  map_input_to_prediction <- function() {
    grado_val <- if (input$Grado_m %in% c("G1", "G2")) "Bajo" else "Alto"
    
    grado_hist_val <- if (input$grado_histologi_m %in% c("G1", "G2")) "Bajo" else "Alto"
    
    afectacion_linf_val <- if (as.character(input$afectacion_linf_m) == "0") "No" else "Si"
    
    ap_centinela_val <- if (as.character(input$AP_centinela_pelvico_m) == "0") "pN0" else "pN1"
    
    ap_ganpelv_val <- if (as.character(input$AP_ganPelv_m) == "0") "Negativo" else "Macrometastasis"
    
    beta_cat_val <- if (input$beta_cateninap_m == "Positivo") "1" else "0"
    
    mlh1_val <- if (input$mlh1_m == "Positivo") "0" else "0"
    
    grupo_riesgo_val <- input$grupo_de_riesgo_definitivo_m
    
    tipo_hist_val <- as.character(input$tipo_histologico_m)
    
    libre_enferm_val <- "No"
    tributaria_rt_val <- "No"
    
    data.frame(
      Grado = grado_val,
      libre_enferm = libre_enferm_val,
      beta_cateninap = beta_cat_val,
      mlh1 = mlh1_val,
      grupo_de_riesgo_definitivo = grupo_riesgo_val,
      Tributaria_a_Radioterapia = tributaria_rt_val,
      afectacion_linf = afectacion_linf_val,
      grado_histologi = grado_hist_val,
      AP_centinela_pelvico = ap_centinela_val,
      AP_ganPelv = ap_ganpelv_val,
      tipo_histologico = tipo_hist_val,
      stringsAsFactors = FALSE
    )
  }
  
  new_patient_reactive <- reactive({
    req(input$modo_datos == "manual")
    tryCatch({
      map_input_to_prediction()
    }, error = function(e) {
      NULL
    })
  })
  
  observeEvent(input$predict_manual, {
    manual_case <- get_manual_case()
    if (is.null(manual_case)) {
      showNotification(
        "Error: No se pudieron obtener los valores de entrada",
        type = "error",
        duration = 5
      )
      return()
    }
    
    if (!file.exists("data/model_objects.RData")) {
      showNotification(
        "Error: Modelo no encontrado. Por favor, ejecute R/build_model.R primero.",
        type = "error",
        duration = 5
      )
      rv$pred <- NULL
      return()
    }
    
    tryCatch({
      pred_result <- predict_from_inputs(manual_case)
      if (is.null(pred_result) || nrow(pred_result) == 0) {
        showNotification(
          "Error: No se pudo calcular la predicción con los valores proporcionados",
          type = "error",
          duration = 5
        )
        rv$pred <- NULL
        rv$pred_days <- NULL
      } else {
        rv$pred <- pred_result
        
        objs <- load_model_objects()
        data_rec <- original_data()
        
        if (!is.null(data_rec)) {
          pred_days_result <- predict_relapse_days(
            new_patient_df = manual_case,
            empirical_table = objs$empirical_summary_wide,
            cluster_cols = objs$cluster_columns,
            data_rec = data_rec,
            surgery_date = Sys.Date(),
            stat = "median"
          )
          rv$pred_days <- pred_days_result
        } else {
          rv$pred_days <- list(pred_days = NA_real_, pred_date = as.Date(NA))
        }
        
        showNotification(
          "Predicción actualizada",
          type = "message",
          duration = 2
        )
      }
    }, error = function(e) {
      showNotification(
        paste("Error al calcular la predicción:", e$message),
        type = "error",
        duration = 5
      )
      rv$pred <- NULL
      rv$pred_days <- NULL
    })
  })
  
  prediction_reactive <- reactive({
    if (!is.null(rv$pred)) {
      return(rv$pred)
    }
    
    req(new_patient_reactive())
    tryCatch({
      predict_from_inputs(new_patient_reactive())
    }, error = function(e) {
      data.frame(
        "Alto riesgo" = NA,
        "Buen pronóstico" = NA,
        "Mixto" = NA
      )
    })
  })
  
  prediction_days_reactive <- reactive({
    if (!is.null(rv$pred_days)) {
      return(rv$pred_days)
    }
    return(NULL)
  })
  
  format_prediction <- function(pred_days_result) {
    if (is.null(pred_days_result) || is.na(pred_days_result$pred_days)) {
      return(list(
        days = NA,
        months = NA,
        date = NA,
        available = FALSE
      ))
    }
    
    days <- round(pred_days_result$pred_days)
    months <- round(days / 30.44, 1)
    pred_date <- pred_days_result$pred_date
    
    return(list(
      days = days,
      months = months,
      date = pred_date,
      available = TRUE
    ))
  }
  
  output$prediction_days_card <- renderUI({
    if (is.null(rv$pred_days)) {
      return(
        div(
          class = "text-muted text-center",
          style = "padding: 20px;",
          p("Haz clic en 'Predict' para calcular la estimación de días hasta recidiva")
        )
      )
    }
    
    formatted <- format_prediction(rv$pred_days)
    
    if (!formatted$available) {
      return(
        div(
          class = "alert alert-warning",
          p("No se pudo calcular la estimación de días. Asegúrate de que el archivo de datos original esté disponible.")
        )
      )
    }
    
    tagList(
      div(
        style = "text-align: center; padding: 20px 0;",
        div(
          style = "font-size: 48px; font-weight: bold; color: var(--color-primary-dark); margin-bottom: 10px;",
          formatted$days
        ),
        div(
          style = "font-size: 18px; color: var(--color-text-secondary); margin-bottom: 15px;",
          "días"
        ),
        if (!is.na(formatted$months) && formatted$months > 0) {
          div(
            style = "font-size: 14px; color: var(--color-text-muted); margin-top: 10px;",
            paste0("Aproximadamente ", formatted$months, " meses")
          )
        },
        if (!is.na(formatted$date)) {
          div(
            style = "font-size: 14px; color: var(--color-text-muted); margin-top: 10px;",
            paste0("Fecha estimada: ", format(formatted$date, "%d/%m/%Y"))
          )
        }
      )
    )
  })
  
  output$prediction_table <- renderTable({
    req(prediction_reactive())
    pred <- prediction_reactive()
    if (nrow(pred) == 0 || all(is.na(pred))) {
      return(data.frame(Message = "No se pudo calcular la predicción"))
    }
    pred
  }, rownames = FALSE, digits = 1)
  
  output$prediction_summary <- renderUI({
    req(prediction_reactive())
    pred <- prediction_reactive()
    if (nrow(pred) == 0 || all(is.na(pred))) {
      return(NULL)
    }
    
    max_prob <- max(pred[1, ], na.rm = TRUE)
    max_cluster <- names(pred)[which.max(pred[1, ])]
    
    div(
      class = "alert alert-info",
      h5("Interpretación:"),
      p(strong("Cluster más probable:"), max_cluster, paste0("(", round(max_prob, 1), "%)")),
      p("Las probabilidades se calculan basándose en la distribución empírica de pacientes similares en cada cluster.")
    )
  })
  
}

shinyApp(ui = ui, server = server)
