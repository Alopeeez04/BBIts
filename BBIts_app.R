# =========================
# LIBRARIES
# =========================
library(shiny)
library(shinythemes)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(shinyWidgets)
library(plotly)

# =========================
# SOURCE PREDICTION FUNCTIONS
# =========================
source("R/prediction_model.R")

# =========================
# LOAD VALUE MAPPING FILE
# =========================
# Load field_value_mapping.csv once at app startup
# This file maps encoded dataset values to human-readable labels
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
        # Normalize types: convert valor to character for robust matching
        mapping_df$valor <- as.character(mapping_df$valor)
        # Remove rows with empty valor (continuous variables that don't need mapping)
        mapping_df <- mapping_df %>%
          filter(!is.na(valor) & valor != "")
        cat("Loaded value mapping from:", path, "\n")
        return(mapping_df)
      }, error = function(e) {
        cat("Error loading mapping from", path, ":", e$message, "\n")
      })
    }
  }
  
  # Return empty dataframe if file not found
  warning("field_value_mapping.csv not found. Distribution charts will use raw values.")
  return(data.frame(variable = character(), valor = character(), texto = character(), stringsAsFactors = FALSE))
}

# Load mapping at app startup
mapping_df <- load_value_mapping()

# =========================
# UI
# =========================
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  tags$head(
    tags$style(HTML("
      /* =========================
         CLINICAL COLOR PALETTE - CSS VARIABLES
         ========================= */
      :root {
        /* Primary: muted blue for key actions and headings */
        --color-primary: #5B7BA3;
        --color-primary-dark: #4A6FA5;
        --color-primary-light: #7A95B8;
        
        /* Secondary: cool gray for borders, dividers, secondary UI */
        --color-secondary: #7A8A9F;
        --color-secondary-light: #9AA5B5;
        --color-secondary-dark: #5A6B7F;
        
        /* Background: off-white (not pure white) */
        --color-bg: #FAFAFA;
        --color-bg-panel: #F5F5F5;
        --color-bg-sidebar: #F0F0F0;
        
        /* Text: near-black (not pure black) */
        --color-text: #2C2C2C;
        --color-text-secondary: #5A5A5A;
        --color-text-muted: #7A7A7A;
        
        /* Status/emphasis: subtle desaturated colors */
        --color-success: #6B8E6B;
        --color-info: #6B8FA5;
        --color-border: #D0D5DC;
        --color-border-light: #E5E8ED;
      }
      
      /* =========================
         GLOBAL STYLES
         ========================= */
      body, html {
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif !important;
        background-color: var(--color-bg) !important;
        color: var(--color-text) !important;
      }
      
      /* Global font consistency */
      * {
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif !important;
      }
      
      /* Ensure all text elements use consistent font */
      h1, h2, h3, h4, h5, h6,
      p, span, div, label,
      .navbar-brand, .navbar-nav,
      .btn, .form-control, .selectize-input,
      .well, .alert,
      table, th, td,
      .dataTables_wrapper,
      .bootstrap-select .filter-option,
      .bootstrap-select .dropdown-toggle {
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif !important;
      }
      
      /* Headings use primary color */
      h1, h2, h3, h4, h5, h6 {
        color: var(--color-primary-dark) !important;
      }
      
      /* =========================
         NAVBAR STYLING
         ========================= */
      .navbar {
        background-color: var(--color-primary) !important;
        border-bottom: 1px solid var(--color-border) !important;
      }
      
      .navbar-default {
        background-color: var(--color-primary) !important;
        border-color: var(--color-border) !important;
      }
      
      .navbar-default .navbar-brand,
      .navbar-default .navbar-nav > li > a {
        color: #FFFFFF !important;
      }
      
      .navbar-default .navbar-brand:hover,
      .navbar-default .navbar-nav > li > a:hover {
        color: #FFFFFF !important;
        background-color: var(--color-primary-dark) !important;
      }
      
      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > .active > a:hover,
      .navbar-default .navbar-nav > .active > a:focus {
        background-color: var(--color-primary-dark) !important;
        color: #FFFFFF !important;
      }
      
      /* Tab styling */
      .nav-tabs {
        border-bottom: 1px solid var(--color-border) !important;
      }
      
      .nav-tabs > li > a {
        color: var(--color-text-secondary) !important;
        background-color: var(--color-bg-panel) !important;
        border: 1px solid var(--color-border) !important;
        border-bottom-color: transparent !important;
      }
      
      .nav-tabs > li > a:hover {
        background-color: var(--color-bg) !important;
        border-color: var(--color-border) var(--color-border) var(--color-border) !important;
        color: var(--color-text) !important;
      }
      
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        color: var(--color-primary-dark) !important;
        background-color: var(--color-bg) !important;
        border: 1px solid var(--color-border) !important;
        border-bottom-color: transparent !important;
        font-weight: 500 !important;
      }
      
      /* =========================
         SIDEBAR STYLING
         ========================= */
      .sidebar-panel {
        width: 380px;
        min-width: 380px;
        overflow-y: auto;
        max-height: calc(100vh - 50px);
        transition: all 0.3s ease;
        background-color: var(--color-bg-sidebar) !important;
      }
      
      body.sidebar-collapsed .sidebar-panel {
        width: 0;
        min-width: 0;
        padding: 0;
        border: none;
        overflow: hidden;
      }
      
      .sidebar-panel .well {
        background-color: var(--color-bg-panel) !important;
        border: 1px solid var(--color-border) !important;
        color: var(--color-text) !important;
      }
      
      /* Floating toggle button when sidebar is collapsed */
      .sidebar-toggle-floating {
        position: fixed;
        top: 60px;
        left: 10px;
        z-index: 1001;
        display: none;
      }
      
      body.sidebar-collapsed .sidebar-toggle-floating {
        display: block;
      }
      
      /* =========================
         BUTTON STYLING
         ========================= */
      .btn-primary {
        background-color: var(--color-primary) !important;
        border-color: var(--color-primary-dark) !important;
        color: #FFFFFF !important;
      }
      
      .btn-primary:hover,
      .btn-primary:focus {
        background-color: var(--color-primary-dark) !important;
        border-color: var(--color-primary-dark) !important;
        color: #FFFFFF !important;
      }
      
      .btn-primary:active,
      .btn-primary.active {
        background-color: var(--color-primary-dark) !important;
        border-color: var(--color-primary-dark) !important;
        color: #FFFFFF !important;
      }
      
      .btn-default {
        background-color: var(--color-bg-panel) !important;
        border-color: var(--color-border) !important;
        color: var(--color-text) !important;
      }
      
      .btn-default:hover,
      .btn-default:focus {
        background-color: var(--color-secondary-light) !important;
        border-color: var(--color-secondary) !important;
        color: var(--color-text) !important;
      }
      
      .btn-default:active,
      .btn-default.active {
        background-color: var(--color-secondary) !important;
        border-color: var(--color-secondary-dark) !important;
        color: var(--color-text) !important;
      }
      
      .btn-success {
        background-color: var(--color-success) !important;
        border-color: var(--color-success) !important;
        color: #FFFFFF !important;
        font-weight: bold !important;
      }
      
      .btn-success:hover,
      .btn-success:focus {
        background-color: #5A7A5A !important;
        border-color: #5A7A5A !important;
        color: #FFFFFF !important;
      }
      
      .btn-success:active,
      .btn-success.active {
        background-color: #5A7A5A !important;
        border-color: #5A7A5A !important;
        color: #FFFFFF !important;
      }
      
      /* Radio group buttons */
      .btn-group-justified .btn {
        background-color: var(--color-bg-panel) !important;
        border-color: var(--color-border) !important;
        color: var(--color-text) !important;
      }
      
      .btn-group-justified .btn.active {
        background-color: var(--color-primary) !important;
        border-color: var(--color-primary-dark) !important;
        color: #FFFFFF !important;
      }
      
      .btn-group-justified .btn:hover:not(.active) {
        background-color: var(--color-bg) !important;
        border-color: var(--color-secondary) !important;
      }
      
      /* =========================
         FORM CONTROLS
         ========================= */
      .form-control,
      .selectize-input {
        background-color: #FFFFFF !important;
        border-color: var(--color-border) !important;
        color: var(--color-text) !important;
      }
      
      .form-control:focus,
      .selectize-input.focus {
        border-color: var(--color-primary) !important;
        box-shadow: 0 0 0 0.2rem rgba(91, 123, 163, 0.15) !important;
      }
      
      /* Variable selector styling */
      .variable-selector-panel {
        box-shadow: 0 2px 4px rgba(0,0,0,0.08) !important;
        background-color: var(--color-bg-panel) !important;
        border: 1px solid var(--color-border) !important;
      }
      
      .variable-selector-panel .bootstrap-select {
        border: 1px solid var(--color-border) !important;
        border-radius: 4px;
      }
      
      .variable-selector-panel .bootstrap-select:focus {
        border-color: var(--color-primary) !important;
        box-shadow: 0 0 0 0.2rem rgba(91, 123, 163, 0.15) !important;
      }
      
      /* =========================
         ALERTS AND PANELS
         ========================= */
      .alert-success {
        background-color: rgba(107, 142, 107, 0.1) !important;
        border-color: var(--color-success) !important;
        color: var(--color-text) !important;
      }
      
      .alert-info {
        background-color: rgba(107, 143, 165, 0.1) !important;
        border-color: var(--color-info) !important;
        color: var(--color-text) !important;
      }
      
      .well {
        background-color: var(--color-bg-panel) !important;
        border: 1px solid var(--color-border) !important;
        color: var(--color-text) !important;
      }
      
      /* =========================
         DATATABLE STYLING
         ========================= */
      .dataTables_wrapper {
        width: 100% !important;
        overflow-x: auto !important;
        color: var(--color-text) !important;
      }
      
      table.dataTable {
        border-collapse: separate !important;
        border-spacing: 0 !important;
      }
      
      table.dataTable thead th {
        background-color: var(--color-bg-panel) !important;
        border-bottom: 2px solid var(--color-border) !important;
        border-right: 1px solid var(--color-border-light) !important;
        color: var(--color-text) !important;
        font-weight: 500 !important;
      }
      
      table.dataTable thead th:last-child {
        border-right: none !important;
      }
      
      table.dataTable tbody td {
        border-bottom: 1px solid var(--color-border-light) !important;
        border-right: 1px solid var(--color-border-light) !important;
        color: var(--color-text) !important;
      }
      
      table.dataTable tbody td:last-child {
        border-right: none !important;
      }
      
      table.dataTable tbody tr:hover {
        background-color: var(--color-bg-panel) !important;
      }
      
      table.dataTable thead th,
      table.dataTable tbody td {
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
        max-width: 300px;
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif !important;
      }
      
      /* DataTables controls */
      .dataTables_wrapper .dataTables_length,
      .dataTables_wrapper .dataTables_filter,
      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate {
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif !important;
        color: var(--color-text) !important;
      }
      
      .dataTables_wrapper .dataTables_filter input {
        border-color: var(--color-border) !important;
      }
      
      .dataTables_wrapper .dataTables_filter input:focus {
        border-color: var(--color-primary) !important;
      }
      
      .dataTables_wrapper .dataTables_paginate .paginate_button {
        color: var(--color-text) !important;
        border-color: var(--color-border) !important;
      }
      
      .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
        background-color: var(--color-bg-panel) !important;
        border-color: var(--color-secondary) !important;
        color: var(--color-text) !important;
      }
      
      .dataTables_wrapper .dataTables_paginate .paginate_button.current {
        background-color: var(--color-primary) !important;
        border-color: var(--color-primary-dark) !important;
        color: #FFFFFF !important;
      }
      
      .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
        background-color: var(--color-primary-dark) !important;
        color: #FFFFFF !important;
      }
      
      /* =========================
         MAIN CONTENT AREA
         ========================= */
      .app-wrapper {
        display: flex;
        min-height: calc(100vh - 50px);
        background-color: var(--color-bg) !important;
      }
      
      .main-content {
        flex: 1;
        display: flex;
        flex-direction: column;
        overflow: hidden;
        background-color: var(--color-bg) !important;
      }
      
      .navbar-wrapper {
        flex-shrink: 0;
      }
      
      .tab-content-wrapper {
        flex: 1;
        overflow: auto;
        padding: 15px;
        background-color: var(--color-bg) !important;
      }
      
      /* Table container - prevent overflow */
      .table-container {
        width: 100%;
        overflow-x: auto;
        overflow-y: auto;
        max-height: calc(100vh - 200px);
        background-color: #FFFFFF !important;
        border: 1px solid var(--color-border) !important;
        border-radius: 4px;
      }
      
      /* =========================
         PLOT CARDS
         ========================= */
      .plot-card {
        margin-bottom: 15px;
        background-color: #FFFFFF !important;
        border: 1px solid var(--color-border) !important;
        border-radius: 4px;
        padding: 15px;
      }
      
      /* =========================
         PICKER INPUT AND OTHER WIDGETS
         ========================= */
      .bootstrap-select .filter-option,
      .bootstrap-select .dropdown-toggle,
      .bootstrap-select .dropdown-menu,
      .selectize-input,
      .selectize-dropdown {
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif !important;
        color: var(--color-text) !important;
      }
      
      .bootstrap-select .dropdown-menu {
        background-color: #FFFFFF !important;
        border-color: var(--color-border) !important;
      }
      
      .bootstrap-select .dropdown-menu li a {
        color: var(--color-text) !important;
      }
      
      .bootstrap-select .dropdown-menu li a:hover {
        background-color: var(--color-bg-panel) !important;
        color: var(--color-text) !important;
      }
      
      /* =========================
         HR AND DIVIDERS
         ========================= */
      hr {
        border-top-color: var(--color-border) !important;
      }
      
      /* =========================
         TEXT COLORS
         ========================= */
      .text-muted {
        color: var(--color-text-muted) !important;
      }
      
      /* =========================
         VARIABLE SELECTOR IN SIDEBAR
         ========================= */
      .variable-selector-container {
        margin-top: 15px;
        margin-bottom: 15px;
        border: 1px solid var(--color-border) !important;
        border-radius: 4px;
        background-color: var(--color-bg-panel) !important;
        padding: 10px;
      }
      
      .variable-selector-scrollable {
        max-height: 300px;
        overflow-y: auto;
        overflow-x: hidden;
        padding-right: 5px;
      }
      
      .variable-selector-scrollable::-webkit-scrollbar {
        width: 6px;
      }
      
      .variable-selector-scrollable::-webkit-scrollbar-track {
        background: var(--color-bg);
        border-radius: 3px;
      }
      
      .variable-selector-scrollable::-webkit-scrollbar-thumb {
        background: var(--color-secondary);
        border-radius: 3px;
      }
      
      .variable-selector-scrollable::-webkit-scrollbar-thumb:hover {
        background: var(--color-secondary-dark);
      }
      
      .variable-selector-header {
        margin-bottom: 10px;
        padding-bottom: 8px;
        border-bottom: 1px solid var(--color-border) !important;
      }
      
      .pretty-checkbox-group {
        margin-top: 5px;
      }
      
      .pretty-checkbox-group label {
        color: var(--color-text) !important;
        font-size: 13px;
        margin-bottom: 5px;
      }
    "))
  ),
  
  # Floating toggle button (shown when sidebar is collapsed)
  actionButton(
    "toggle_sidebar_floating",
    label = NULL,
    icon = icon("bars"),
    class = "btn-default sidebar-toggle-floating",
    title = "Mostrar panel lateral"
  ),
  
  # Main wrapper
  div(
    class = "app-wrapper",
    # Sidebar (persistent across pages)
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
    
    # Main content area with navbar
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
              h3("Predicted cluster probabilities (%)"),
              p("Probabilidades de pertenencia a cada cluster basadas en los valores ingresados en el panel lateral."),
              conditionalPanel(
                condition = "input.modo_datos == 'manual'",
                tableOutput("prediction_table"),
                br(),
                uiOutput("prediction_summary")
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
              h3("Impacto de Variables por Cohortes"),
              p("Distribución de variables según cluster y estado vital."),
              conditionalPanel(
                condition = "output.data_loaded",
                plotlyOutput("impact_heatmap_not_died", height = "500px"),
                br(),
                plotlyOutput("impact_heatmap_died", height = "500px")
              ),
              conditionalPanel(
                condition = "!output.data_loaded",
                div(
                  class = "alert alert-info text-center",
                  style = "margin-top: 50px;",
                  h4("No hay datos cargados"),
                  p("Carga datos primero para ver los heatmaps de impacto")
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

# =========================
# SERVER
# =========================
server <- function(input, output, session) {
  
  # Reactive data storage
  data <- reactiveVal(NULL)
  
  # ReactiveValues for prediction results (from Predict button)
  rv <- reactiveValues(pred = NULL)
  
  # Value mapping table (loaded at app startup, accessible in server)
  # mapping_df is defined in global scope and available here
  
  # =========================
  # HEATMAP HELPER FUNCTIONS
  # Extracted from BBIts25_filtrar.Rmd
  # =========================
  
  # Variable labels for heatmaps (matching Rmd exactly)
  heatmap_var_labels <- c(
    "afectacion_linf" = "Afectació limfàtica",
    "AP_centinela_pelvico" = "AP centinela pèlvic",
    "AP_ganPelv" = "AP gangli pèlvic",
    "beta_cateninap" = "Beta-catenina",
    "Grado" = "Grau tumoral",
    "grado_histologi" = "Grau histològic",
    "grupo_de_riesgo_definitivo" = "Grup de risc",
    "libre_enferm" = "Lliure de malaltia",
    "mlh1" = "MLH1",
    "tipo_histologico" = "Tipus histològico",
    "Tributaria_a_Radioterapia" = "Radioteràpia"
  )
  
  # Variables of interest for heatmaps (matching Rmd)
  heatmap_vars_sig <- c(
    "Grado", "libre_enferm", "beta_cateninap", "mlh1",
    "grupo_de_riesgo_definitivo", "Tributaria_a_Radioterapia",
    "afectacion_linf", "grado_histologi", "AP_centinela_pelvico",
    "AP_ganPelv", "tipo_histologico"
  )
  
  # Prepare heatmap data from dataset
  # Returns data frame with cluster_label, variable, freq (max frequency per cluster-variable)
  # Matching Rmd logic: computes frequency of each value, then takes max for heatmap display
  prepare_heatmap_data <- function(df, death_status_filter) {
    # Check required columns - allow missing some variables
    required_base_cols <- c("cluster_label", "recidiva_exitus")
    missing_base <- setdiff(required_base_cols, names(df))
    
    if (length(missing_base) > 0) {
      return(NULL)
    }
    
    # Filter by death status
    # death_status_filter: "Vivos" (alive) or "Muertos" (died)
    df_filtered <- df %>%
      mutate(death_status = ifelse(recidiva_exitus == 1, "Muertos", "Vivos")) %>%
      filter(death_status == death_status_filter)
    
    if (nrow(df_filtered) == 0) {
      return(NULL)
    }
    
    # Get available variables from heatmap_vars_sig that exist in the data
    available_vars <- intersect(heatmap_vars_sig, names(df_filtered))
    
    if (length(available_vars) == 0) {
      return(NULL)
    }
    
    # Prepare data: pivot and compute frequencies (matching Rmd exactly)
    heatmap_data <- df_filtered %>%
      select(cluster_label, death_status, all_of(available_vars)) %>%
      mutate(across(all_of(available_vars), as.factor)) %>%
      pivot_longer(cols = all_of(available_vars), names_to = "variable", values_to = "value") %>%
      filter(!is.na(value) & value != "" & as.character(value) != "NA") %>%
      group_by(cluster_label, death_status, variable, value) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(cluster_label, death_status, variable) %>%
      mutate(freq = n / sum(n)) %>%
      # Take maximum frequency for each (cluster, variable) combination
      # This represents the proportion of the most common value within that cluster-variable
      group_by(cluster_label, variable) %>%
      summarise(freq = max(freq, na.rm = TRUE), .groups = "drop")
    
    return(heatmap_data)
  }
  
  # Build green heatmap (patients who did NOT die)
  build_heatmap_not_died <- function(df) {
    heatmap_data <- prepare_heatmap_data(df, "Vivos")
    
    if (is.null(heatmap_data) || nrow(heatmap_data) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                   label = "Datos insuficientes o columnas requeridas no disponibles", size = 5) +
          theme_void()
      )
    }
    
    # Ensure cluster_label is factor with correct order
    cluster_order <- c("Buen pronóstico", "Mixto", "Alto riesgo")
    heatmap_data$cluster_label <- factor(
      heatmap_data$cluster_label,
      levels = cluster_order[cluster_order %in% unique(heatmap_data$cluster_label)]
    )
    
    # Ensure variable order matches Rmd
    heatmap_data$variable <- factor(
      heatmap_data$variable,
      levels = heatmap_vars_sig[heatmap_vars_sig %in% unique(heatmap_data$variable)]
    )
    
    # Create plot matching Rmd exactly
    p <- ggplot(heatmap_data, aes(x = cluster_label, y = variable, fill = freq,
                                   text = paste0(
                                     "Cluster: ", cluster_label, "\n",
                                     "Variable: ", variable, "\n",
                                     "Proporción: ", round(freq * 100, 1), "%"
                                   ))) +
      geom_tile(color = "black") +
      scale_fill_gradient(low = "white", high = "#00C040", name = "Proporción") +
      scale_y_discrete(labels = heatmap_var_labels) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(angle = 0)
      ) +
      labs(
        title = "Pacients vius amb pacients no recessiva",
        x = "Cluster",
        y = "Variable"
      )
    
    return(p)
  }
  
  # Build red heatmap (patients who died)
  build_heatmap_died <- function(df) {
    heatmap_data <- prepare_heatmap_data(df, "Muertos")
    
    if (is.null(heatmap_data) || nrow(heatmap_data) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                   label = "Datos insuficientes o columnas requeridas no disponibles", size = 5) +
          theme_void()
      )
    }
    
    # Ensure cluster_label is factor with correct order
    cluster_order <- c("Buen pronóstico", "Mixto", "Alto riesgo")
    heatmap_data$cluster_label <- factor(
      heatmap_data$cluster_label,
      levels = cluster_order[cluster_order %in% unique(heatmap_data$cluster_label)]
    )
    
    # Ensure variable order matches Rmd
    heatmap_data$variable <- factor(
      heatmap_data$variable,
      levels = heatmap_vars_sig[heatmap_vars_sig %in% unique(heatmap_data$variable)]
    )
    
    # Create plot matching Rmd exactly
    p <- ggplot(heatmap_data, aes(x = cluster_label, y = variable, fill = freq,
                                   text = paste0(
                                     "Cluster: ", cluster_label, "\n",
                                     "Variable: ", variable, "\n",
                                     "Proporción: ", round(freq * 100, 1), "%"
                                   ))) +
      geom_tile(color = "black") +
      scale_fill_gradient(low = "white", high = "#DB0B00", name = "Proporción") +
      scale_y_discrete(labels = heatmap_var_labels) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_blank()  # Matching Rmd: ocultar nombres de variables
      ) +
      labs(
        title = "Pacients morts amb pacients no recessiva",
        x = "Cluster",
        y = ""
      )
    
    return(p)
  }
  
  # Data status indicator
  output$data_loaded <- reactive({
    !is.null(data())
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  # Data info text
  output$data_info <- renderText({
    req(data())
    df <- data()
    paste0(
      "Datos: ", nrow(df), " filas, ", ncol(df), " columnas"
    )
  })
  
  # Load CSV file
  observeEvent(input$file, {
    req(input$file)
    tryCatch({
      df <- read_csv(input$file$datapath, show_col_types = FALSE)
      df <- as.data.frame(df)
      
      # Normalize column types to match manual input format
      # read_csv() infers column types automatically. If CSV contains numeric codes
      # (e.g., 1, 2, 3 for tipo_histologico), read_csv() infers them as double.
      # However, Shiny selectInput() widgets always return character strings,
      # even when choices are numeric (e.g., choices = c(0,1) returns "0" or "1").
      # Convert categorical columns to character to ensure bind_rows() compatibility.
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
  
  # Add manual entry
  observeEvent(input$crear_manual, {
    # Validate required inputs
    if (is.na(input$edad_m) || input$edad_m < 18) {
      showNotification("Edad debe ser mayor o igual a 18 años", type = "error")
      return()
    }
    if (is.na(input$imc_m) || input$imc_m < 10 || input$imc_m > 60) {
      showNotification("IMC debe estar entre 10 y 60", type = "error")
      return()
    }
    
    # Create new row with all fields
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
  
  # Toggle sidebar (both buttons)
  observeEvent(input$toggle_sidebar, {
    session$sendCustomMessage("toggleSidebar", list())
  })
  
  observeEvent(input$toggle_sidebar_floating, {
    session$sendCustomMessage("toggleSidebar", list())
  })
  
  # Allowed variables for distribution plots
  ALLOWED_VARIABLES <- c(
    "Grado", "libre_enferm", "beta_cateninap", "mlh1", 
    "grupo_de_riesgo_definitivo", "Tributaria_a_Radioterapia", 
    "afectacion_linf", "grado_histologi", "AP_centinela_pelvico", 
    "AP_ganPelv", "tipo_histologico_collapsed"
  )
  
  # Update variable checkbox group when data changes
  observeEvent(data(), {
    req(data())
    cols <- names(data())
    # Filter to only allowed variables that exist in the data
    available_vars <- intersect(ALLOWED_VARIABLES, cols)
    
    if (length(available_vars) > 0) {
      updatePrettyCheckboxGroup(
        session,
        "dist_fields",
        choices = available_vars,
        selected = available_vars[1:min(3, length(available_vars))]
      )
    } else {
      # If no allowed variables found, show message
      updatePrettyCheckboxGroup(
        session,
        "dist_fields",
        choices = character(0),
        selected = NULL
      )
    }
  })
  
  # Preview table (limit columns for performance)
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
  
  # =========================
  # CHART STYLING CONFIGURATION
  # Improved for clinical publication quality while maintaining scientific correctness
  # =========================
  CHART_CONFIG <- list(
    # Colors - muted, publication-appropriate palette
    bar_color = "black",
    bar_alpha = 0.75,
    fill_palette = "Set3",
    
    # Title - improved legibility
    title_hjust = 0.5,
    title_face = "bold",
    title_size = 15,
    title_margin = margin(b = 12),
    
    # Axis labels - improved legibility
    axis_title_size = 12,
    axis_title_face = "plain",
    
    # Axis text - improved readability
    axis_text_x_angle = 45,
    axis_text_x_hjust = 1,
    axis_text_x_size = 10.5,
    axis_text_y_size = 10.5,
    
    # Plot margins - better spacing
    plot_margin = margin(10, 15, 10, 15),
    
    # Y-axis limit multiplier
    ylim_multiplier = 1.1
  )
  
  # Special cases for specific variables (from reference file)
  SPECIAL_CASES <- list(
    "tamano_tumoral" = list(
      axis_text_x_angle = 0,
      axis_text_x_size = 11,
      ylim_multiplier = 1.15
    )
  )
  
  # =========================
  # VALUE MAPPING HELPER FUNCTION
  # =========================
  # Maps encoded values to human-readable labels using the mapping table
  # Input: vector v, field name field, mapping table mapping_df
  # Output: character vector of mapped labels
  # Handles missing mappings gracefully by returning original values
  map_values <- function(v, field, mapping_df) {
    # Convert input to character for matching
    v_char <- as.character(v)
    
    # Check if field exists in mapping
    field_mappings <- mapping_df %>% filter(variable == field)
    
    if (nrow(field_mappings) == 0) {
      # No mapping for this field, return original values as character
      return(v_char)
    }
    
    # Handle potential duplicates: take first mapping if same valor appears multiple times
    field_mappings <- field_mappings %>%
      distinct(valor, .keep_all = TRUE)
    
    # Create a lookup vector: valor -> texto
    lookup <- setNames(field_mappings$texto, field_mappings$valor)
    
    # Map values: if found in lookup, use mapped label; otherwise use original value
    mapped <- ifelse(
      v_char %in% names(lookup),
      lookup[v_char],
      v_char  # Fallback to original value if not in mapping
    )
    
    return(mapped)
  }
  
  # Distribution plot function (matching Diagnostic mixte.Rmd style exactly)
  # Now uses value mapping to display human-readable labels
  make_distribution_plot <- function(df, field) {
    if (!field %in% names(df)) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "Variable no encontrada", size = 5) +
          theme_void()
      )
    }
    
    # Filter out NA values strictly (including empty strings and "NA" strings)
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
    
    # Convert to character for categorical variables
    v_temp <- as.character(data_clean[[field]])
    v_temp <- v_temp[!is.na(v_temp) & v_temp != "" & v_temp != "NA"]
    
    if (length(v_temp) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "Sin datos disponibles", size = 5) +
          theme_void()
      )
    }
    
    # Apply value mapping to get human-readable labels
    v_mapped <- map_values(v_temp, field, mapping_df)
    
    # Create frequency table using mapped labels
    frecuencia <- table(v_mapped)
    
    # Create dataframe with Frecuencia and Porcentaje
    # Matching reference: round(as.numeric(prop.table(frecuencia)) * 100, 2)
    df_plot <- data.frame(
      Valor = names(frecuencia),
      Frecuencia = as.numeric(frecuencia),
      Porcentaje = round(as.numeric(prop.table(frecuencia)) * 100, 2),
      stringsAsFactors = FALSE
    )
    
    # Preserve order: if mapping exists, use valor order from mapping; otherwise use frequency order
    field_mappings <- mapping_df %>% filter(variable == field)
    if (nrow(field_mappings) > 0) {
      # Get ordered valor values from mapping (preserve original order in mapping file)
      ordered_valors <- unique(field_mappings$valor)
      # Create mapping: valor -> texto (taking first match if duplicates)
      valor_to_texto <- field_mappings %>%
        select(valor, texto) %>%
        distinct(valor, .keep_all = TRUE)
      # Map to texto labels in order
      ordered_labels <- valor_to_texto$texto[match(ordered_valors, valor_to_texto$valor)]
      # Keep only labels that appear in the data
      ordered_labels <- ordered_labels[ordered_labels %in% df_plot$Valor]
      # Add any unmapped values that appear in data but not in mapping
      unmapped <- setdiff(df_plot$Valor, ordered_labels)
      if (length(unmapped) > 0) {
        ordered_labels <- c(ordered_labels, unmapped)
      }
      # Set factor levels to preserve mapping order
      df_plot$Valor <- factor(df_plot$Valor, levels = ordered_labels)
    } else {
      # No mapping: use frequency order (most common first)
      df_plot$Valor <- factor(df_plot$Valor, levels = df_plot$Valor[order(df_plot$Frecuencia, decreasing = TRUE)])
    }
    
    # Get config for this field (use special case if exists, otherwise default)
    config <- CHART_CONFIG
    if (field %in% names(SPECIAL_CASES)) {
      special <- SPECIAL_CASES[[field]]
      for (key in names(special)) {
        config[[key]] <- special[[key]]
      }
    }
    
    # Calculate y-axis breaks for better readability
    max_pct <- max(df_plot$Porcentaje) * config$ylim_multiplier
    y_breaks <- pretty(c(0, max_pct), n = 5)
    y_max <- max(y_breaks)
    
    # Create plot with improved visual quality
    # Maintains scientific correctness while improving readability
    # Add text aesthetic for plotly tooltips
    p <- ggplot(df_plot, aes(x = Valor, y = Porcentaje, fill = Valor,
                              text = paste0(
                                "Categoría: ", Valor, "\n",
                                "Porcentaje: ", Porcentaje, "%\n",
                                "Frecuencia: ", Frecuencia
                              ))) +
      geom_bar(stat = "identity", color = config$bar_color, alpha = config$bar_alpha, width = 0.7) +
      labs(
        title = paste("Distribució de freqüències:", field),
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
  
  # Render distribution plots
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
  
  # Generate plots dynamically
  observe({
    req(data())
    fields <- input$dist_fields
    if (is.null(fields) || length(fields) == 0) return()
    
    for (f in fields) {
      local({
        field <- f
        output[[paste0("dist_", field)]] <- renderPlotly({
          p <- make_distribution_plot(data(), field)
          # Convert to plotly with tooltips, disable mode bar for clean medical look
          ggplotly(p, tooltip = "text") %>%
            config(displayModeBar = FALSE)
        })
      })
    }
  })
  
  # =========================
  # PREDICTION LOGIC
  # =========================
  
  # Helper function to get manual case data from inputs
  get_manual_case <- function() {
    req(input$modo_datos == "manual")
    tryCatch({
      map_input_to_prediction()
    }, error = function(e) {
      NULL
    })
  }
  
  # Helper function to map manual input values to prediction format
  map_input_to_prediction <- function() {
    # Map Grado: G1/G2 -> Bajo, G3 -> Alto
    grado_val <- if (input$Grado_m %in% c("G1", "G2")) "Bajo" else "Alto"
    
    # Map grado_histologi: G1/G2 -> Bajo, G3 -> Alto
    grado_hist_val <- if (input$grado_histologi_m %in% c("G1", "G2")) "Bajo" else "Alto"
    
    # Map afectacion_linf: 0 -> No, 1 -> Si
    afectacion_linf_val <- if (as.character(input$afectacion_linf_m) == "0") "No" else "Si"
    
    # Map AP_centinela_pelvico: 0 -> pN0, 1 -> pN1 (simplified mapping)
    # Note: Full mapping would be 0->pN0, 1->pN0(i+), 2->pN1(mi), 3->pN1, 4->pNx
    ap_centinela_val <- if (as.character(input$AP_centinela_pelvico_m) == "0") "pN0" else "pN1"
    
    # Map AP_ganPelv: 0 -> Negativo, 1 -> Macrometastasis (simplified mapping)
    # Note: Full mapping would be 0->Negativo, 1->Cels_aisladas, 2->Micrometastasis, 3->Macrometastasis
    ap_ganpelv_val <- if (as.character(input$AP_ganPelv_m) == "0") "Negativo" else "Macrometastasis"
    
    # Map beta_cateninap: Positivo -> "1", Negativo -> "0"
    # Note: The empirical table may use "0", "1", "2", "NA" - using "1" for Positivo, "0" for Negativo
    beta_cat_val <- if (input$beta_cateninap_m == "Positivo") "1" else "0"
    
    # Map mlh1: Need to check what values are in empirical table
    # For now, mapping Positivo -> "0", Negativo -> "0" (may need adjustment)
    # The empirical table likely uses "0", "1" or similar
    mlh1_val <- if (input$mlh1_m == "Positivo") "0" else "0"
    
    # Map grupo_de_riesgo_definitivo: keep as is
    grupo_riesgo_val <- input$grupo_de_riesgo_definitivo_m
    
    # Map tipo_histologico: keep as is (needs to match empirical table values)
    tipo_hist_val <- as.character(input$tipo_histologico_m)
    
    # For libre_enferm and Tributaria_a_Radioterapia, use default values
    # These are not in the manual input panel, so using defaults
    libre_enferm_val <- "No"  # Default value
    tributaria_rt_val <- "No"  # Default value
    
    # Create data frame with required columns
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
  
  # Reactive: convert manual inputs to prediction format
  new_patient_reactive <- reactive({
    req(input$modo_datos == "manual")
    tryCatch({
      map_input_to_prediction()
    }, error = function(e) {
      NULL
    })
  })
  
  # Observe Predict button click - runs prediction without adding to dataset
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
    
    # Validate that model objects exist
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
      } else {
        rv$pred <- pred_result
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
    })
  })
  
  # Reactive: compute prediction
  # Uses stored prediction from Predict button if available, otherwise uses reactive
  prediction_reactive <- reactive({
    # If Predict button was clicked, use stored result
    if (!is.null(rv$pred)) {
      return(rv$pred)
    }
    
    # Otherwise, use reactive computation (for initial load or when inputs change)
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
  
  # Render prediction table
  output$prediction_table <- renderTable({
    req(prediction_reactive())
    pred <- prediction_reactive()
    if (nrow(pred) == 0 || all(is.na(pred))) {
      return(data.frame(Message = "No se pudo calcular la predicción"))
    }
    pred
  }, rownames = FALSE, digits = 1)
  
  # Render prediction summary text
  output$prediction_summary <- renderUI({
    req(prediction_reactive())
    pred <- prediction_reactive()
    if (nrow(pred) == 0 || all(is.na(pred))) {
      return(NULL)
    }
    
    # Find cluster with highest probability
    max_prob <- max(pred[1, ], na.rm = TRUE)
    max_cluster <- names(pred)[which.max(pred[1, ])]
    
    div(
      class = "alert alert-info",
      h5("Interpretación:"),
      p(strong("Cluster más probable:"), max_cluster, paste0("(", round(max_prob, 1), "%)")),
      p("Las probabilidades se calculan basándose en la distribución empírica de pacientes similares en cada cluster.")
    )
  })
  
  # =========================
  # IMPACTO POR COHORTES HEATMAPS
  # =========================
  
  # Render green heatmap (patients who did NOT die)
  output$impact_heatmap_not_died <- renderPlotly({
    req(data())
    df <- data()
    
    # Validate required columns
    validate(
      need("recidiva_exitus" %in% names(df), 
           "Columna 'recidiva_exitus' no encontrada en los datos."),
      need("cluster_label" %in% names(df) || any(grepl("cluster", names(df), ignore.case = TRUE)),
           "Información de clusters no encontrada. Los heatmaps requieren datos con clusters asignados.")
    )
    
    # Check if cluster_label exists, if not try cluster or compute from available data
    if (!"cluster_label" %in% names(df)) {
      # Try to find cluster column
      cluster_col <- grep("cluster", names(df), ignore.case = TRUE, value = TRUE)[1]
      if (!is.na(cluster_col)) {
        # Map cluster numbers to labels if needed
        df$cluster_label <- case_when(
          as.character(df[[cluster_col]]) == "1" ~ "Buen pronóstico",
          as.character(df[[cluster_col]]) == "2" ~ "Alto riesgo",
          as.character(df[[cluster_col]]) == "3" ~ "Mixto",
          TRUE ~ as.character(df[[cluster_col]])
        )
      } else {
        return(
          ggplot() +
            annotate("text", x = 0.5, y = 0.5, 
                     label = "Datos no contienen información de clusters.\nLos heatmaps requieren clusters asignados.", 
                     size = 5) +
            theme_void()
        ) %>% ggplotly() %>% config(displayModeBar = FALSE)
      }
    }
    
    p <- build_heatmap_not_died(df)
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  # Render red heatmap (patients who died)
  output$impact_heatmap_died <- renderPlotly({
    req(data())
    df <- data()
    
    # Validate required columns
    validate(
      need("recidiva_exitus" %in% names(df), 
           "Columna 'recidiva_exitus' no encontrada en los datos."),
      need("cluster_label" %in% names(df) || any(grepl("cluster", names(df), ignore.case = TRUE)),
           "Información de clusters no encontrada. Los heatmaps requieren datos con clusters asignados.")
    )
    
    # Check if cluster_label exists, if not try cluster or compute from available data
    if (!"cluster_label" %in% names(df)) {
      # Try to find cluster column
      cluster_col <- grep("cluster", names(df), ignore.case = TRUE, value = TRUE)[1]
      if (!is.na(cluster_col)) {
        # Map cluster numbers to labels if needed
        df$cluster_label <- case_when(
          as.character(df[[cluster_col]]) == "1" ~ "Buen pronóstico",
          as.character(df[[cluster_col]]) == "2" ~ "Alto riesgo",
          as.character(df[[cluster_col]]) == "3" ~ "Mixto",
          TRUE ~ as.character(df[[cluster_col]])
        )
      } else {
        return(
          ggplot() +
            annotate("text", x = 0.5, y = 0.5, 
                     label = "Datos no contienen información de clusters.\nLos heatmaps requieren clusters asignados.", 
                     size = 5) +
            theme_void()
        ) %>% ggplotly() %>% config(displayModeBar = FALSE)
      }
    }
    
    p <- build_heatmap_died(df)
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
}

# =========================
# RUN APP
# =========================
shinyApp(ui = ui, server = server)
