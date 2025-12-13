# =========================
# LIBRARIES
# =========================
library(shiny)
library(shinythemes)
library(readr)
library(dplyr)
library(ggplot2)
library(DT)
library(shinyWidgets)

# =========================
# UI
# =========================
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  tags$head(
    tags$style(HTML("
      /* Main container with sidebar */
      .app-wrapper {
        display: flex;
        min-height: calc(100vh - 50px);
      }
      
      /* Sidebar styling */
      .sidebar-panel {
        width: 280px;
        min-width: 280px;
        background: #f8f9fa;
        border-right: 1px solid #dee2e6;
        padding: 15px;
        overflow-y: auto;
        max-height: calc(100vh - 50px);
        transition: all 0.3s ease;
      }
      
      body.sidebar-collapsed .sidebar-panel {
        width: 0;
        min-width: 0;
        padding: 0;
        border: none;
        overflow: hidden;
      }
      
      /* Main content area */
      .main-content {
        flex: 1;
        display: flex;
        flex-direction: column;
        overflow: hidden;
      }
      
      .navbar-wrapper {
        flex-shrink: 0;
      }
      
      .tab-content-wrapper {
        flex: 1;
        overflow: auto;
        padding: 15px;
      }
      
      .sidebar-panel .form-group {
        margin-bottom: 15px;
      }
      
      /* Table container - prevent overflow */
      .table-container {
        width: 100%;
        overflow-x: auto;
        overflow-y: auto;
        max-height: calc(100vh - 200px);
        position: relative;
      }
      
      .dataTables_wrapper {
        width: 100% !important;
        overflow-x: auto !important;
      }
      
      .dataTables_scroll {
        width: 100% !important;
        overflow-x: auto !important;
      }
      
      table.dataTable {
        width: 100% !important;
        margin: 0 !important;
      }
      
      table.dataTable thead th,
      table.dataTable tbody td {
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
        max-width: 300px;
        padding: 8px !important;
      }
      
      /* Plot cards */
      .plot-card {
        background: #fff;
        border: 1px solid #dee2e6;
        border-radius: 8px;
        padding: 15px;
        margin-bottom: 15px;
      }
      
      /* Responsive adjustments */
      @media (max-width: 768px) {
        .sidebar-panel {
          position: absolute;
          z-index: 1000;
          height: calc(100vh - 50px);
          box-shadow: 2px 0 5px rgba(0,0,0,0.1);
        }
        body.sidebar-collapsed .sidebar-panel {
          display: none;
        }
      }
    "))
  ),
  
  # Main wrapper
  div(
    class = "app-wrapper",
    # Sidebar (persistent across pages)
    div(
      class = "sidebar-panel",
      actionButton(
        "toggle_sidebar",
        label = NULL,
        icon = icon("bars"),
        class = "btn-sm mb-3",
        style = "width: 100%;",
        title = "Ocultar/mostrar panel lateral"
      ),
      
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
        fileInput("file", "Sube tu CSV (.csv)", accept = ".csv"),
        hr()
      ),
      
      conditionalPanel(
        condition = "input.modo_datos == 'manual'",
        numericInput("edad_m", "Edad", value = 60, min = 18, max = 120),
        numericInput("imc_m", "IMC", value = 25, min = 10, max = 60, step = 0.1),
        selectInput(
          "tipo_histologico_m",
          "Tipo histológico",
          choices = c("Endometrioide", "Seroso", "Claro", "Mixto", "Otro"),
          selected = "Endometrioide"
        ),
        actionButton("crear_manual", "Crear registro", class = "btn-primary btn-block"),
        hr()
      ),
      
      # Data status indicator
      conditionalPanel(
        condition = "output.data_loaded",
        div(
          class = "alert alert-success",
          style = "margin-top: 15px; padding: 8px; font-size: 12px;",
          HTML("<strong>✓ Datos cargados</strong>"),
          br(),
          textOutput("data_info", inline = FALSE)
        )
      ),
      
      hr(),
      
      # Variable selector in sidebar
      conditionalPanel(
        condition = "output.data_loaded",
        h5("Variables para análisis"),
        pickerInput(
          "dist_fields",
          "Selecciona variables",
          choices = NULL,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `live-search` = TRUE,
            `selected-text-format` = "count > 3",
            size = 10
          )
        )
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
          selected = "Preview datos",
          
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
                  HTML("<h4>No hay datos cargados</h4><p>Sube un archivo CSV o introduce datos manualmente</p>")
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
                  HTML("<h4>No hay datos cargados</h4><p>Carga datos primero para ver las distribuciones</p>")
                )
              ),
              conditionalPanel(
                condition = "output.data_loaded",
                uiOutput("dist_plots")
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
      data(as.data.frame(df))
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
    # Validate inputs
    if (is.na(input$edad_m) || input$edad_m < 18 || input$edad_m > 120) {
      showNotification("Edad debe estar entre 18 y 120 años", type = "error")
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
  
  # Toggle sidebar
  observeEvent(input$toggle_sidebar, {
    session$sendCustomMessage("toggleSidebar", list())
  })
  
  # Update variable picker when data changes
  observeEvent(data(), {
    req(data())
    cols <- names(data())
    if (length(cols) > 0) {
      updatePickerInput(
        session,
        "dist_fields",
        choices = cols,
        selected = cols[1:min(3, length(cols))]
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
  
  # Distribution plot function
  make_distribution_plot <- function(df, field) {
    if (!field %in% names(df)) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "Variable no encontrada", size = 5) +
          theme_void()
      )
    }
    
    v <- df[[field]]
    v <- v[!is.na(v)]  # Remove NA values
    
    if (length(v) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "Sin datos disponibles", size = 5) +
          theme_void()
      )
    }
    
    if (is.numeric(v)) {
      ggplot(df, aes(x = .data[[field]])) +
        geom_histogram(bins = min(20, length(unique(v))), fill = "steelblue", color = "white", alpha = 0.8, na.rm = TRUE) +
        labs(title = field, x = NULL, y = "Frecuencia") +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", size = 12),
          panel.grid.minor = element_blank()
        )
    } else {
      # Limit factor levels for better display
      v_factor <- factor(v)
      if (length(levels(v_factor)) > 20) {
        # Keep top 19 most frequent and group others
        freq_table <- table(v_factor)
        top_levels <- names(sort(freq_table, decreasing = TRUE)[1:19])
        v_factor <- factor(
          ifelse(v_factor %in% top_levels, as.character(v_factor), "Otros"),
          levels = c(top_levels, "Otros")
        )
      }
      
      df_plot <- data.frame(x = v_factor)
      ggplot(df_plot, aes(x = x)) +
        geom_bar(fill = "steelblue", alpha = 0.8, na.rm = TRUE) +
        labs(title = field, x = NULL, y = "Frecuencia") +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor = element_blank()
        )
    }
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
        div(
          class = "plot-card",
          plotOutput(
            outputId = paste0("dist_", f),
            height = "280px"
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
        output[[paste0("dist_", field)]] <- renderPlot({
          make_distribution_plot(data(), field)
        })
      })
    }
  })
}

# =========================
# RUN APP
# =========================
shinyApp(ui = ui, server = server)
