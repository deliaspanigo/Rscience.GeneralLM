
#' @export
MASTER_module_fixed_anova_1_way_ui <- function(id) {
  ns <- NS(id)
  
  # Contenido principal organizado en columnas para mantener los cards separados
  div(
    # Encabezado con botones en una fila
    card(
      card_body(
        div(
          class = "d-flex justify-content-start gap-2 mb-3",
          Sbutton_01_dataselector_ui(ns("dataset_selector")),
          Sbutton_02_tools_ui(ns("selector_tools")),
          Sbutton_03_variable_selector_ui(ns("selector_variables")),
          Sbutton_reset_ui(ns("reset_button")),
          Sbutton_play_ui(ns("play_button"))
        )
      )
    ),
    
    uiOutput(ns("show_dev_full")),
    
    
    # Panel de resultados
    card(
      card_header("Resultados"),
      card_body(
        # Mensaje de estado
        uiOutput(ns("mensaje_seleccion"))
        # Mostrar datos simple
        #tableOutput("tabla_datos"),
      )
    ),
    
    # Card separado para Quarto
    div(
      style = "margin-top: 20px; width: 100%;",
      ""
      # quartoRendererUI(id = "quarto_doc")
    )
  )
}

#' @export
MASTER_module_fixed_anova_1_way_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Mostrar información sobre el dataset
    my_show_dev <- TRUE 
    
    valores_default <- list(
      pack_import_dataset = "",
      check_import_dataset = FALSE,
      
      pack_tool_selection = "",
      check_tool_selection = FALSE,
      
      pack_var_selection = "",
      check_var_selection = FALSE,
      
      check_play = FALSE,
      the_results = ""
      
    )
    
    # Valores reactivos inicializados con valores vacíos
    valores_internos <- do.call(reactiveValues, valores_default)
    valores_activos  <- do.call(reactiveValues, valores_default)
    
    
    # Inicializar los módulos y guardar los callbacks de reseteo
    reset_callbacks <- list()
    
    dataset_module  <- Sbutton_01_dataselector_server("dataset_selector", valores_internos)
    reset_callbacks <- c(reset_callbacks, dataset_module$reset)
    
    
    
    # # Inicializar el módulo de selector de datos
    tools_module <- Sbutton_02_tools_server("selector_tools", valores_default, valores_internos)
    reset_callbacks <- c(reset_callbacks, tools_module$reset)
    
    # Inicializar el módulo de selector de variables
    variables_module <- Sbutton_03_variable_selector_server("selector_variables", valores_default, valores_internos)
    reset_callbacks <- c(reset_callbacks, variables_module$reset)
    
    # Inicializar el módulo de botón PLAY
    play_module <- Sbutton_play_server("play_button", valores_default, valores_internos, valores_activos)
    reset_callbacks <- c(reset_callbacks, play_module$reset)
    
    # Inicializar el módulo de reseteo con los callbacks
    Sbutton_reset_server("reset_button", valores_default, valores_internos, valores_activos, reset_callbacks)
    
    
    
    output$show_dev_full <- renderUI({
      ns <- NS(id)
      
      req(my_show_dev)
      
      # Fila separada para el contenido en otra tarjeta
      card(
        card_body(
          fluidRow(uiOutput(ns("info_text2"))),
          fluidRow(verbatimTextOutput(ns("info_text"))),
          fluidRow(
            column(4, verbatimTextOutput(ns("debug_rv_default"))),
            column(4, verbatimTextOutput(ns("debug_rv_internos"))),
            column(4, verbatimTextOutput(ns("debug_rv_activos")))
            
            
          )
        )
      )
    })
    
    output$debug_rv_default <- renderPrint({
      # Crear una lista con toda la información relevante
      
      
      # Mostrar la lista completa
      str(valores_default, max.level = 2)
    })
    output$debug_rv_internos <- renderPrint({
      valores_internos_list <- reactiveValuesToList(valores_internos)
      
      # Imprimir la estructura con detalles
      str(valores_internos_list, max.level = 3)
    })
    output$debug_rv_activos <- renderPrint({
      valores_activos_list <- reactiveValuesToList(valores_activos)
      
      # Imprimir la estructura con detalles
      str(valores_activos_list, max.level = 3)
    })
    
    # Reemplazar el renderPrint original con una versión más estética usando renderUI
    output$info_text2 <- renderUI({
      valores_internos_list <- reactiveValuesToList(valores_internos)
      req(valores_internos_list)
      
      req(valores_internos_list$pack_import_dataset)
      #req(valores_internos_list$pack_import_dataset)
      data_source <- valores_internos_list$pack_import_dataset$data_source
      original_file_name <- valores_internos_list$pack_import_dataset$"original_file_name"
      value_ncol <- ncol(valores_internos_list$pack_import_dataset$"database")
      value_nrow <- nrow(valores_internos_list$pack_import_dataset$"database")
      
      req(valores_internos_list$pack_tool_selection)
      selected_tool <-  valores_internos_list$pack_tool_selection$"tipo_modelo"
      acordeon <- valores_internos_list$pack_tool_selection$acordeon
      modelo_seleccionado <- valores_internos_list$pack_tool_selection$modelo_seleccionado
      
      req(valores_internos_list$pack_var_selection)
      value_factor <- valores_internos_list$pack_var_selection$"factor"
      value_rv <-     valores_internos_list$pack_var_selection$"respuesta"
      vector_selected_vars <- valores_internos_list$pack_var_selection$"vector_selected_vars"
      
      
      # Suponemos que estos valores ya están definidos en valores_internos_list
      # data_source <- "ejemplo"
      # original_file_name <- "datos_ventas.csv"
      # value_ncol <- 15
      # value_nrow <- 250
      # selected_tool <- "Regresión"
      # acordeon <- "Panel 2"
      # modelo_seleccionado <- "Regresión Lineal"
      # value_factor <- "Género"
      # value_rv <- "Ingreso"
      # vector_selected_vars <- c("Edad", "Educación", "Experiencia")
      
      # Contenedor principal
      div(
        class = "p-3 rounded shadow-sm",
        style = "background: linear-gradient(to right, #f8f9fa, #ffffff);",
        
        # Título principal
        h4(
          class = "mb-3 pb-2",
          style = "border-bottom: 2px solid #0d6efd; color: #0d6efd;",
          icon("info-circle"), 
          "Resumen de configuración"
        ),
        
        fluidRow(
          column(4,       # Sección de datos
                 div(
                   class = "mb-3 p-2 rounded",
                   style = "background-color: rgba(13, 110, 253, 0.05); border-left: 4px solid #0d6efd;",
                   
                   h5(class = "text-primary", icon("database", class = "me-2"), "Información de datos"),
                   
                   div(class = "d-flex flex-wrap",
                       div(class = "me-4 mb-2",
                           span(class = "fw-bold", "Fuente: "),
                           span(data_source, style = "font-family: monospace;")),
                       
                       div(class = "me-4 mb-2",
                           span(class = "fw-bold", "Archivo: "),
                           span(original_file_name, style = "font-family: monospace;")),
                       
                       div(class = "me-4 mb-2",
                           span(class = "fw-bold", "Dimensiones: "),
                           span(paste0(value_nrow, " filas × ", value_ncol, " columnas"), 
                                style = "font-family: monospace;"))
                   )
                 )
          ),
          column(4,
                 # Sección de herramientas
                 div(
                   class = "mb-3 p-2 rounded",
                   style = "background-color: rgba(25, 135, 84, 0.05); border-left: 4px solid #198754;",
                   
                   h5(class = "text-success", icon("tools", class = "me-2"), "Modelo seleccionado"),
                   
                   div(class = "d-flex flex-wrap",
                       div(class = "me-4 mb-2",
                           span(class = "fw-bold", "Tipo: "),
                           span(selected_tool, style = "font-family: monospace;")),
                       
                       div(class = "me-4 mb-2",
                           span(class = "fw-bold", "Modelo: "),
                           span(modelo_seleccionado, style = "font-family: monospace;")),
                       
                       div(class = "me-4 mb-2",
                           span(class = "fw-bold", "Panel: "),
                           span(acordeon, style = "font-family: monospace;"))
                   )
                 )
          ),
          column(4,
                 # Sección de variables
                 div(
                   class = "p-2 rounded",
                   style = "background-color: rgba(255, 193, 7, 0.05); border-left: 4px solid #ffc107;",
                   
                   h5(class = "text-warning", icon("table-cells", class = "me-2"), "Variables seleccionadas"),
                   
                   div(class = "d-flex flex-wrap",
                       div(class = "me-4 mb-2",
                           span(class = "fw-bold", "Factor: "),
                           span(value_factor, style = "font-family: monospace;")),
                       
                       div(class = "me-4 mb-2",
                           span(class = "fw-bold", "Respuesta: "),
                           span(value_rv, style = "font-family: monospace;"))
                   ),
                   
                   div(
                     class = "mt-2",
                     span(class = "fw-bold", "Variables en el modelo: "),
                     div(
                       class = "mt-1",
                       lapply(vector_selected_vars, function(var) {
                         span(
                           class = "badge bg-light text-dark me-1 mb-1",
                           style = "border: 1px solid #dee2e6; padding: 5px;",
                           var
                         )
                       })
                     )
                   )
                 )
          )
        )
      )
    })
    
    
    output$info_text <- renderPrint({
      valores_internos_list <- reactiveValuesToList(valores_internos)
      req(valores_internos_list)
      
      req(valores_internos_list$pack_import_dataset)
      #req(valores_internos_list$pack_import_dataset)
      data_source <- valores_internos_list$pack_import_dataset$data_source
      original_file_name <- valores_internos_list$pack_import_dataset$"original_file_name"
      value_ncol <- ncol(valores_internos_list$pack_import_dataset$"database")
      value_nrow <- nrow(valores_internos_list$pack_import_dataset$"database")
      
      
      print(paste0("data_source: ", data_source))
      print(paste0("original_file_name: ", original_file_name))
      print(paste0("value_ncol: ", value_ncol))
      print(paste0("value_nrow: ", value_nrow))
      
      req(valores_internos_list$pack_tool_selection)
      selected_tool <-  valores_internos_list$pack_tool_selection$"tipo_modelo"
      acordeon <- valores_internos_list$pack_tool_selection$acordeon
      modelo_seleccionado <- valores_internos_list$pack_tool_selection$modelo_seleccionado
      print(paste0("selected_tool: ", selected_tool))
      print(paste0("acordeon: ", acordeon))
      print(paste0("nodelo_seleccionado: ", modelo_seleccionado))
      
      req(valores_internos_list$pack_var_selection)
      value_factor <- valores_internos_list$pack_var_selection$"factor"
      value_rv <-     valores_internos_list$pack_var_selection$"respuesta"
      vector_selected_vars <- valores_internos_list$pack_var_selection$"vector_selected_vars"
      print(paste0("value_factor: ", value_factor))
      print(paste0("value_rv: ", value_rv))
      print(paste0("vector_selected_vars: ", paste0(vector_selected_vars, collapse =",")))
      
    })
    
    # Mensaje de selección
    output$mensaje_seleccion <- renderUI({
      if (!valores_activos$check_play) {
        div(
          class = "alert alert-info",
          icon("info-circle"),
          " Seleccione datos, luego las variables y presione ",
          tags$strong("PLAY"),
          " para visualizar los resultados."
        )
      } else {
        div(
          class = "alert alert-success",
          icon("check-circle"),
          paste(" Mostrando:", valores_internos$dataset_activo, "- Variables:",
                paste(valores_internos$variables_activas, collapse = ", "))
        )
      }
    })
    if(FALSE){
      quartoRendererServer("quarto_doc", documento = "anova2.qmd")
      # Mostrar el dataframe simple
      output$tabla_datos <- renderTable({
        # Solo mostrar los datos si la selección está activa y hay datos
        req(valores_activos$check_play, !is.null(valores_activos$pack_import_dataset$"database"))
        
        df <- mtcars
        # Devolver los datos con solo las variables seleccionadas
        df <- valores_activos$pack_import_dataset$"database"
        # if (!is.null(valores_internos$variables_activas)) {
        #   if (all(valores_internos$variables_activas %in% names(df))) {
        #     # Para mtcars, incluir los nombres de filas
        #     if (identical(valores_internos$dataset_activo, "mtcars")) {
        #       df <- df[, valores_internos$variables_activas, drop = FALSE]
        #       df <- cbind(car = rownames(df), df)
        #     } else {
        #       df <- df[, c(valores_internos$variables_activas), drop = FALSE]
        #     }
        #   }
        # }
        
        head(df, 10)
      })
    }
    
  })
}
