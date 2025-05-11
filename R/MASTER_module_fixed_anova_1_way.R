
#' @export
MASTER_module_fixed_anova_1_way_ui <- function(id) {
  ns <- NS(id)
  
  # Contenido principal organizado en columnas para mantener los cards separados
  div(
    # Usamos card() para envolver todo el contenido
    card(
      # Añadimos un card_header explícito
      card_header(
        h4("Rscience", class = "btn-sidebar")
      ),
      layout_sidebar(
        sidebar = sidebar(
          p("HOLA", class = "text-center fs-4 fw-bold py-4"),
          open = "closed"
        ),
        # Encabezado con botones en una fila
        div(
          class = "d-flex flex-wrap",
          # Primer bloque - 3/12
          div(
            style = "flex: 0 0 12.5%; max-width: 12.5%;",
            uiOutput(ns("card01_botonera_inicial"))
          ),
          # Segundo bloque - 3/12
          div(
            style = "flex: 0 0 25%; max-width: 25%;",
            uiOutput(ns("card02_user_selection"))
          ),
          # Tercer bloque - 6/12
          div(
            style = "flex: 0 0 62.5%; max-width: 62.5%;",
            uiOutput(ns("card03_botonera_output"))
          )
        )
        ,
        
        uiOutput(ns("mega_tabs")), br(),
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
    )
  )
}

#' @export
MASTER_module_fixed_anova_1_way_server <- function(id, show_dev) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    output$card01_botonera_inicial <- renderUI({
      card(
        card_header("Main menu"),
        card_body(
          div(
              class = "d-flex flex-column align-items-center", # Para centrar horizontalmente
              style = "gap: 20px;",
              Sbutton_01_dataselector_ui(ns("dataset_selector")),
              Sbutton_02_tools_ui(ns("selector_tools")),
              Sbutton_03_variable_selector_ui(ns("selector_variables")),
              Sbutton_play_ui(ns("play_button")),
              Sbutton_reset_ui(ns("reset_button")),
          )
        )
      )
    })
    
    ############################################################################
    output$tarjeta01_dataset <- renderUI({
      valores_internos_list <- reactiveValuesToList(valores_internos)
      req(valores_internos_list)
      
      req(valores_internos_list$pack_import_dataset)
      #req(valores_internos_list$pack_import_dataset)
      data_source <- valores_internos_list$pack_import_dataset$data_source
      original_file_name <- valores_internos_list$pack_import_dataset$"original_file_name"
      value_ncol <- ncol(valores_internos_list$pack_import_dataset$"database")
      value_nrow <- nrow(valores_internos_list$pack_import_dataset$"database")
      
      div(
        class = "mb-3 p-2 rounded",
        style = "background-color: rgba(13, 110, 253, 0.05); border-left: 4px solid #0d6efd;",
        
        h5(class = "text-primary", icon("database", class = "me-2"), "Información de datos"),
        
        div(
          div(
            span(class = "fw-bold", "Source: "),
            span(data_source, style = "font-family: monospace;")
          ),
          div(
            span(class = "fw-bold", "File: "),
            span(original_file_name, style = "font-family: monospace;")
          ),
          div(
            span(class = "fw-bold", "Dimensiones: "),
            span(paste0(value_nrow, " filas × ", value_ncol, " columnas"), 
                 style = "font-family: monospace;")
          )
        )
      )
    })
    
    output$tarjeta02_tools <- renderUI({
      valores_internos_list <- reactiveValuesToList(valores_internos)
      req(valores_internos_list)
      
      req(valores_internos_list$pack_tool_selection)
      selected_tool <-  valores_internos_list$pack_tool_selection$"tipo_modelo"
      acordeon <- valores_internos_list$pack_tool_selection$acordeon
      modelo_seleccionado <- valores_internos_list$pack_tool_selection$modelo_seleccionado
      
      div(
        class = "mb-3 p-2 rounded",
        style = "background-color: rgba(25, 135, 84, 0.05); border-left: 4px solid #198754;",
        
        h5(class = "text-success", icon("tools", class = "me-2"), "Modelo seleccionado"),
        
        div(
          class = "d-flex flex-column",
          div(
            span(class = "fw-bold", "Tipo: "),
            span(selected_tool, style = "font-family: monospace;")
          ),
          div(
            span(class = "fw-bold", "Modelo: "),
            span(modelo_seleccionado, style = "font-family: monospace;")
          ),
          div(
            span(class = "fw-bold", "Panel: "),
            span(acordeon, style = "font-family: monospace;")
          )
        )
        
      )
      
    })
    
    output$tarjeta03_vars <- renderUI({
      valores_internos_list <- reactiveValuesToList(valores_internos)
      req(valores_internos_list)
      
      req(valores_internos_list$pack_var_selection)
      value_factor <- valores_internos_list$pack_var_selection$"factor"
      value_rv <-     valores_internos_list$pack_var_selection$"respuesta"
      vector_selected_vars <- valores_internos_list$pack_var_selection$"vector_selected_vars"
      
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
      
    })
    
    
    output$card02_user_selection <- renderUI({
      valores_internos_list <- reactiveValuesToList(valores_internos)
      req(valores_internos_list)
      
      card(
        card_header("User selection"),
        card_body(
          uiOutput(ns("tarjeta01_dataset")),
          uiOutput(ns("tarjeta02_tools")),
          uiOutput(ns("tarjeta03_vars"))
        )
      )
      
      
      # # Contenedor principal
      # div(
      #   # class = "p-3 rounded shadow-sm",
      #   # style = "background: linear-gradient(to right, #f8f9fa, #ffffff);",
      #   
      #   # Título principal
      #   h4(
      #     # class = "mb-3 pb-2",
      #     # style = "border-bottom: 2px solid #0d6efd; color: #0d6efd;",
      #     # icon("info-circle"), 
      #     "Resumen de configuración"
      #   ),
      #  
      #   
      #   
      # )
      
    })
      
      
    ############################################################################
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
    
    dataset_module  <- Sbutton_01_dataselector_server("dataset_selector", valores_internos, show_dev = show_dev)
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
    
    ############################################################################
    
    
    
    ############################################################################
    # R
    mis_valores <- reactive({
      
      valores_internos_list <- reactiveValuesToList(valores_activos)
      req(valores_internos_list)
      req(valores_internos_list$pack_import_dataset)
      req(valores_internos_list$pack_import_dataset$"database")
      database <- valores_internos_list$pack_import_dataset$"database"
      var_name_factor <- valores_internos_list$pack_var_selection$"factor"
      var_name_vr <- valores_internos_list$pack_var_selection$"respuesta"
      alpha_value <- 0.05
      
      the_results <- GeneralLM_fix_anova1_RCode(database, var_name_factor, var_name_vr, alpha_value)
      vector_order_names <- GeneralLM_fix_anova1_objects_in_order(fn = GeneralLM_fix_anova1_RCode)
      the_results <- the_results[vector_order_names]
      the_results
      
    })
    function_code <- GeneralLM_fix_anova1_take_code(str_fn_name="GeneralLM_fix_anova1_RCode")
    
    crear_outputs_y_ui  <- function(list_objetos, prefix, mis_valores_reactive, output, ns) {
      # Crea los outputs en bucle, en ámbitos independientes para evitar sobrescrituras
      for (i in seq_along(list_objetos)) {
        id_output <- paste0(prefix, i)
        obj_name <- list_objetos[[i]]$"objects"
        
        # Crear un ámbito local para que cada output sea independiente
        local({
          n <- i
          id <- id_output
          obj <- obj_name
          output[[id]] <- renderPrint({
            req(mis_valores_reactive())
            mis_valores_reactive()[obj]
          })
        })
      }
      
      # Crear UI dinámicamente
      ui_list <- lapply(seq_along(list_objetos), function(i) {
        id_output <- paste0(prefix, i)
        list(
          h4(list_objetos[[i]]$"title"),
          verbatimTextOutput(ns(id_output)),
          br()
        )
      })
      
      return(do.call(tagList, ui_list))
    }
    
    crear_outputs_y_ui2 <- function(list_objetos, prefix, mis_valores_reactive, output, ns) {
      # Crear los outputs en ámbitos independientes
      for (i in seq_along(list_objetos)) {
        id_output_table <- paste0(prefix, i, "_table")
        id_output_plot <- paste0(prefix, i, "_plot")
        
        obj_name_table <- list_objetos[[i]]$"table"
        obj_name_plot <- list_objetos[[i]]$"plot"
        
        local({
          n <- i
          id_table <- id_output_table
          id_plot <- id_output_plot
          obj_table <- obj_name_table
          obj_plot <- obj_name_plot
          
          output[[id_plot]] <- plotly::renderPlotly({
            req(mis_valores_reactive())
            mis_valores_reactive()[[obj_plot]]
          })
          
          output[[id_table]] <- renderPrint({
            req(mis_valores_reactive())
            mis_valores_reactive()[[obj_table]]
          })
        })
      }
      
      # Crear UI dinámicamente
      ui_list <- lapply(seq_along(list_objetos), function(i) {
        id_output_table <- paste0(prefix, i, "_table")
        id_output_plot <- paste0(prefix, i, "_plot")
        list(
          fluidRow(
            h4(list_objetos[[i]]$"title"),
            column(6, plotlyOutput(ns(id_output_plot))),
            column(6, verbatimTextOutput(ns(id_output_table)))
          ),
          br(), br(), br()
        )
      })
      
      return(do.call(tagList, ui_list))
    }
    
    ############################################################################
    
    
    
    # Define la información de los botones con grupo y orden
    botones_info <- list( 
      list(id = "boton_1",  label = "Summary",                class = "btn-primary",         grupo = 1, orden = 1),
      list(id = "boton_2",  label = "Full Analysis",          class = "btn-secondary",       grupo = 1, orden = 2),
      list(id = "boton_3",  label = "Descriptive Statistics", class = "btn-success",         grupo = 1, orden = 3),
      list(id = "boton_4",  label = "Script",                 class = "btn-danger",          grupo = 1, orden = 4),
      list(id = "boton_5",  label = "Download",               class = "btn-warning",         grupo = 2, orden = 1),
      list(id = "boton_6",  label = "Hypotheses",             class = "btn-info",            grupo = 3, orden = 1),
      list(id = "boton_7",  label = "Theoretical Framework",  class = "btn-light",           grupo = 3, orden = 2),
      list(id = "boton_8",  label = "Bibliography",           class = "btn-dark",            grupo = 3, orden = 3),
      list(id = "boton_9",  label = "Stock",                  class = NULL,                  grupo = 3, orden = 4),
      list(id = "boton_10", label = "Catastrophic Errors",    class = "btn-outline-primary", grupo = 3, orden = 5),
      list(id = "boton_11", label = "Possible Cases",         class = "btn-outline-primary", grupo = 3, orden = 6),
      list(id = "boton_12", label = "Analysis Structure",     class = "btn-outline-primary", grupo = 3, orden = 7)
    )
    
    # En cuanto a renderUI, primero filtramos por grupo
    output$botones_dinamicos <- renderUI({
      grupos <- c(1, 2, 3)
      ui_list <- lapply(grupos, function(g) {
        # Filtrar y ordenar botones del grupo
        botones_grupo <- Filter(function(b) b$grupo == g, botones_info)
        botones_grupo <- botones_grupo[order(sapply(botones_grupo, function(b) b$orden))]
        
        # Crear los botones
        lista_botones <- lapply(botones_grupo, function(boton) {
          if (!is.null(boton$class)) {
            actionButton(
              inputId = ns(boton$id),
              label = boton$label,
              class = boton$class
            )
          } else {
            actionButton(
              inputId = ns(boton$id),
              label = boton$label
            )
          }
        })
        
        # Crear un div estilo tarjeta
        div(
          style = "
        border: 1px solid #ccc; 
        border-radius: 4px; 
        padding: 10px; 
        margin-bottom: 15px; 
        background-color: #f9f9f9;",
          # Título del grupo
          tags$h3(paste("Grupo", g)),
          div(
            style = "display: flex; flex-wrap: wrap; gap: 10px;",
            lista_botones
          )
        )
      })
      
      do.call(tagList, ui_list)
    })
    
    # Detectar qué botón se ha pulsado y mostrar mensaje
    observe({
      for (boton in botones_info) {
        nombre_interno <- boton$id
        
        if (!is.null(input[[nombre_interno]])) {
          # Usar local para capturar el valor correcto en cada iteración
          local({
            local_boton <- boton
            
            observeEvent(input[[local_boton$id]], {
              output$resultado <- renderText({
                paste("Se ha pulsado el botón:", local_boton$label, 
                      "(ID interno:", local_boton$id, ")",
                      "- Clase:", ifelse(is.null(local_boton$class), "ninguna", local_boton$class),
                      "- Número de clics:", input[[local_boton$id]])
              })
            })
          })
        }
      }
    })
    
    button_info <- reactive({
      # Aquí puedes devolver información sobre botones presionados si lo necesitas
      lapply(botones_info, function(id) input[[id]])
    })
    
    
    output$el_cartel <- renderUI({
      div(
        class = "row mb-4",
        div(
          class = "col-12",
          div(
            style = "background: linear-gradient(90deg, #2C3E50, #4CA1AF); color: white; border-radius: 10px; padding: 20px; box-shadow: 0 4px 10px rgba(0,0,0,0.15);",
            div(
              class = "d-flex justify-content-between align-items-center",
              div(
                h3(icon("edit"), "Editor de Tarjeta", style = "margin: 0; font-weight: 600;"),
                p("Personaliza tu texto y visualízalo en tiempo real", style = "margin: 5px 0 0 0; opacity: 0.9;")
              ),
              div(
                icon("boxes-stacked", style = "font-size: 2.5rem; opacity: 0.8;")
              )
            )
          )
        )
      )
    })
    output$card03_botonera_output <- renderUI({
      div(
        card(
          card_header("Resultado"),
          uiOutput(ns("el_cartel")),
          uiOutput(ns("botones_dinamicos")),
          textOutput(ns("resultado"))
        )
      )
    })
    ############################################################################
    
    
    # Tab01 - Analysis
    output$dynamic_tab01_ui <- renderUI({
      req(mis_valores())
      crear_outputs_y_ui(list_vec01, "render_tab01_", mis_valores, output, ns)
    })
    list_vec01 <- list()
    list_vec01[[1]] <- list("title" = "1) References", "objects" = c("df_selected_vars"))
    list_vec01[[2]] <- list("title" = "2) Factor resumen", "objects" = c("df_factor_info", "check_unbalanced_reps"))
    list_vec01[[3]] <- list("title" = "3) Anova 1 way - Table", "objects" = c("df_table_anova"))
    list_vec01[[4]] <- list("title" = "4) Multiple comparation test (Tukey)", "objects" = c("df_tukey_table"))
    list_vec01[[5]] <- list("title" = "5) Model Error", "objects" = c("df_model_error"))
    
    ############################################################################
    
    # Tab02 - Requeriments
    list_vec02 <- list()
    list_vec02[[1]] <- list("title" = "1) Requeriment - Normaility test - Residuals", 
                            "objects" = c("test_residuals_normality"))
    
    list_vec02[[2]] <- list("title" = "2) Requeriment - Homogeneity test - Residuals", 
                            "objects" = c("test_residuals_homogeneity"))
    list_vec02[[3]] <- list("title" = "3) Estimated variances - Residuals", 
                            "objects" = c("df_residuals_variance_levels"))
    
    
    output$dynamic_tab02_ui <- renderUI({
      req(mis_valores())
      crear_outputs_y_ui(list_vec02, "render_tab02_", mis_valores, output, ns)
    })
    ##############################################################################
    
    # Tab03 - Plots - Raw Data
    list_vec03 <- list()
    list_vec03[[1]] <- list("title" = "", 
                            "table" = "df_table_factor_plot001",
                            "plot"  = "plot001_factor")
    
    list_vec03[[2]] <- list("title" = "", 
                            "table" = "df_table_factor_plot002",
                            "plot"  = "plot002_factor")
    
    list_vec03[[3]] <- list("title" = "", 
                            "table" = "df_table_factor_plot003",
                            "plot"  = "plot003_factor")
    
    list_vec03[[4]] <- list("title" = "", 
                            "table" = "df_table_factor_plot004",
                            "plot"  = "plot004_factor")
    
    list_vec03[[5]] <- list("title" = "", 
                            "table" = "df_table_factor_plot005",
                            "plot"  = "")
    
    list_vec03[[6]] <- list("title" = "", 
                            "table" = "df_table_factor_plot006",
                            "plot"  = "plot005_factor")
    
    list_vec03[[7]] <- list("title" = "", 
                            "table" = "df_table_factor_plot007",
                            "plot"  = "plot005_factor")
    
    output$dynamic_tab03_ui <- renderUI({
      req(mis_valores())
      
      crear_outputs_y_ui2(list_vec03, "render_tab03_", mis_valores, output, ns)
    })
    ##############################################################################
    
    # Tab04 - Plots - Residuals
    list_vec04 <- list()
    list_vec04[[1]] <- list("title" = "", 
                            "table" = "df_table_residuals_plot001",
                            "plot"  = "plot001_residuals")
    
    list_vec04[[2]] <- list("title" = "", 
                            "table" = "df_table_residuals_plot002",
                            "plot"  = "plot002_residuals")
    
    list_vec04[[3]] <- list("title" = "", 
                            "table" = "df_table_residuals_plot003",
                            "plot"  = "plot003_residuals")
    
    list_vec04[[4]] <- list("title" = "", 
                            "table" = "df_table_residuals_plot004",
                            "plot"  = "plot004_residuals")
    
    list_vec04[[5]] <- list("title" = "", 
                            "table" = "df_table_residuals_plot005",
                            "plot"  = "plot005_residuals")
    
    list_vec04[[6]] <- list("title" = "", 
                            "table" = "df_table_residuals_plot006",
                            "plot"  = "plot006_residuals")
    
    list_vec04[[7]] <- list("title" = "", 
                            "table" = "df_table_residuals_plot007",
                            "plot"  = "plot007_residuals")
    
    list_vec04[[8]] <- list("title" = "", 
                            "table" = "df_table_residuals_plot008",
                            "plot"  = "plot008_residuals")
    
    list_vec04[[9]] <- list("title" = "", 
                            "table" = "df_table_residuals_plot009",
                            "plot"  = "plot009_residuals")
    
    list_vec04[[10]] <- list("title" = "", 
                             "table" = "df_table_residuals_plot010",
                             "plot"  = "plot010_residuals")
    
    output$dynamic_tab04_ui <- renderUI({
      req(mis_valores())
      
      crear_outputs_y_ui2(list_vec03, "render_tab04_", mis_valores, output, ns)
    })
    ##############################################################################
    
    # Tab05 - RCode
    
    output$shiny_ace_editor <- renderUI({
      req(mis_valores())
      
      #function_code <- GeneralLM_fix_anova1_take_code(my_fn=GeneralLM_fix_anova1_RCode)
      # Calcular la altura adecuada para el editor basado en el número de líneas
      line_count <- length(strsplit(function_code, "\n")[[1]])
      line_count <- line_count + 5
      # Asignar aproximadamente 20px por línea para el alto del editor
      editor_height <- paste0(max(300, line_count * 20), "px")
      
      card(
        card_header("Editor Options"),
        card_body(
          selectInput(ns("theme"), "Editor Theme:", 
                      choices = c("xcode", "monokai", "github", "eclipse", "tomorrow", 
                                  "solarized_light", "solarized_dark", "textmate", "twilight"),
                      selected = "xcode"),
          sliderInput(ns("fontSize"), "Font Size:", min = 8, max = 24, value = 14, step = 1)
        )
      )
      
      shinyAce::aceEditor(
        outputId = "script_part1",
        value = function_code,
        mode = "r",
        theme = input$"theme", #"chrome",
        height = editor_height,#"200px",
        fontSize = input$"fontSize", #14,
        showLineNumbers = TRUE,
        readOnly = TRUE,
        autoScrollEditorIntoView = TRUE,
        maxLines = 1000,  # Un número grande para evitar scroll
        minLines = line_count 
      )
      
    })
    
    # Función para descargar el código como archivo .R
    output$download_btn <- downloadHandler(
      filename = function() {
        "code_generalLM_fixed_anova_1way.R"
      },
      content = function(file) {
        writeLines(function_code, file)
      }
    )
    
    
    output$dynamic_tab05_ui <- renderUI({
      req(mis_valores())
      
      vector_editor_values <- c("xcode", "monokai", "github", "eclipse", "tomorrow", 
                                "solarized_light", "solarized_dark", "textmate", "twilight")
      
      selected_pos <- 1
      
      card(
        card_header("Editor Options"),
        card_body(
          fluidRow(
            column(3,     
                   selectInput(ns("theme"), "Editor Theme:", 
                               choices = vector_editor_values,
                               selected = vector_editor_values[selected_pos])
            ),
            column(3, 
                   sliderInput(ns("fontSize"), "Font Size:", min = 8, max = 24, value = 14, step = 1)
            ),
            #column(3, actionButton(ns("copy_btn"), "Copiar código", icon = icon("copy"))),
            column(3, downloadButton(ns("download_btn"), "Descargar como .R", icon = icon("download")))
            
          ),
          uiOutput(ns("shiny_ace_editor"))
        )
      )
    })
    
    ############################################################################
    
    # Tab06 - Quarto
    the_quarto_file <- reactive({
      req(mis_valores())
      GeneralLM_fix_anova1_quarto_file_path()
    })
    
    module_quartoRenderer_server(id="quarto_doc", documento = the_quarto_file())
    
    output$dynamic_tab06_ui <- renderUI({
      req(mis_valores())
      module_quartoRenderer_ui(id=ns("quarto_doc"))
    })
    
    ############################################################################
    output$mega_tabs <- renderUI({
      req(mis_valores())
      
      tabsetPanel(
        tabPanel(title = "Analysis",          uiOutput(ns("dynamic_tab01_ui"))),
        tabPanel(title = "Requeriments",      uiOutput(ns("dynamic_tab02_ui"))),
        tabPanel(title = "Plots - Raw Data",  uiOutput(ns("dynamic_tab03_ui"))),
        tabPanel(title = "Plots - Residuals", uiOutput(ns("dynamic_tab04_ui"))),
        tabPanel(title = "RCode",             uiOutput(ns("dynamic_tab05_ui"))),
        tabPanel(title = "Quarto",            uiOutput(ns("dynamic_tab06_ui")))
      )
      
    })
    ############################################################################
    # Shiny Clasic
    
    
    ############################################################################
    output$show_dev_full <- renderUI({
      ns <- NS(id)
      
      req(my_show_dev)
      
      # Fila separada para el contenido en otra tarjeta
      card(
        card_body(
          #fluidRow(uiOutput(ns("info_text2"))),
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
      print(paste0("modelo_seleccionado: ", modelo_seleccionado))
      
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