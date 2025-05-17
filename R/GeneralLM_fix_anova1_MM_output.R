
#' @export
GeneralLM_fix_anova1_MM_output_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("card03_botonera_output"))
  # # Contenido principal organizado en columnas para mantener los cards separados
  # div(
  #   # Usamos card() para envolver todo el contenido
  #   card(
  #     # Añadimos un card_header explícito
  #     card_header(
  #       h4("Rscience", class = "btn-sidebar")
  #     ),
  #     layout_sidebar(
  #       sidebar = sidebar(
  #         p("HOLA", class = "text-center fs-4 fw-bold py-4"),
  #         open = "closed"
  #       ),
  #       # Encabezado con botones en una fila
  #       div(
  #         class = "d-flex flex-wrap",
  #         # Primer bloque - 3/12
  #         div(
  #           style = "flex: 0 0 12.5%; max-width: 12.5%;",
  #           uiOutput(ns("card01_botonera_inicial"))
  #         ),
  #         # Segundo bloque - 3/12
  #         div(
  #           style = "flex: 0 0 25%; max-width: 25%;",
  #           uiOutput(ns("card02_user_selection"))
  #         ),
  #         # Tercer bloque - 6/12
  #         div(
  #           style = "flex: 0 0 62.5%; max-width: 62.5%;",
  #           uiOutput(ns("card03_botonera_output"))
  #         )
  #       )
  #       ,
  #       
  #       #uiOutput(ns("mega_tabs")), br(),
  #       uiOutput(ns("show_dev_full")),
  #       uiOutput(ns("mensaje_seleccion"))
        
        
        
        # Card separado para Quarto
        # div(
        #   style = "margin-top: 20px; width: 100%;",
        #   ""
        #   # quartoRendererUI(id = "quarto_doc")
        # )
 
}

#' @export
GeneralLM_fix_anova1_MM_output_server <- function(id, show_dev) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    
    
    ############################################################################
    # R
    mis_valores <- reactive({
      
      # valores_internos_list <- reactiveValuesToList(valores_activos)
      # req(valores_internos_list)
      # req(valores_internos_list$pack_import_dataset)
      # req(valores_internos_list$pack_import_dataset$"database")
      database <- mtcars #valores_internos_list$pack_import_dataset$"database"
      var_name_factor <- "cyl" #valores_internos_list$pack_var_selection$"factor"
      var_name_vr <- "mpg"     #valores_internos_list$pack_var_selection$"respuesta"
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
      list(id = "boton_1",  label = "Summary",                class = "btn-primary",         grupo = 1, orden = 1, content = "dynamic_tab01_ui", show_always = TRUE),
      list(id = "boton_2",  label = "Full Analysis",          class = "btn-secondary",       grupo = 1, orden = 2, content = "mega_tabs", show_always = TRUE),
      list(id = "boton_3",  label = "Descriptive Statistics", class = "btn-success",         grupo = 1, orden = 3, content = "3", show_always = TRUE),
      list(id = "boton_4",  label = "Script",                 class = "btn-danger",          grupo = 1, orden = 4, content = "dynamic_tab05_ui", show_always = TRUE),
      list(id = "boton_5",  label = "Download",               class = "btn-warning",         grupo = 1, orden = 5, content = "dynamic_tab06_ui", show_always = TRUE),
      list(id = "boton_6",  label = "Hypotheses",             class = "btn-info",            grupo = 2, orden = 1, content = "6", show_always = TRUE),
      list(id = "boton_7",  label = "Theoretical Framework",  class = "btn-light",           grupo = 2, orden = 2, content = "7", show_always = TRUE),
      list(id = "boton_8",  label = "Bibliography",           class = "btn-dark",            grupo = 2, orden = 3, content = "8", show_always = TRUE),
      list(id = "boton_9",  label = "Stock",                  class = NULL,                  grupo = 2, orden = 4, content = "9", show_always = TRUE),
      list(id = "boton_10", label = "Catastrophic Errors",    class = "btn-outline-primary", grupo = 2, orden = 5, content = "10", show_always = TRUE),
      list(id = "boton_11", label = "Possible Cases",         class = "btn-outline-primary", grupo = 2, orden = 6, content = "11", show_always = TRUE),
      list(id = "boton_12", label = "Analysis Structure",     class = "btn-outline-primary", grupo = 2, orden = 7, content = "12", show_always = TRUE)
    )
    
    # En cuanto a renderUI, primero filtramos por grupo
    # Dentro de tu server, después de definir botones_info y el renderUI
    
    # Crear los botones con grupos y orden, y ponerlos en UI
    output$botones_dinamicos <- renderUI({
      
      # valores_internos_list <- reactiveValuesToList(valores_activos)
      # if(is.null(valores_internos_list)) play_ok <- FALSE else  play_ok <- valores_internos_list$check_play
      play_ok <- TRUE
      
      grupos <- c(1, 2)
      ui_list <- lapply(grupos, function(g) {
        botones_grupo <- Filter(function(b) b$grupo == g, botones_info)
        botones_grupo <- botones_grupo[order(sapply(botones_grupo, function(b) b$orden))]
        
        
        lista_botones <- lapply(botones_grupo, function(boton) {
          
          is_disabled <- !boton$show_always
          if(!is_disabled) if(g == 1) is_disabled <- !play_ok
          # is_disabled <- FALSE
          
          if (!is.null(boton$class)) {
            actionButton(
              inputId = ns(boton$id),
              label = boton$label,
              class = boton$class,
              disabled = is_disabled
            )
          } else {
            actionButton(
              inputId = ns(boton$id),
              label = boton$label
            )
          }
        })
        
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
    
    boton_seleccionado <- reactiveVal(NULL)
    
    # Para cada botón, actualizamos la variable
    lapply(botones_info, function(boton) {
      observeEvent(input[[boton$id]], {
        boton_seleccionado(boton)
      })
    })
    
    observeEvent(boton_seleccionado(), {
      output$resultado <- renderText({
        boton <- boton_seleccionado()
        paste(
          "Se ha pulsado el botón:", boton$label,
          "(ID interno:", boton$id, ")",
          "- Clase:", ifelse(is.null(boton$class), "ninguna", boton$class),
          "- Número de clics:", input[[boton$id]]
        )
      })
    })
    
    observeEvent(boton_seleccionado(), {
      output$resultado2 <- renderUI({
        boton <- boton_seleccionado()
        aver <- paste(
          "Se ha pulsado el botón:", boton$label,
          "(ID interno:", boton$id, ")",
          "- Clase:", ifelse(is.null(boton$class), "ninguna", boton$class),
          "- Número de clics:", input[[boton$id]]
        )
        
        showModal(
          modalDialog(
            title = boton$label,
            size = "xl",
            # Aplicamos estilos personalizados para hacer el modal más grande y posicionarlo más arriba
            tags$div(
              tags$style(HTML("
        /* Hacer que el modal sea más grande que xl - ancho y alto */
        .modal-xl {
          max-width: 95% !important; /* Aumentamos el ancho a 95% de la ventana */
          width: 95%;
        }
        
        /* Aumentar la altura del modal y posicionarlo más cerca del borde superior */
        .modal-dialog {
          height: 90vh !important; /* 90% de la altura de la ventana */
          max-height: 90vh !important;
          margin-top: 20px !important; /* Reducimos el margen superior (valor por defecto es 1.75rem ~28px) */
        }
        
        /* Hacer que el contenido del modal ocupe más espacio vertical */
        .modal-content {
          height: 100% !important;
          display: flex;
          flex-direction: column;
        }
        
        /* Ajustar el cuerpo del modal para que ocupe el espacio disponible */
        .modal-body {
          flex: 1;
          overflow: hidden; /* Evita scroll doble */
          padding: 0; /* Quitamos padding para maximizar espacio */
        }
        
        /* Asegurar que en pantallas muy grandes se mantenga un tamaño razonable */
        @media (min-width: 1400px) {
          .modal-xl {
            max-width: 1800px !important; /* O el tamaño máximo que prefieras */
          }
        }
      ")),
            ),
            
            # Contenedor para el módulo de importación - ahora ocupa todo el espacio disponible
            div(
              style = "height: 100%; overflow-y: auto; padding: 15px;", 
              aver,
              uiOutput(ns(boton$content)),
              botones_info[[boton$id]]
            ),
            
            easyClose = TRUE,
            footer = modalButton("Cerrar33")#,
            
            #style = "color: #721c24; background-color: #f8d7da; border-color: #f5c6cb;"
          )
        )
        
      })
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
                h3(icon("edit"), "Anova 1 way - Fixed Effects - General Linear Model", style = "margin: 0; font-weight: 600;"),
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
          textOutput(ns("resultado")),
          uiOutput(ns("resultado2"))
        )
      )
    })
    ############################################################################
    
    list_vec01 <- list()
    list_vec01[[1]] <- list("title" = "1) References", "objects" = c("df_selected_vars"))
    list_vec01[[2]] <- list("title" = "2) Factor resumen", "objects" = c("df_factor_info", "check_unbalanced_reps"))
    list_vec01[[3]] <- list("title" = "3) Anova 1 way - Table", "objects" = c("df_table_anova"))
    list_vec01[[4]] <- list("title" = "4) Multiple comparation test (Tukey)", "objects" = c("df_tukey_table"))
    list_vec01[[5]] <- list("title" = "5) Model Error", "objects" = c("df_model_error"))
    
    # Tab01 - Analysis
    output$dynamic_tab01_ui <- renderUI({
      req(mis_valores())
      crear_outputs_y_ui(list_vec01, "render_tab01_", mis_valores, output, ns)
    })
    
    
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
          
          fluidRow(
            column(3, 
                   selectInput(ns("theme"), "Editor Theme:", 
                               choices = c("xcode", "monokai", "github", "eclipse", "tomorrow", 
                                           "solarized_light", "solarized_dark", "textmate", "twilight"),
                               selected = "solarized_dark")),
            column(3,
                   sliderInput(ns("fontSize"), "Font Size:", min = 8, max = 40, value = 14, step = 1)
            ),
            column(3, downloadButton(ns("download_btn"), "Descargar como .R", icon = icon("download")))
            
          ),
          fluidRow(
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
          )
        )
        
        
        
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
      
      
      uiOutput(ns("shiny_ace_editor"))
      
      
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
        tabPanel(title = "Plots - Residuals", uiOutput(ns("dynamic_tab04_ui")))#,
        #tabPanel(title = "RCode",             uiOutput(ns("dynamic_tab05_ui"))),
        #tabPanel(title = "Quarto",            uiOutput(ns("dynamic_tab06_ui")))
      )
      
    })
    ############################################################################
    # Shiny Clasic
    
    
    ############################################################################
    if(FALSE){
    output$show_dev_full <- renderUI({
      ns <- NS(id)
      
      req(show_dev)
      
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
      req(show_dev)
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