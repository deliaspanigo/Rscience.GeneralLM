
#' @export
GeneralLM_fix_anova1_MM_output_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("SUPER_OUTPUT"))
  
 
}

#' @export
GeneralLM_fix_anova1_MM_output_server <- function(id, show_dev, 
                                                  active_DATASET_SELECTOR, 
                                                  active_TOOLS_SELECTOR,
                                                  active_VARIABLE_SELECTOR,
                                                  active_PLAY_SELECTOR) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    OK_ALL_ACTIVE <- reactive({
      req(active_DATASET_SELECTOR, active_TOOLS_SELECTOR, 
          active_VARIABLE_SELECTOR, active_PLAY_SELECTOR)
      
      req(active_DATASET_SELECTOR$"check_output", 
          active_TOOLS_SELECTOR$"check_output",
          active_VARIABLE_SELECTOR$"check_output",
          active_PLAY_SELECTOR$"check_output")
      
      return(TRUE)
      
    })
    
    ############################################################################
    # 1) R Code
    Rcode_original <- reactive({
      req(OK_ALL_ACTIVE())
      
      the_code   <- GeneralLM_fix_anova1_take_code(str_fn_name="GeneralLM_fix_anova1_RCode")
      the_code
    })
    
    Rcode_script   <- reactive({
      req(OK_ALL_ACTIVE())
      
      the_code   <- Rcode_original()
      str_import <- active_DATASET_SELECTOR$"pack_output"$"str_import_external"
      the_code   <- sub(pattern = "_my_import_sentence_", replacement = str_import, x = the_code)
      the_code   <- gsub(pattern = "#---", replacement = "", x = the_code)
      the_code   <- gsub(pattern = "#---", replacement = "", x = the_code)
      
      the_code   <- gsub(pattern = "#---", replacement = "", x = the_code)
      
      the_code
    })
    
    Rcode_quarto <- Rcode_script   <- reactive({
      req(OK_ALL_ACTIVE(), Rcode_original())
      
      str_import      <- active_DATASET_SELECTOR$"pack_output"$"str_import_external"
      var_name_factor <- active_VARIABLE_SELECTOR$"pack_output"$"factor" #valores_internos_list$pack_var_selection$"factor"
      var_name_vr     <- active_VARIABLE_SELECTOR$"pack_output"$"respuesta"  #     #valores_internos_list$pack_var_selection$"respuesta"
      alpha_value     <- 0.05
      
      the_code   <- Rcode_original()
      the_code   <- sub(pattern = "_my_import_sentence_", replacement = str_import, x = the_code)
      the_code   <- gsub(pattern = "#---", replacement = "", x = the_code)
      the_code   <- sub(pattern = "_var_name_factor_", replacement = var_name_factor, x = the_code)
      the_code   <- sub(pattern = "_var_name_vr_", replacement = var_name_vr, x = the_code)
      the_code   <- sub(pattern = "_alpha_value_", replacement = alpha_value, x = the_code)
      the_code
      
    })
    
    ############################################################################
    
    # 2) R - Results
    mis_valores <- reactive({
      
      req(OK_ALL_ACTIVE())
      
      database        <- active_DATASET_SELECTOR$"pack_output"$"database" # mtcars #valores_internos_list$pack_import_dataset$"database"
      var_name_factor <- active_VARIABLE_SELECTOR$"pack_output"$"factor" #valores_internos_list$pack_var_selection$"factor"
      var_name_vr     <- active_VARIABLE_SELECTOR$"pack_output"$"respuesta"  #     #valores_internos_list$pack_var_selection$"respuesta"
      alpha_value     <- 0.05
      
      the_results        <- GeneralLM_fix_anova1_RCode(database, var_name_factor, var_name_vr, alpha_value)
      vector_order_names <- GeneralLM_fix_anova1_objects_in_order(fn = GeneralLM_fix_anova1_RCode)
      the_results        <- the_results[vector_order_names]
      the_results
      
    })
    
    ############################################################################
    
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
            mis_valores_reactive()[obj_table]
          })
        })
      }
      
      # Crear UI dinámicamente
      ui_list <- lapply(seq_along(list_objetos), function(i) {
        
        obj_name_plot <- list_objetos[[i]]$"plot"
        id_output_table <- paste0(prefix, i, "_table")
        id_output_plot <- paste0(prefix, i, "_plot")
        list(
          h4(list_objetos[[i]]$"title"),
          HTML(paste0("<b><u>R plot object:</u></b> ", obj_name_plot)),
          fluidRow(
            column(6, plotlyOutput(ns(id_output_plot))),
            column(6, verbatimTextOutput(ns(id_output_table)))
          ),
          hr(),
          br(), br(), br()
        )
      })
      
      return(do.call(tagList, ui_list))
    }
    
    ############################################################################
    
    output$df_summary_anova <- DT::renderDataTable({
      req(mis_valores())
      tabla_p <- mis_valores()$"df_summary_anova"
      validate(need(!is.null(tabla_p), "No hay datos disponibles."))
      datatable(
        tabla_p,
        rownames = FALSE,
        editable = FALSE,
        options = list(
          dom = 't',
          ordering = FALSE,
          paging = FALSE,
          info = FALSE,
          autoWidth = TRUE
        ),
        class = 'stripe hover compact'
      ) %>% formatStyle(
        columns = names(tabla_p),
        target = 'cell',
        backgroundColor = 'white'
      )
    })
    
    output$the_plot <- renderPlotly({
      req(mis_valores())
      my_plot <- mis_valores()$"plot007_factor"
      my_plot
    })
    
    output$mini_resumen <- renderUI({
      req(mis_valores())
      
      div(
        class = "d-flex",  # Contenedor flexible
        style = "height: 90%",  # Altura del contenedor principal (80% de la ventana)
        div(
            style = "flex: 0 0 50%; max-width: 50%; padding: 10px; height: 50%;",
        card(
          card_header("Summary anova"),
          card_body(
            div(
              DTOutput(ns("df_summary_anova"))
            )
          )
        ),
        card(
          card_header("Interpretation"),
          card_body(
            div(
              div(
                tags$div(mis_valores()$"phrase_shapiro_selected"),
                tags$div(mis_valores()$"phrase_bartlett_selected"),
                tags$div(mis_valores()$"phrase_requeriments_selected"),
                tags$div(mis_valores()$"phrase_anova_selected")
              )
            )
          )
        )
        ),
        div(
          style = "flex: 0 0 50%; max-width: 50%; padding: 10px; height: 50%;",
        card(
          card_header("Plot"),
          card_body(
            div(
              plotlyOutput(ns("the_plot"))
            )
          )
        )
        )
      )
      
      
      
      
    })
    
    ############################################################################
    
    # Define la información de los botones con grupo y orden
    botones_info <- list( 
      list(id = "boton_1",  label = "Summary",                class = "btn-outline-primary",         grupo = 1, orden = 1, content = "mini_resumen", show_always = TRUE),
      list(id = "boton_2",  label = "Full Analysis",          class = "btn-outline-primary",       grupo = 1, orden = 2, content = "mega_tabs", show_always = TRUE),
      list(id = "boton_3",  label = "Descriptive Statistics", class = "btn-outline-primary",         grupo = 1, orden = 3, content = "3", show_always = TRUE),
      list(id = "boton_4",  label = "Script",                 class = "btn-outline-primary",          grupo = 1, orden = 4, content = "shiny_ace_editor_OUTPUT", show_always = TRUE),
      list(id = "boton_5",  label = "Download",               class = "btn-warning",         grupo = 1, orden = 5, content = "dynamic_download_quarto", show_always = TRUE),
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
          max-width: 95% !important; /* Aumentamos el ancho al 95% */
          width: 95%;
        }
        
        /* Aumentar la altura del modal y posicionarlo más cerca del borde superior */
        .modal-dialog {
          height: 90vh !important; /* 90% de la altura de la ventana */
          max-height: 90vh !important;
          margin-bottom: 20px !important; /* margen inferior */
          margin-top: 20px !important; /* margen superior */
        }
        
        /* Hacer que el contenido del modal ocupe más espacio verticalmente */
        .modal-content {
          height: 100% !important;
          display: flex;
          flex-direction: column;
        }
        
        /* Ajustar el cuerpo del modal para que permita scroll y ocupe espacio disponible */
        .modal-body {
          flex: 1;
          overflow-y: auto; /* scroll vertical cuando sea necesario */
          padding: 15px; /* ajuste de padding si prefieres */
        }
        
        /* Asegura que en pantallas muy grandes se mantenga un tamaño razonable */
        @media (min-width: 1400px) {
          .modal-xl {
            max-width: 1800px !important; /* tamaño máximo en pantallas muy grandes */
          }
        }
        "))
              ,
            ),
            
            # Contenedor para el módulo de importación - ahora ocupa todo el espacio disponible
            div(
              # class = "d-flex",  # Contenedor flexible
              style = "overflow-y: auto; padding: 15px; height: 95%", 
              # style = "height: 100%; overflow-y: auto; padding: 15px;",
              fluidRow(
                column(10, fn_html_cartel(my_text = "Anova 1 way - Fixed Effects - General Linear Model")),
                column(2, aver)
                ),
              uiOutput(ns(boton$content)),
              botones_info[[boton$id]]
            ),
            
            easyClose = TRUE,
            footer = modalButton("Cerrar")#,
            
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
      fn_html_cartel(my_text = "Anova 1 way - Fixed Effects - General Linear Model")
    })
    # style = "height: 100%; width: 100%; max-width: 100%; box-sizing: border-box; overflow-x: hidden;",  # Ajustes para evitar el scroll horizontal
    
    output$SUPER_OUTPUT <- renderUI({
      div(
      # style = "",  # Altura del contenedor (100% del contenedor padre)
        # card(
          # style = "height: 100%; display: flex; flex-direction: column;",  # Altura de la card (100% del contenedor padre)
          # card_header("Resultado"),
          # div(
            style = "flex-grow: 1; overflow-y: auto; height: 100%; width: 100%; max-width: 100%; box-sizing: border-box; overflow-x: hidden;",  # Ocupa el espacio restante y añade scroll si es necesario
            uiOutput(ns("el_cartel")),
            # uiOutput(ns("mini_resumen")),
            uiOutput(ns("botones_dinamicos")),
            textOutput(ns("resultado")),
            uiOutput(ns("resultado2"))
          # )
        )
      # )
      
    })
    ############################################################################
    
    list_vec01 <- list()
    list_vec01[[1]] <- list("title" = "1) References", "objects" = c("df_selected_vars"))
    list_vec01[[2]] <- list("title" = "2) Factor resumen", "objects" = c("df_factor_info", "check_unbalanced_reps", "phrase_selected_unbalanced"))
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
                            "plot"  = "plot005_factor")
    
    list_vec03[[6]] <- list("title" = "", 
                            "table" = "df_table_factor_plot006",
                            "plot"  = "plot006_factor")
    
    list_vec03[[7]] <- list("title" = "", 
                            "table" = "df_table_factor_plot007",
                            "plot"  = "plot007_factor")
    
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
    
    # Tab05 - Summary Special
    list_vec05 <- list()
    list_vec05[[1]] <- list("title" = "1) Summary Anova", 
                            "objects" = c("df_summary_anova", "phrase_shapiro_selected",
                                          "phrase_bartlett_selected", "phrase_requeriments_selected",
                                          "phrase_anova_selected"))
    

    output$dynamic_ESPECIAL_ui <- renderUI({
      req(mis_valores())
      crear_outputs_y_ui(list_vec05, "render_tab05_", mis_valores, output, ns)
    })
    
    ##############################################################################
    # Tab05 - RCode
    
    output$shiny_ace_editor_MENU <- renderUI({
      req(mis_valores())
      
      #Rcode_script <- GeneralLM_fix_anova1_take_code(my_fn=GeneralLM_fix_anova1_RCode)
      # Calcular la altura adecuada para el editor basado en el número de líneas

      
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
            
          )
        )
      )
      
    })
    
    output$shiny_ace_CODE <- renderUI({
      req(mis_valores(), Rcode_script(), input$"theme", input$"fontSize")
      
      
      line_count <- length(strsplit(Rcode_script(), "\n")[[1]])
      line_count <- line_count + 5
      # Asignar aproximadamente 20px por línea para el alto del editor
      editor_height <- paste0(max(300, line_count * 20), "px")
      
            shinyAce::aceEditor(
              outputId = "script_part1",
              value = Rcode_script(),
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
    
    output$shiny_ace_editor_OUTPUT <- renderUI({
      req(mis_valores())
      
     div(uiOutput(ns("shiny_ace_editor_MENU")),
         uiOutput(ns("shiny_ace_CODE")))
      
    })
    
    # Función para descargar el código como archivo .R
    output$download_btn <- downloadHandler(
      filename = function() {
        "code_generalLM_fixed_anova_1way.R"
      },
      content = function(file) {
        writeLines(Rcode_script(), file)
      }
    )
    
    
   
    
    ############################################################################
    
    # Tab06 - Quarto
    the_quarto_file_path <- reactive({
      req(mis_valores())
      GeneralLM_fix_anova1_quarto_file_path()
    })
    
    module_quartoRenderer_server(id="quarto_doc", documento = the_quarto_file_path(), 
                                 Rcode_script = Rcode_script)
    
    output$dynamic_download_quarto <- renderUI({
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
        tabPanel(title = "Summary",           uiOutput(ns("dynamic_ESPECIAL_ui")))
      )
      
    })
    ############################################################################

    
    

  })
}