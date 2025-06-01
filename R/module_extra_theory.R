
#' @export
module_extra_theory_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("SUPER_OUTPUT"))
  
  
}

#' @export
module_extra_theory_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    OK_ALL_ACTIVE <- reactive({
      req(mis_valores())
      
      return(TRUE)
      
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
    
     
    # Define la información de los botones con grupo y orden
    botones_info <- list( 
      list(id = "boton_1",  label = "Summary",                class = "btn-outline-primary", grupo = 1, orden = 1, content = "1",  show_always = TRUE),
      list(id = "boton_2",  label = "Full Analysis",          class = "btn-outline-primary", grupo = 1, orden = 2, content = "2",  show_always = TRUE),
      list(id = "boton_3",  label = "Descriptive Statistics", class = "btn-outline-primary", grupo = 1, orden = 3, content = "3",  show_always = TRUE),
      list(id = "boton_4",  label = "Script",                 class = "btn-outline-primary", grupo = 1, orden = 4, content = "4",  show_always = TRUE),
      list(id = "boton_5",  label = "Download",               class = "btn-warning",         grupo = 1, orden = 5, content = "5",  show_always = TRUE),
      list(id = "boton_6",  label = "Hypotheses",             class = "btn-info",            grupo = 2, orden = 1, content = "6",  show_always = TRUE),
      list(id = "boton_7",  label = "Theoretical Framework",  class = "btn-light",           grupo = 2, orden = 2, content = "7",  show_always = TRUE),
      list(id = "boton_8",  label = "Bibliography",           class = "btn-dark",            grupo = 2, orden = 3, content = "8",  show_always = TRUE),
      list(id = "boton_9",  label = "Stock",                  class = NULL,                  grupo = 2, orden = 4, content = "9",  show_always = TRUE),
      list(id = "boton_10", label = "Catastrophic Errors",    class = "btn-outline-primary", grupo = 2, orden = 5, content = "10", show_always = TRUE),
      list(id = "boton_11", label = "Possible Cases",         class = "btn-outline-primary", grupo = 2, orden = 6, content = "11", show_always = TRUE),
      list(id = "boton_12", label = "Analysis Structure",     class = "btn-outline-primary", grupo = 2, orden = 7, content = "12", show_always = TRUE)
    )
    

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

    
  
    
    
   
    
    
  })
}