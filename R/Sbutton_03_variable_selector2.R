#' @export
Sbutton_03_variable_selector2_ui <- function(id) {
  ns <- NS(id)
  
  # Botón para elegir variables
  
  uiOutput(ns("my_action_button"))
  
  
  
  
}


#' @export
Sbutton_03_variable_selector2_server <- function(id, 
                                                 internal_DATASET_SELECTOR, 
                                                 internal_TOOLS_SELECTOR, 
                                                 internal_STR, 
                                                 internal_VARIABLE_SELECTOR) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    # UI BUTTON
    button_state <- reactiveVal(NULL)
    observe({
      button_state(internal_VARIABLE_SELECTOR$"button_class")
     })
    
    output$my_action_button <- renderUI({
      # req(local_OK())
      # btn_class <- switch(button_state(),
      #                     "initial"   = "btn-primary",    # Azul inicial
      #                     "confirmed" = "btn-success",    # Verde después de confirmar
      #                     "modified"  = "btn-primary")    # Vuelve a azul si se modifica
      
      btn_class <- switch(button_state(),
                          "initial"   = "btn-outline-primary",    # Azul inicial
                          "confirmed" = "btn-outline-success",    # Verde después de confirmar
                          "modified"  = "btn-outline-primary")    # Vuelve a azul si se modifica
      # Botón para elegir variables
      actionButton(
        ns("btn_variables"),
        HTML(paste0('<i class="fa fa-sliders" style="font-size: 75px; display: block; margin-bottom: 8px;"></i>', 
                    '<span></span>')),
        class = btn_class, 
        style = "height: 100px; width: 140px; display: flex; flex-direction: column; justify-content: center; align-items: center; font-size: 14px;",
        title = "Variables Selection"
      )
    })
    
    #---------------------------------------------------------------------------
    
    
    
    
    
    # Button actions
    local_OK <- reactive({
      vector_all <- c(internal_STR$"check_output",
                      internal_TOOLS_SELECTOR$"check_output")
      
      the_final <- all(vector_all)
      the_final
    })
    
    my_str_MM_server <- reactiveVal(NULL)
    my_str_MM_ui <- reactiveVal(NULL)
    my_str_FN <- reactiveVal(NULL)
    local_id <- reactiveVal(NULL)
    output_list_variable_selector_rv <- reactiveVal(NULL)
    
    observe({
      req(local_OK())

      mi_super_lista <- reactiveValuesToList(internal_STR) #print(paste0("AVER: ", internal_STR$"pack_output"$"vector_str"$"str_01_MM_variable_selector"))
      
      # Hardcoded --------------------------------------------------------------
      my_df <- mi_super_lista$"pack_output"$"df_01_settings"
      vector_short_names  <- my_df$"short_name"
      vector_full_names   <- my_df$"resource_name"
      str_local_id  <- "the_settings"
      str_MM_server <- "MM_server"
      str_MM_ui     <- "MM_ui"
      str_FN        <- "FN_validate_vars"
      # ------------------------------------------------------------------------
      
      # MM server
      dt_str_MM_server    <- vector_short_names == str_MM_server
      full_name_MM_server <- vector_full_names[dt_str_MM_server]
      # print(full_name_MM_server)
      
      # MM ui
      dt_str_MM_ui    <- vector_short_names == str_MM_ui
      full_name_MM_ui <- vector_full_names[dt_str_MM_ui]
      # print(full_name_MM_ui)
      
      # FN
      dt_str_FN    <- vector_short_names == str_FN
      full_name_FN <- vector_full_names[dt_str_FN]
      
      # Filling ReactiveValues
      my_str_MM_server(full_name_MM_server)
      my_str_MM_ui(full_name_MM_ui)
      my_str_FN(full_name_FN)
      local_id(str_local_id)
    })
    

    
    # Usar str_01_MM_variable_selector dinámicamente
    observe({
      req(local_OK())
      req(my_str_MM_server())
      req(internal_DATASET_SELECTOR$"pack_output")
      req(internal_DATASET_SELECTOR$"pack_output"$"database")
      
      my_dataset <- internal_DATASET_SELECTOR$"pack_output"$"database"
      
      args <- list(id = "the_selection", 
                   my_dataset = my_dataset)

      # print(my_str_MM_server())
      resultado <- do.call(my_str_MM_server(), args)
      output_list_variable_selector_rv(resultado)
      
    
    })
    
    # Renderizar la UI del selector de variables
    output$menu_show_output <- renderUI({
      req(local_OK())
      # print(local_OK())
      
      req(my_str_MM_ui())
      # req(my_str_MM_ui())  # Asegurarse de que str_01_MM_variable_selector tenga un valor
      # print(my_str_MM_ui())
      
      # str_ui(new_ui)
      args <- list(id = ns("the_selection"))
      resultado <- do.call(my_str_MM_ui(), args)
      resultado
      
     
    })
    
    
    #---------------------------------------------------------------------------
    
    
    # Selección de variables
    observeEvent(input$btn_variables, {
      
      if (!internal_DATASET_SELECTOR$check_output) {
        showNotification(
          "Please, select a dataset first!",
          type = "warning"
        )
        return(NULL)  # No hacer nada si no se ha seleccionado una base de datos
      }
      
      if (!internal_TOOLS_SELECTOR$check_output) {
        showNotification(
          "Please, select a statistic tool!",
          type = "warning"
        )
        return(NULL)  # No hacer nada si no se ha seleccionado una base de datos
      }
      
      
     
      
      #columnas <- colnames(valores_internos$pack_import_dataset$"database")
      base_name <- internal_DATASET_SELECTOR$"pack_output"$"original_file_name"
      
      showModal(modalDialog(
        title = paste("Seleccionar variables para", base_name),
        size = "xl",
        easyClose = TRUE,
        
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
        
        # Instrucciones
        div(
          class = "alert alert-info",
          "Por favor, seleccione exactamente 2 variables."
        ),
        div(
          style = "height: 100%; overflow-y: auto; padding: 15px;", 
          uiOutput(ns("menu_show_output"))
        ),
        footer = tags$div(
          style = "display: flex; justify-content: center; width: 100%; gap: 10px;",
          # Botón Cancelar de ancho completo
          tags$button(
            id = ns("btn_cancelar"),
            type = "button",
            class = "btn btn-default",
            style = "width: 50%; height: 45px;", # Aumentado la altura
            "data-bs-dismiss" = "modal",
            "CANCEL"
          ),
          actionButton(inputId = ns("confirmar_variables"), label = "ADD", 
                       class = "btn-primary", style = "width: 100%; height: 45px;") # Aumentado la altura
          
        )
      ))
    })
    
    
    # Al confirmar selección de variables
    observeEvent(input$confirmar_variables, {
      
      if (is.null(output_list_variable_selector_rv)) {
        # print(output_list_variable_selector_rv)
        showNotification(
          "Please, select variables.",
          type = "warning"
        )
        return()  # No hacer nada si no se ha seleccionado una base de datos
      }
      
      
      # variables_seleccionadas <- output_list_variable_selector_rv()
      
      args <- list(output_list_variable_selector_rv = output_list_variable_selector_rv)
      # print(str_02_FN_validate_vars())
      resultado <- do.call(my_str_FN(), args)
      resultado
        
      # resultado <- GeneralLM_fix_anova1_FN_validate_vars(output_list_variable_selector_rv = output_list_variable_selector_rv)
      
      if (!resultado$status) {
        showNotification(resultado$message, type = "warning")
        return()
      }
      
    
      
      fn_shiny_apply_changes_reactiveValues(rv = internal_VARIABLE_SELECTOR, list(
        "pack_input"   = resultado$output_list,
        "check_input"  = resultado$status,
        "pack_output"  = resultado$output_list,
        "check_output" = resultado$status,
        "button_class" = "confirmed"))
      

      removeModal()
    })
    
    return(NULL)
   
  })
}
