#' @export
Sbutton_03_settings_ui <- function(id) {
  ns <- NS(id)
  
  # Botón para elegir variables
  
  uiOutput(ns("my_action_button"))
  
  
  
  
}


#' @export
Sbutton_03_settings_server <- function(id,
                                       step_pos, 
                                       number_current_step, 
                                       internal_DATASET_SELECTOR, internal_TOOLS_SELECTOR, 
                                       internal_CFG, internal_SETTINGS) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    #---------------------------------------------------------------------------
    # UI BUTTON - state button entails a specific color
    button_state <- reactiveVal(NULL)
    observe({button_state(internal_SETTINGS$"button_state")})
    
    output$my_action_button <- renderUI({
      
      btn_class <- fn_R_switch_class_from_button_state(button_state = button_state())
      
      
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
    
    output$the_cartel <- renderUI({
      the_cartel <- internal_TOOLS_SELECTOR$"pack_output"$"selected_cartel"
      fn_html_cartel(my_text = the_cartel)
    })
    
    
    
    #
    #---------------------------------------------------------------------------
    # Local OK
    local_OK <- reactive({
      
      print(internal_CFG$"check_output")
      print("FFF")
      vector_all <- c(isTruthy(internal_DATASET_SELECTOR$"check_output"),
                      isTruthy(internal_TOOLS_SELECTOR$"check_output"),
                      isTruthy(internal_CFG$"check_output")
      )

      
      the_final <- all(vector_all)
      the_final
    })
    #
    #---------------------------------------------------------------------------
    ### Initialization objects
    my_str_MM_server <- reactiveVal(NULL)
    my_str_MM_ui <- reactiveVal(NULL)
    my_str_FN <- reactiveVal(NULL)
    local_id <- reactiveVal(NULL)
    
    ### And for module output
    output_list_variable_selector_rv <- reactiveVal(NULL)
    module_uiOutput_settings <- reactiveVal(NULL)
    
    ### Finding modules
    observe({
      req(local_OK())

      sub_list_settings <- internal_CFG$"pack_output"$"settings"

      # ------------------------------------------------------------------------
      # Hardcoded
      str_local_id  <- "the_settings"
      # ------------------------------------------------------------------------
      # Preparing delivery
      full_name_MM_server <- sub_list_settings$"module_server"
      full_name_MM_ui     <- sub_list_settings$"module_ui"
      full_name_FN        <- sub_list_settings$"fn_control"
      # ------------------------------------------------------------------------
      # Filling ReactiveValues
      isolate({
        my_str_MM_server(full_name_MM_server)
        my_str_MM_ui(full_name_MM_ui)
        my_str_FN(full_name_FN)
        local_id(str_local_id)
      })
      # ------------------------------------------------------------------------
      
    })
    
    #---------------------------------------------------------------------------
    
    # Building modules, ui and server
    observe({
      req(local_OK(), my_str_MM_server(), my_str_MM_ui(), local_id())
      
      #-------------------------------------------------------------------------
      # Basics
      my_dataset <- internal_DATASET_SELECTOR$"pack_output"$"database"
      
      #
      #-------------------------------------------------------------------------
      # Running server module
      ### Ui execution and taking return (uiOutput offering settings to the user)
      args_ui <- list(id = ns(local_id()))
      the_ui_output <- do.call(my_str_MM_ui(), args_ui)
      module_uiOutput_settings(the_ui_output)
      
      ### Server execution and taking return (the user selections for settings!!!)
      args_server <- list(id = local_id(), my_dataset = my_dataset)
      the_output_server <- do.call(my_str_MM_server(), args_server)
      
      print("the output")
      print(the_output_server)
      print("the output end")
      
      output_list_variable_selector_rv(the_output_server)
      
      #
      #-------------------------------------------------------------------------
    })
    
    # Menu show output
    output$menu_show_output <- renderUI({
      req(local_OK(), module_uiOutput_settings())

      module_uiOutput_settings()
      
     
    })
    
    
    #---------------------------------------------------------------------------
    
    
    # Selección de variables
    observeEvent(input$btn_variables, {
      
      if (!internal_DATASET_SELECTOR$"check_output") {
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
      
      
       # Instrucciones
      the_title <-   div(
                      class = "alert alert-info",
                      "Settings..."
                      )
      
      #columnas <- colnames(valores_internos$pack_import_dataset$"database")
      base_name <- internal_DATASET_SELECTOR$"pack_output"$"original_file_name"
      
      showModal(modalDialog(
        title = "",
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
        # div(
        #   class = "alert alert-info",
        #   "Por favor, seleccione exactamente 2 variables."
        # ),
        div(uiOutput(ns("the_cartel"))),
        the_title,
        div(
          style = "height: 100%; overflow-y: auto; padding: 15px;", 
          uiOutput(ns("menu_show_output"))
        ),
        footer = tags$div(
          style = "display: flex; justify-content: center; width: 100%; gap: 10px;",
          # Botón Cancelar de ancho completo
          tags$button(
            id = ns("btn_cancel"),
            type = "button",
            class = "btn btn-default",
            style = "width: 50%; height: 45px;", # Aumentado la altura
            "data-bs-dismiss" = "modal",
            "CANCEL"
          ),
          actionButton(inputId = ns("confirm_action"), label = "ADD", 
                       class = "btn-primary", style = "width: 100%; height: 45px;") # Aumentado la altura
          
        )
      ))
    })
    
    
    # Al confirmar selección de variables
    observeEvent(input$confirm_action, {
      
      if (is.null(output_list_variable_selector_rv)) {
        # print(output_list_variable_selector_rv)
        showNotification(
          "Please, select variables.",
          type = "warning"
        )
        return()  # No hacer nada si no se ha seleccionado una base de datos
      }
      
      
      variables_seleccionadas <- output_list_variable_selector_rv()
      # print(variables_seleccionadas)
      # args <- list(output_list_variable_selector_rv = output_list_variable_selector_rv)
      # # print(str_02_FN_validate_vars())
      # resultado <- do.call(my_str_FN(), args)
      # resultado
      #   
      # # resultado <- GeneralLM_fix_anova1_FN_validate_vars(output_list_variable_selector_rv = output_list_variable_selector_rv)
      # 
      # if (!resultado$status) {
      #   showNotification(resultado$message, type = "warning")
      #   return()
      # }
      
    
      
      fn_shiny_apply_changes_reactiveValues(rv = internal_SETTINGS, list(
        "pack_output"  = variables_seleccionadas(),
        "check_output" = TRUE,
        "button_state" = "confirmed"))
      

      removeModal()
    })
    
    return(NULL)
   
  })
}
