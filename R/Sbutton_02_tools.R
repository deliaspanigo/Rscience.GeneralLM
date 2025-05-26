#' @export
Sbutton_02_tools_ui <- function(id) {
  ns <- NS(id)
  

  uiOutput(ns("my_action_button"))
  
  
  
  
}

#' @export
Sbutton_02_tools_server <- function(id, step_pos, number_current_step, internal_DATASET_SELECTOR, internal_TOOLS_SELECTOR){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    button_state <- reactiveVal()
    
    observe({ 
      button_state(internal_TOOLS_SELECTOR$"button_state")
    })
    
    output$my_action_button <- renderUI({
      
      btn_class <- fn_R_switch_class_from_button_state(button_state = button_state())
      
      # Botón para elegir variables
      actionButton(
        ns("btn_tools"),
        HTML(paste0('<i class="fa fa-hammer" style="font-size: 75px; display: block; margin-bottom: 8px; transform: scaleX(-1);"></i>', 
                    '<span></span>')),
        class = btn_class, 
        style = "height: 100px; width: 140px; display: flex; flex-direction: column; justify-content: center; align-items: center; font-size: 14px;",
        title = "Statistic tools"
      )
    })
    
    
    # Module - Server - Button - Tools
    list_user_tool_selection <- Rscience.menu::MASTER_module_tools_server(id ="module_tools")

    check_selected_tool <- reactive({
      list_user_tool_selection()$"check_selected_tool"
      
    })
    
    # observe(print(list_user_tool_selection()))
     
    # Selección de variables
    observeEvent(input$btn_tools, {
      
      # Verificar que se haya seleccionado un dataset primero
      if (!internal_DATASET_SELECTOR$"check_output") {
        showNotification(
          "Please, select your dataset!",
          type = "warning"
        )
        return(NULL)  # No hacer nada si no se ha seleccionado una base de datos
      }
      
     
      # Si ya hay un dataset elegido, mostramos ahora si las opciones para elegir
      # una herramienta estadistica.
      
      # general_topic_name <- "Modelos Lineales Generales"
      showModal(
        modalDialog(
          title = paste("Tools"),
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
            "Please, choose 1 tool."
          ),
          div(
            style = "height: 100%; overflow-y: auto; padding: 15px;", 
            # uiOutput(ns("MEGA_MENU")),
            # SSelector_02_tools_ui(ns("seleccion_modelo"))
            Rscience.menu::MASTER_module_tools_ui(id = ns("module_tools"))
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
            actionButton(inputId = ns("confirmar_tools"), label = "ADD", 
                         class = "btn-primary", style = "width: 100%; height: 45px;") # Aumentado la altura
            
          )
          
        ))
    })
    
    # Al confirmar selección de variables
    observeEvent(input$confirmar_tools, {
      
      # Verificar que se haya seleccionado un dataset primero
      if (!check_selected_tool()) {
        showNotification(
          "Please, select your tool!",
          type = "warning"
        )
        return(NULL)  # No hacer nada si no se ha seleccionado una base de datos
      }
      
      # # Verificar que se haya seleccionado una opción válida
      # if (!valores_internos$check_import_dataset) {
      #   showNotification("Por favor, seleccione una base de datos.", type = "warning")
      #   return()  # No hacer nada si no se seleccionó nada
      # }
      
      
      # fn_shiny_apply_changes_reactiveValues(rv = internal_TOOLS_SELECTOR, default_structure_internal)
      
      
      obj_intermedio <- list_user_tool_selection()
      
      the_check <- obj_intermedio$"check_selected_tool"
      selected_tool <- obj_intermedio$"selected_tool"
      
      if (!the_check) {
        texto_error <- "Error on tools selection."
        showNotification(texto_error, type = "warning")
        button_state("error")
        return()
      }
      
      
      fn_shiny_apply_changes_reactiveValues(rv = internal_TOOLS_SELECTOR, list(
        "pack_output"  = obj_intermedio,
        "check_output" = the_check,
        "button_state" = "confirmed"))
      

      
      # Succesful message
      fn_show_notification_ok(the_message = "Tool selection successful.")
     
      # Close modal
      removeModal()
   
    })
      
    
    
    # Función para restablecer este botón (accesible desde el exterior)
    return(NULL)
    
  })
}