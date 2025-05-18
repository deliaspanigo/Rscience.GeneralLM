#' @export
Sbutton_02_tools2_ui <- function(id) {
  ns <- NS(id)
  

  uiOutput(ns("my_action_button"))
  
  
  
  
}

#' @export
Sbutton_02_tools2_server <- function(id, internal_DATASET_SELECTOR, internal_TOOLS_SELECTOR) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    # Variable reactiva para almacenar el módulo de importación
    ### output_list_tools_rv <- reactiveVal(NULL)  # # # POR AHORA INACTIVO
    
    # My button
    button_state <- reactiveVal(NULL)
    
    observe({ 
      button_state(internal_TOOLS_SELECTOR$"button_class")
    })
    
    output$my_action_button <- renderUI({
      
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

    
     
    # Selección de variables
    observeEvent(input$btn_tools, {
      
      # Verificar que se haya seleccionado un dataset primero
      if (!internal_DATASET_SELECTOR$check_output) {
        showNotification(
          "Please, select a dataset first!",
          type = "warning"
        )
        return(NULL)  # No hacer nada si no se ha seleccionado una base de datos
      }
      
      # Si ya hay un dataset elegido, mostramos ahora si las opciones para elegir
      # una herramienta estadistica.
      
      general_topic_name <- "Modelos Lineales Generales"
      showModal(
        modalDialog(
          title = paste("Seleccionar una herramienta estadístisca para", general_topic_name),
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
            "Por favor, seleccione exactamente 1 herramienta."
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
      
      # # Verificar que se haya seleccionado una opción válida
      # if (!valores_internos$check_import_dataset) {
      #   showNotification("Por favor, seleccione una base de datos.", type = "warning")
      #   return()  # No hacer nada si no se seleccionó nada
      # }
      
      obj_intermedio <- list_user_tool_selection()
      
      fn_shiny_apply_changes_reactiveValues(rv = internal_TOOLS_SELECTOR, list(
        "pack_input"   = obj_intermedio,
        "check_input"  = TRUE,
        "pack_output"  = obj_intermedio,
        "check_output" = TRUE,
        "button_class" = "confirmed"))
      
      # internal_TOOLS_SELECTOR$"pack_input"   = obj_intermedio
      # internal_TOOLS_SELECTOR$"check_input"  = TRUE
      # internal_TOOLS_SELECTOR$"pack_output"  = obj_intermedio
      # internal_TOOLS_SELECTOR$"check_output" = TRUE
      # internal_TOOLS_SELECTOR$"button_class" = "confirmed"
      

      
      # Succesful message
      fn_show_notification_ok(the_message = "Tool selection successful.")
     
      # Close modal
      removeModal()
   
    })
      
    
    
    # Función para restablecer este botón (accesible desde el exterior)
    return(NULL)
    # return(list(
    #   reset = function() {
    #     runjs(sprintf("$('#%s').css('border', 'none');", ns("btn_tools")))
    #     runjs(sprintf("$('#%s').removeClass('btn-success').addClass('btn-primary');", ns("btn_tools")))
    #   }
    # )
    # )
  })
}