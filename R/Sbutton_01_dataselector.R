#' @export
Sbutton_01_dataselector_ui <- function(id) {
  ns <- NS(id)
  
  # Solo necesitamos el botón para activar el modal
  
  uiOutput(ns("my_action_button"))
  
  
}

#' @export
Sbutton_01_dataselector_server <- function(id, step_pos, number_current_step, internal_DATASET_SELECTOR) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    
    # Variable reactiva para almacenar el módulo de importación
    # the_output <- reactiveVal(NULL)  
    
    # My button
    button_state <- reactiveVal(NULL)
    
    observe({
      button_state(internal_DATASET_SELECTOR$"button_state")
    })
    
    output$my_action_button <- renderUI({

      
      btn_class <- fn_R_switch_class_from_button_state(button_state = button_state())

      actionButton(
        ns("btn_dataset"),
        tagList(
          icon("database", style = "font-size: 75px; display: block; margin-bottom: 8px;"),
          span()
        ),
        class = btn_class, 
        style = "height: 100px; width: 140px; display: flex; flex-direction: column; justify-content: center; align-items: center; font-size: 14px;",
        title = "Import dataset"
      )
    })
    
    # It a reactive object!
    my_info_dataset <- Rscience.import::MASTER_module_import_server(id = "MASTER_import", show_dev = show_dev)

    # observeEvent(my_info_dataset(),{
    #   number_current_step(step_pos)
    # }, ignoreInit = T)
    
    
    # Cuando el usuario hace clic en el botón para elegir datos
    observeEvent(input$btn_dataset, {
      
      
      
      # 2. Mostramos el modal con el contenido del módulo ya inicializado
      # Usando tamaño "xl" (extra large)
      showModal(
        modalDialog(
          # title = "Seleccionar Base de Datos",
          size = "xl", # Mantenemos "xl" como base
          easyClose = TRUE,
          
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
            Rscience.import::MASTER_module_import_ui(id = ns("MASTER_import"))
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
                         class = "btn-primary", style = "width: 50%; height: 45px;") # Aumentado la altura
            
          )
          
        )
      )
      
      
      
    })
    
    # Al confirmar selección, actualizar los valores_internos
    observeEvent(input$confirm_action, {
      
      # 1) Hacer validaciones sobre la importacion realizada.
      #    Si todo esta bien...
      # 2) Asignar nuevos valores a "valores_internos".
      # 3) Cerrar el modal
      # Verificar que se haya seleccionado un dataset primero
      if (is.null(my_info_dataset())) {
        print(my_info_dataset())
        showNotification(
          "Please, select a dataset.",
          type = "warning"
        )
        the_output(NULL)
        return()  
      }  

      
       

       
      fn_show_notification_ok(the_message = "Dataset imported successfully.")
      
      
      removeModal()

      

      fn_shiny_apply_changes_reactiveValues(rv = internal_DATASET_SELECTOR,  changes_list = list(
        "pack_output"  = my_info_dataset(),
        "check_output" = TRUE,
        "button_state" = "confirmed")
        )
      
      
    })
    
    
    
  })
}
