#' @export
Sbutton_03_variable_selector_ui <- function(id) {
  ns <- NS(id)
  
  # Botón para elegir variables
  
  uiOutput(ns("my_action_button"))
  
  
  
  
}


#' @export
Sbutton_03_variable_selector_server <- function(id, valores_default, valores_internos) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # My button
    button_state <- reactiveVal(NULL)
    
    observe({
      button_state(valores_internos$button_class_var_selection)
      # if(!valores_internos$check_import_dataset) button_state("initial")
    })
    
    output$my_action_button <- renderUI({
      
      btn_class <- switch(button_state(),
                          "initial"   = "btn-primary",    # Azul inicial
                          "confirmed" = "btn-success",    # Verde después de confirmar
                          "modified"  = "btn-primary")    # Vuelve a azul si se modifica
      
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
    
    
    # observe({
    #   special_check <- valores_internos$check_tool_selection && !valores_internos$check_var_selection
    #   
    #   if(special_check) runjs(sprintf("$('#%s').css('border', '2px dotted #3355FF');", ns("btn_variables")))
    # })
    # Observar cambios en el dataset seleccionado
    # Inicializar el módulo cuando cambia el dataset
    
    # Observar cambios en el dataset seleccionado
    observeEvent(valores_internos$pack_tool_selection, {
      
      # Estamos en variables...
      # Si el dataset cambia, damos reset al pack de variables, al check y al color del boton.
      # Si el dataset cambia, reset el estado de variables_seleccionadas
      valores_internos$pack_var_selection  <-  valores_default$pack_var_selection
      valores_internos$check_var_selection <-  valores_default$check_var_selection
      valores_internos$button_class_var_selection <-  valores_default$button_class_var_selection
      
      
      # Restablecer el color del botón a primario (azul)
      # runjs(sprintf("$('#%s').removeClass('btn-success').addClass('btn-primary');", ns("btn_tools")))
      
    }, ignoreInit = TRUE)
    
    selected_vars_anova <- SSelector_03_variable_selector_server("variable_selector", valores_internos$pack_import_dataset$"database")
    
    # factor
    # respuesta
    # vector_selected_vars
    # check_not_equal
    
    # Selección de variables
    observeEvent(input$btn_variables, {
      # Verificar que se haya seleccionado un dataset primero
      # if (!valores_internos$dataset_check_selected || valores_internos$dataset_name == "") {
      if (!valores_internos$check_import_dataset) {
        showNotification(
          "Por favor, seleccione una base de datos primero",
          type = "warning"
        )
        return()  # No hacer nada si no se ha seleccionado una base de datos
      }
      
      if (!valores_internos$check_tool_selection) {
        showNotification(
          "Por favor, seleccione una herramienta estadística.",
          type = "warning"
        )
        return()  # No hacer nada si no se ha seleccionado una base de datos
      }
      
      
      # Obtener las columnas disponibles en el dataset seleccionado
      #columnas <- names(valores_internos$dataset_df)
      
      
      #columnas <- colnames(valores_internos$pack_import_dataset$"database")
      base_name <- valores_internos$pack_import_dataset$"original_file_name"
      
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
          SSelector_03_variable_selector_ui(ns("variable_selector"))
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
      req(selected_vars_anova())
      
      variables_seleccionadas <- selected_vars_anova()
      
      # print(variables_seleccionadas)
      
      # Verificar que se hayan seleccionado exactamente 2 variables
      # if (length(variables_seleccionadas) != 2) {
      #   showNotification(
      #     "Por favor, seleccione exactamente 2 variables.",
      #     type = "warning"
      #   )
      #   return()
      # }
      vector_names_espected <- c("factor", "respuesta", "vector_selected_vars",
                                 "check_not_equal")
      
      vector_names_espected <- unname(vector_names_espected)
      vector_names_observed <- names(variables_seleccionadas)
      vector_names_observed <- unname(vector_names_observed)
      
      vector_cantidad_espected <- c(1, 1, 2, 1)
      vector_cantidad_observed <- sapply(variables_seleccionadas, length)
      
      # Verificar que se haya seleccionado una opción válida
      if (!all(vector_names_espected == vector_names_observed)) {
        #print(vector_names_espected == vector_names_observed)
        showNotification("Inconvenientes en la eleccion de Variables para ANOVA.", type = "warning")
        return()  # No hacer nada si no se seleccionó nada
      }
      
      # Verificar que se haya seleccionado una opción válida
      if (!all(vector_cantidad_espected == vector_cantidad_observed)) {
        showNotification("Inconvenientes en la eleccion de Variables para ANOVA.
                         No coincide la cantidad de elementos.", type = "warning")
        return()  # No hacer nada si no se seleccionó nada
      }
      
      
      
      # Guardar las variables seleccionadas
      valores_internos$pack_var_selection         <- variables_seleccionadas
      valores_internos$check_var_selection        <- TRUE
      valores_internos$button_class_var_selection <- "confirmed"
      
      
      if(valores_internos$check_var_selection){
        # Cambiar el color del botón usando jQuery para asegurar que funcione
        runjs(sprintf("$('#%s').css('border', 'none');", ns("btn_variables")))
        runjs(sprintf("$('#%s').removeClass('btn-primary').addClass('btn-success');", ns("btn_variables")))
        
        showNotification(
          ui = tags$div(
            style = "background-color: #d1e7dd; color: #0f5132; font-size: 15px; font-weight: bold; padding: 10px; border-radius: 4px; border-left: 5px solid #0f5132; display: flex; align-items: center;",
            tags$i(
              class = "fa fa-check-circle",
              style = "font-size: 50px; margin-right: 5px;"  # Tamaño de ícono más grande
            ),
            "Variables seleccionadas exitosamente."
          ),
          duration = 15,
          closeButton = TRUE
        )
        
        removeModal()
        
        # showNotification(
        #   ui = tags$div(
        #     style = "background-color: #d1e7dd; color: #0f5132; font-size: 15px; font-weight: bold; padding: 10px; border-radius: 4px; border-left: 5px solid #0f5132;",
        #     icon("check-circle"), "Variables seleccionadas exitosamente."
        #   ),
        #   duration = 15,
        #   closeButton = TRUE
        # )
        
        #showNotification("Variables seleccionadas.", type = "default")
        
      }
      
    })
    
    return(NULL)
    # Función para restablecer este botón (accesible desde el exterior)
    # return(list(
    #   reset = function() {
    #     runjs(sprintf("$('#%s').css('border', 'none');", ns("btn_variables")))
    #     runjs(sprintf("$('#%s').removeClass('btn-success').addClass('btn-primary');", ns("btn_variables")))
    #   }
    # ))
  })
}
