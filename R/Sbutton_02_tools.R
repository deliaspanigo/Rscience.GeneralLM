#' @export
Sbutton_02_tools_ui <- function(id) {
  ns <- NS(id)
  
  # Botón para elegir variables
  actionButton(
    ns("btn_tools"),
    HTML(paste0('<i class="fa fa-hammer" style="font-size: 75px; display: block; margin-bottom: 8px; transform: scaleX(-1);"></i>', 
                '<span></span>')),
    class = "btn-special", #btn-primary", 
    style = "height: 100px; width: 140px; display: flex; flex-direction: column; justify-content: center; align-items: center; font-size: 14px;",
    title = "Statistic tools"
  )
  
  
}

#' @export
Sbutton_02_tools_server <- function(id, valores_default, valores_internos) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observe({
      special_check <- valores_internos$check_import_dataset && !valores_internos$check_tool_selection
      if(special_check) runjs(sprintf("$('#%s').css('border', '2px dotted #3355FF');", ns("btn_tools")))
    })
    
    modelo_resultado <- SSelector_tools_server("seleccion_modelo")
    # Es un objeto reactivo...
    # Contiene...
    # tipo_modelo
    # acordeon
    # modelo_seleccionado
    
    
    # Observar cambios en el dataset seleccionado
    observeEvent(valores_internos$pack_import_dataset, {
      
      # Estamos en variables...
      # Si el dataset cambia, damos reset al pack de variables, al check y al color del boton.
      # Si el dataset cambia, reset el estado de variables_seleccionadas
      valores_internos$pack_tool_selection  <-  valores_default$pack_tool_selection
      valores_internos$check_tool_selection <-  valores_default$check_tool_selection
      
      # Restablecer el color del botón a primario (azul)
      runjs(sprintf("$('#%s').removeClass('btn-neon-green').addClass('btn-primary');", ns("btn_tools")))
      
    }, ignoreInit = TRUE)
    
    # Selección de variables
    observeEvent(input$btn_tools, {
      
      # Verificar que se haya seleccionado un dataset primero
      if (!valores_internos$check_import_dataset) {
        showNotification(
          "Por favor, seleccione una base de datos primero.",
          type = "warning"
        )
        return()  # No hacer nada si no se ha seleccionado una base de datos
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
            SSelector_tools_ui(ns("seleccion_modelo"))
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
              "Cancelar"
            ),
            actionButton(inputId = ns("confirmar_tools"), label = "Seleccionar", 
                         class = "btn-primary", style = "width: 100%; height: 45px;") # Aumentado la altura
            
          )
          
        ))
    })
    
    # Al confirmar selección de variables
    observeEvent(input$confirmar_tools, {
      
      # Verificar que se haya seleccionado una opción válida
      if (!valores_internos$check_import_dataset) {
        showNotification("Por favor, seleccione una base de datos.", type = "warning")
        return()  # No hacer nada si no se seleccionó nada
      }
      
      # # Verificar que se haya seleccionado una opción válida
      # if (!is.null(modelo_resultado())) {
      #   showNotification("Por favor, una herramienta estadística.", type = "warning")
      #   return()  # No hacer nada si no se seleccionó nada
      # }
      #
      # # Verificar que se haya seleccionado una opción válida
      # if (!is.null(modelo_resultado()$modelo_seleccionado)) {
      #   showNotification("Por favor, una herramienta estadística.", type = "warning")
      #   return()  # No hacer nada si no se seleccionó nada
      # }
      
      # print(names(modelo_resultado()))
      vector_names_espected <- c("tipo_modelo", "acordeon", "modelo_seleccionado")
      vector_names_espected <- unname(vector_names_espected)
      vector_names_observed <- names(modelo_resultado())
      vector_names_observed <- unname(vector_names_observed)
      
      vector_cantidad_espected <- c(1, 1, 1)
      vector_cantidad_observed <- sapply(modelo_resultado(), length)
      
      # Verificar que se haya seleccionado una opción válida
      if (!all(vector_names_espected == vector_names_observed)) {
        #print(vector_names_espected == vector_names_observed)
        showNotification("Inconvenientes en la eleccion de TOOLS.", type = "warning")
        return()  # No hacer nada si no se seleccionó nada
      }
      
      # Verificar que se haya seleccionado una opción válida
      if (!all(vector_cantidad_espected == vector_cantidad_observed)) {
        showNotification("Inconvenientes en la eleccion de TOOLS.
                         Se espera que cada objeto tenga solo 1 elemento.", type = "warning")
        return()  # No hacer nada si no se seleccionó nada
      }
      
      
      
      # Guardar las variables seleccionadas
      valores_internos$pack_tool_selection  <- modelo_resultado()
      valores_internos$check_tool_selection <- TRUE
      
      if(valores_internos$check_tool_selection){
        # Cambiar el color del botón usando jQuery para asegurar que funcione
        runjs(sprintf("$('#%s').css('border', 'none');", ns("btn_tools")))
        runjs(sprintf("$('#%s').removeClass('btn-primary').addClass('btn-neon-green');", ns("btn_tools")))
        
        showNotification(
          ui = tags$div(
            style = "background-color: #d1e7dd; color: #0f5132; font-size: 15px; font-weight: bold; padding: 10px; border-radius: 4px; border-left: 5px solid #0f5132; display: flex; align-items: center;",
            tags$i(
              class = "fa fa-check-circle",
              style = "font-size: 50px; margin-right: 5px;"  # Tamaño de ícono más grande
            ),
            "Herramienta estadística seleccionada exitosamente."
          ),
          duration = 3,
          closeButton = TRUE
        )
        removeModal()
        # showNotification(
        #   ui = tags$div(
        #     style = "background-color: #d1e7dd; color: #0f5132; font-size: 15px; font-weight: bold; padding: 10px; border-radius: 4px; border-left: 5px solid #0f5132;",
        #     icon("check-circle"),
        #   ),
        #   duration = 15,
        #   closeButton = TRUE
        # )
        #    showNotification(, type = "default")
        
      }
      
    })
    
    # Función para restablecer este botón (accesible desde el exterior)
    return(list(
      reset = function() {
        runjs(sprintf("$('#%s').css('border', 'none');", ns("btn_tools")))
        runjs(sprintf("$('#%s').removeClass('btn-neon-green').addClass('btn-primary');", ns("btn_tools")))
      }
    ))
  })
}