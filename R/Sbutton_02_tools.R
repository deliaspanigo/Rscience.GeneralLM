#' @export
Sbutton_02_tools_ui <- function(id) {
  ns <- NS(id)
  
  # Botón para elegir variables
  actionButton(
    ns("btn_tools"),
    "Tools",
    icon = icon("sliders"),
    class = "btn-sm btn-primary"
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
          
          SSelector_tools_ui(ns("seleccion_modelo")),
          
          
          # Instrucciones
          div(
            class = "alert alert-info",
            "Por favor, seleccione exactamente 1 herramienta."
          ),
          
          footer = tagList(
            modalButton("Cancelar"),
            actionButton(ns("confirmar_tools"), "Seleccionar", class = "btn-primary")
          ),
          size = "xl"
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
          duration = 15,
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