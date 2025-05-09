
#' @export
Sbutton_01_dataselector_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Botón para elegir base de datos
    actionButton(
      ns("btn_dataset"),
      "Elegir datos",
      icon = icon("database"),
      class = "btn-sm btn-primary"
    ),
    
    # Overlay para oscurecer el fondo
    div(id = ns("overlay"),
        style = "display: none; position: fixed; top: 0; left: 0; width: 100%; height: 100%;
               background-color: rgba(0, 0, 0, 0.5); z-index: 999;"),
    
    # La ventana flotante fija
    div(
      id = ns("ventana_dataset"),
      style = "display: none; position: fixed; top: 50%; left: 50%; transform: translate(-50%, -50%);
              background: white; border: 1px solid #ddd; border-radius: 5px;
              padding: 15px; width: 80%; max-width: 1200px; z-index: 1000;
              box-shadow: 0 0 20px rgba(0,0,0,0.3);",
      
      # Contenedor para los módulos con scroll
      div(
        style = "max-height: 70vh; overflow-y: auto;",
        Rscience.import::MASTER_module_import_ui(id = ns("MASTER_import"))
        
      ),
      
      # Footer con botones
      div(
        style = "border-top: 1px solid #eee; margin-top: 15px; padding-top: 10px; text-align: right;",
        actionButton(ns("confirmar_dataset"), "Seleccionar", class = "btn-primary")
      )
    )
  )
}


#' @export
Sbutton_01_dataselector_server <- function(id, valores_internos) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    output_list_database <- Rscience.import::MASTER_module_import_server(id = "MASTER_import", show_dev = TRUE)
    
    
    # Función para mostrar ventana y overlay
    mostrar_todo <- function() {
      shinyjs::show("overlay")
      shinyjs::show("ventana_dataset", anim = TRUE)
    }
    
    # Función para cerrar ventana y overlay
    cerrar_todo <- function() {
      shinyjs::hide("ventana_dataset", anim = TRUE)
      shinyjs::hide("overlay")
    }
    
    # Mostrar la ventana flotante al hacer clic en el botón
    observeEvent(input$btn_dataset, {
      mostrar_todo()
    })
    
    # Al confirmar selección, actualizar los valores_internos
    observeEvent(input$confirmar_dataset, {
      # Verificar que se haya seleccionado una opción válida
      if (!is.list(output_list_database())) {
        showNotification("Por favor, seleccione una base de datos.", type = "warning")
        return()  # No hacer nada si no se seleccionó nada
      }
      
      if (is.null(output_list_database()$"database")) {
        showNotification("Por favor, seleccione una base de datos.", type = "warning")
        return()  # No hacer nada si no se seleccionó nada
      }
      
      vector_nombres_esperados <- c("data_source", "selected_input_file", 
                                    "temporal_file_path", "original_file_name",
                                    "str_import_selected", "str_import_external",
                                    "str_import_internal", "info_extra", "database",
                                    "error_message")
      
      vector_nombres_observados <- names(output_list_database())
      
      if (!all(vector_nombres_esperados %in% vector_nombres_observados)) {
        showNotification("Inconvenientes en la carga de la base de datos. \n
                         La lista esperada de objetos no corresponde con la observada.", type = "warning")
        return()  # No hacer nada si no se seleccionó nada
      }
      
      if (!is.data.frame(output_list_database()$"database")) {
        showNotification("Inconvenientes en la carga de la base de datos.", type = "warning")
        return()  # No hacer nada si no se seleccionó nada
      }
      
      # Como todo esta OK...
      # asignamos los objetos al pack y al check y cambiamos de color el boton del pack
      valores_internos$pack_import_dataset <- output_list_database()
      valores_internos$check_import_dataset <- TRUE
      
      if(valores_internos$check_import_dataset) {
        # Cambiar el color del botón usando jQuery 
        runjs(sprintf("$('#%s').removeClass('btn-primary').addClass('btn-success');", ns("btn_dataset")))
        showNotification(
          ui = tags$div(
            style = "background-color: #d1e7dd; color: #0f5132; font-size: 15px; font-weight: bold; padding: 10px; border-radius: 4px; border-left: 5px solid #0f5132; display: flex; align-items: center;",
            tags$i(
              class = "fa fa-check-circle",
              style = "font-size: 20px; margin-right: 5px;"
            ),
            "Base de datos importada exitosamente."
          ),
          duration = 5,
          closeButton = TRUE
        )
        
        # Cerrar la ventana y overlay
        cerrar_todo()
      } else showNotification("Error inespecífico en import_dataset.", type = "warning")
      
    })
    
    # Función para restablecer este botón (accesible desde el exterior)
    return(list(
      reset = function() {
        runjs(sprintf("$('#%s').removeClass('btn-success').addClass('btn-primary');", ns("btn_dataset")))
      }
    ))
  })
}