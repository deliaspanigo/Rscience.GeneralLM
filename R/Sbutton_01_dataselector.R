#' @export
Sbutton_01_dataselector_ui <- function(id) {
  ns <- NS(id)
  
  # Solo necesitamos el botón para activar el modal
  actionButton(
    ns("btn_dataset"),
    HTML(paste0('<i class="fa fa-database" style="font-size: 75px; display: block; margin-bottom: 8px;"></i>', 
                '<span></span>')),
    class = "btn-special", #btn-primary", 
    style = "height: 100px; width: 140px; display: flex; flex-direction: column; justify-content: center; align-items: center; font-size: 14px;",
    title = "Import dataset"
  )
  

}

#' @export
Sbutton_01_dataselector_server <- function(id, valores_internos, show_dev = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Variable reactiva para almacenar el módulo de importación
    output_list_database_rv <- reactiveVal(NULL)
    
    # Cuando el usuario hace clic en el botón para elegir datos
    observeEvent(input$btn_dataset, {
      # 1. Primero inicializamos los elementos del módulo
      # Esto garantiza que estén creados antes de mostrar el modal
      output_module <- Rscience.import::MASTER_module_import_ui(id = ns("MASTER_import"))
      
      # 2. Mostramos el modal con el contenido del módulo ya inicializado
      # Usando tamaño "xl" (extra large)
      showModal(
        modalDialog(
          # title = "Seleccionar Base de Datos",
          size = "xl", # Mantenemos "xl" como base
          easyClose = FALSE,
          
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
            output_module
          ),
          
          footer = tagList(
            actionButton(ns("confirmar_dataset"), "Seleccionar", class = "btn-primary")
          )
        )
      )
      
      
      
      
      # 3. Inicializamos el servidor del módulo DESPUÉS de que el modal esté visible
      # pero en el mismo contexto de ejecución para garantizar que todo esté listo
      output_list_database_rv(
        Rscience.import::MASTER_module_import_server(id = "MASTER_import", show_dev = show_dev)
      )
    })
    
    # Al confirmar selección, actualizar los valores_internos
    observeEvent(input$confirmar_dataset, {
      # Obtenemos el valor actual del reactiveVal
      output_list_database <- output_list_database_rv()
      
      # Verificaciones
      if (is.null(output_list_database) || !is.function(output_list_database)) {
        showNotification("El módulo de importación no se ha inicializado correctamente.", type = "error")
        return()
      }
      
      # Intentamos obtener los datos
      tryCatch({
        datos_importados <- output_list_database()
        
        # Verificar que se haya seleccionado una opción válida
        if (!is.list(datos_importados)) {
          showNotification("Por favor, seleccione una base de datos.", type = "warning")
          return()
        }
        
        if (is.null(datos_importados$"database")) {
          showNotification("Por favor, seleccione una base de datos.", type = "warning")
          return()
        }
        
        vector_nombres_esperados <- c("data_source", "selected_input_file", 
                                      "temporal_file_path", "original_file_name",
                                      "str_import_selected", "str_import_external",
                                      "str_import_internal", "info_extra", "database",
                                      "error_message")
        
        vector_nombres_observados <- names(datos_importados)
        
        if (!all(vector_nombres_esperados %in% vector_nombres_observados)) {
          showNotification("Inconvenientes en la carga de la base de datos. \nLa lista esperada de objetos no corresponde con la observada.", 
                           type = "warning")
          return()
        }
        
        if (!is.data.frame(datos_importados$"database")) {
          showNotification("Inconvenientes en la carga de la base de datos.", type = "warning")
          return()
        }
        
        # Como todo esta OK...
        # asignamos los objetos al pack y al check y cambiamos de color el boton del pack
        valores_internos$pack_import_dataset <- datos_importados
        valores_internos$check_import_dataset <- TRUE
        
        if (valores_internos$check_import_dataset) {
          # Cambiar el color del botón
          shinyjs::runjs(sprintf("$('#%s').removeClass('btn-primary').addClass('btn-success');", 
                                 ns("btn_dataset")))
          
          # Notificación de éxito
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
          
          # Cerrar el modal
          removeModal()
        } else {
          showNotification("Error inespecífico en import_dataset.", type = "warning")
        }
      }, error = function(e) {
        # En caso de error, mostrar un mensaje amigable
        showNotification(
          paste("Error al procesar los datos:", e$message), 
          type = "error"
        )
      })
    })
    
    # Función para restablecer este botón (accesible desde el exterior)
    return(list(
      reset = function() {
        shinyjs::runjs(sprintf("$('#%s').removeClass('btn-success').addClass('btn-primary');", 
                               ns("btn_dataset")))
      }
    ))
  })
}
