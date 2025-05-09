
#' @export
Sbutton_reset_ui <- function(id) {
  ns <- NS(id)
  
  # Botón para elegir variables
  actionButton(
    ns("btn_update"),
    HTML(paste0('<i class="fa fa-arrows-rotate" style="font-size: 75px; display: block; margin-bottom: 8px; transform: scaleX(-1);"></i>', 
                '<span></span>')),
    class = "btn-sm btn-warning",
    style = "height: 100px; width: 140px; display: flex; flex-direction: column; justify-content: center; align-items: center; font-size: 14px;",
    title = "Refresh!"
  )
  
  # # Botón de reseteo
  # actionButton(
  #   ns("btn_update"),
  #   "Resetear",
  #   icon = icon("arrows-rotate"),
  #   class = "btn-sm btn-warning"
  # )
}

#' @export
Sbutton_reset_server <- function(id, valores_default, valores_internos, valores_activos, reset_callbacks) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Función de reseteo para el botón "Resetear"
    observeEvent(input$btn_update, {
      # Mostrar confirmación
      showModal(modalDialog(
        title = "Confirmar reseteo",
        "¿Está seguro que desea resetear todas las selecciones?",
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("confirmar_reset"), "Resetear", class = "btn-warning")
        ),
        easyClose = TRUE
      ))
    })
    
    # Confirmación de reseteo
    observeEvent(input$confirmar_reset, {
      # Restaurar todos los valores_internos a los predeterminados
      for (nombre in names(valores_default)) {
        valores_internos[[nombre]] <- valores_default[[nombre]]
        valores_activos[[nombre]]  <- valores_default[[nombre]]
      }
      
      # Llamar a todos los callbacks de reseteo
      for (callback in reset_callbacks) {
        callback()
      }
      
      # Mostrar mensaje de éxito
      showNotification(
        "Todas las selecciones han sido reseteadas.",
        type = "message"
      )
      
      removeModal()
    })
  })
}
