
#' @export
Sbutton_reset2_ui <- function(id) {
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
Sbutton_reset2_server <- function(id, 
                                  number_current_step, default_list_button, internal_DATASET_SELECTOR, 
                                  internal_TOOLS_SELECTOR, internal_SETTINGS) {
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
      
      isolate({
        number_current_step(1)
        fn_shiny_apply_changes_reactiveValues(rv = internal_DATASET_SELECTOR,  changes_list = default_list_button)
        fn_shiny_apply_changes_reactiveValues(rv = internal_TOOLS_SELECTOR,  changes_list = default_list_button)
        fn_shiny_apply_changes_reactiveValues(rv = internal_SETTINGS,  changes_list = default_list_button)
      })
      
      # Mostrar mensaje de éxito
      showNotification(
        "Todas las selecciones han sido reseteadas.",
        type = "message"
      )
      
      removeModal()
    })
    
   
    observeEvent(internal_DATASET_SELECTOR$"pack_output",{
      req(internal_DATASET_SELECTOR$"check_output")
      
      isolate({
        number_current_step(3)
        fn_shiny_apply_changes_reactiveValues(rv = internal_TOOLS_SELECTOR,  changes_list = default_list_button)
        fn_shiny_apply_changes_reactiveValues(rv = internal_SETTINGS,  changes_list = default_list_button)
      })
      
      
    })
   
    observeEvent(internal_TOOLS_SELECTOR$"pack_output",{
      req(internal_TOOLS_SELECTOR$"check_output")
      
      isolate({
        number_current_step(4)
        fn_shiny_apply_changes_reactiveValues(rv = internal_SETTINGS,  changes_list = default_list_button)
      })
      
      
    })
    
  })
}
