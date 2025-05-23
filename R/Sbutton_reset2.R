
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
                                  default_structure, 
                                  internal_DATASET_SELECTOR,  active_DATASET_SELECTOR,
                                  internal_TOOLS_SELECTOR,    active_TOOLS_SELECTOR,
                                  internal_STR,               active_STR,
                                  internal_VARIABLE_SELECTOR, active_VARIABLE_SELECTOR,
                                  internal_PLAY_SELECTOR,     active_PLAY_SELECTOR) {
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
      
      fn_shiny_apply_changes_reactiveValues(rv = internal_DATASET_SELECTOR, default_structure)
      fn_shiny_apply_changes_reactiveValues(rv = active_DATASET_SELECTOR, default_structure)
      
      fn_shiny_apply_changes_reactiveValues(rv = internal_TOOLS_SELECTOR, default_structure)
      fn_shiny_apply_changes_reactiveValues(rv = active_TOOLS_SELECTOR, default_structure)
      
      fn_shiny_apply_changes_reactiveValues(rv = internal_STR, default_structure)
      fn_shiny_apply_changes_reactiveValues(rv = active_STR, default_structure)
      
      fn_shiny_apply_changes_reactiveValues(rv = internal_VARIABLE_SELECTOR, default_structure)
      fn_shiny_apply_changes_reactiveValues(rv = active_VARIABLE_SELECTOR, default_structure)
      
      fn_shiny_apply_changes_reactiveValues(rv = internal_PLAY_SELECTOR, default_structure)
      fn_shiny_apply_changes_reactiveValues(rv = active_PLAY_SELECTOR, default_structure)
      
     
      
      # Mostrar mensaje de éxito
      showNotification(
        "Todas las selecciones han sido reseteadas.",
        type = "message"
      )
      
      removeModal()
    })
    
    
    observeEvent(internal_DATASET_SELECTOR$pack_output,{

        fn_shiny_apply_changes_reactiveValues(rv = active_TOOLS_SELECTOR, default_structure)
        fn_shiny_apply_changes_reactiveValues(rv = internal_TOOLS_SELECTOR, default_structure)
        
        fn_shiny_apply_changes_reactiveValues(rv = internal_STR, default_structure)
        fn_shiny_apply_changes_reactiveValues(rv = active_STR, default_structure)
    })
    
    # 
    # 
    observeEvent(internal_TOOLS_SELECTOR$pack_output,{

        
        fn_shiny_apply_changes_reactiveValues(rv = active_VARIABLE_SELECTOR, default_structure)
        fn_shiny_apply_changes_reactiveValues(rv = internal_VARIABLE_SELECTOR, default_structure)
      
        
      
    })
    # 
    observeEvent(internal_VARIABLE_SELECTOR$pack_output,{

        fn_shiny_apply_changes_reactiveValues(rv = active_PLAY_SELECTOR, default_structure)
        fn_shiny_apply_changes_reactiveValues(rv = internal_PLAY_SELECTOR, default_structure)

      
    })
    
  })
}
