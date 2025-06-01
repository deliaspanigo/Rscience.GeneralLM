
#' @export
mod_x_all_results_v1_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("dynamic_outputs"))
  )
}

#' @export
mod_x_all_results_v1_server <- function(id, resultados) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Crear la UI dinámicamente
    output$dynamic_outputs <- renderUI({
      ui_list <- list()
      for (nombre in names(resultados)) {
        new_ui <- c()
        
        if (startsWith(nombre, "plot")) {
          new_ui <- div(
            HTML(paste0("<b><u>R plot object:</u></b> ", nombre)),
            plotlyOutput(ns(nombre)), 
            br())
        } else {
          new_ui <- div(
            verbatimTextOutput(ns(nombre)), 
            br())
        }
        ui_list[[length(ui_list) + 1]] <- new_ui
      }
      do.call(tagList, ui_list)
    })
    
    # Asignar salidas dinámicas
    # Asignar salidas dinámicas
    for (nombre in names(resultados)) {
      if (startsWith(nombre, "plot")) {
        local({
          nm <- nombre
          output[[nm]] <- renderPlotly({ resultados[[nm]]} )  # Corrección de cierre
        })
      } else {
        local({
          nm <- nombre
          output[[nm]] <- renderPrint({ resultados[nm]} )
        })
      }
    }
    
    
    
  })
}
