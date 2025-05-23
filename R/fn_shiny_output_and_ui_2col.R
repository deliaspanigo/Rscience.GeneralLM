
#' @export
fn_shiny_output_and_ui_2col <- function(list_objetos, prefix, mis_valores_reactive, output, ns) {
  # Crear los outputs en ámbitos independientes
  for (i in seq_along(list_objetos)) {
    id_output_table <- paste0(prefix, i, "_table")
    id_output_plot <- paste0(prefix, i, "_plot")
    
    obj_name_table <- list_objetos[[i]]$"table"
    obj_name_plot <- list_objetos[[i]]$"plot"
    
    local({
      n <- i
      id_table <- id_output_table
      id_plot <- id_output_plot
      obj_table <- obj_name_table
      obj_plot <- obj_name_plot
      
      output[[id_plot]] <- plotly::renderPlotly({
        req(mis_valores_reactive())
        mis_valores_reactive()[[obj_plot]]
      })
      
      output[[id_table]] <- renderPrint({
        req(mis_valores_reactive())
        mis_valores_reactive()[obj_table]
      })
    })
  }
  
  # Crear UI dinámicamente
  ui_list <- lapply(seq_along(list_objetos), function(i) {
    
    obj_name_plot <- list_objetos[[i]]$"plot"
    id_output_table <- paste0(prefix, i, "_table")
    id_output_plot <- paste0(prefix, i, "_plot")
    list(
      h4(list_objetos[[i]]$"title"),
      HTML(paste0("<b><u>R plot object:</u></b> ", obj_name_plot)),
      fluidRow(
        column(6, shinycssloaders::withSpinner(plotlyOutput(ns(id_output_plot)))),
        column(6, shinycssloaders::withSpinner(verbatimTextOutput(ns(id_output_table))))
      ),
      hr(),
      br(), br(), br()
    )
  })
  
  return(do.call(tagList, ui_list))
}
