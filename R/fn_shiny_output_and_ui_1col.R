
#' @export
fn_shiny_output_and_ui_1col  <- function(list_objetos, prefix, mis_valores_reactive, output, ns) {
  # Crea los outputs en bucle, en ámbitos independientes para evitar sobrescrituras
  
  ui_list <- list()
  
  for (i in seq_along(list_objetos)) {
    id_output <- paste0(prefix, i)
    obj_name <- list_objetos[[i]]$"objects"
    
    is_plotly <- FALSE
    if(length(obj_name) == 1 && grepl("^plot", obj_name)) is_plotly <- TRUE
    # the_selected_pos <- mis_valores_reactive()[[obj_name]]
    
    the_title <-   list_objetos[[i]]$"title"
    
    if(!is_plotly){
      ui_list <- c(ui_list, list(
        tags$h4(the_title),
        shinycssloaders::withSpinner(verbatimTextOutput(ns(id_output))),
        br(),
        hr()
      ))
      
      
      local({
        n <- i
        id <- id_output
        obj <- obj_name
        output[[id]] <- renderPrint({
          req(mis_valores_reactive())
          mis_valores_reactive()[obj]
        })
      })
    }
    
    if(is_plotly){
      ui_list <- c(ui_list, list(
        tags$h4(the_title),
        HTML(paste0("<b><u>R object:</u></b> ", obj_name)),
        shinycssloaders::withSpinner(plotlyOutput(ns(id_output))),
        br(),
        hr()
      ))
      
      
      local({
        n <- i
        id <- id_output
        obj <- obj_name
        output[[id]] <- renderPlotly({
          req(mis_valores_reactive())
          mis_valores_reactive()[[obj]]
        })
      })
    }
    
  }
  
  # # Crear UI dinámicamente
  # ui_list <- lapply(seq_along(list_objetos), function(i) {
  #   id_output <- paste0(prefix, i)
  #   list(
  #     h4(list_objetos[[i]]$"title"),
  #     shinycssloaders::withSpinner(verbatimTextOutput(ns(id_output))),
  #     br()
  #   )
  # })
  
  return(do.call(tagList, ui_list))
}
