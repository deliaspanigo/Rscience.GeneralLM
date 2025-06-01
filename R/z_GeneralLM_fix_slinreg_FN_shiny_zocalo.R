#' @export
GeneralLM_fix_slinreg_FN_shiny_zocalo <- function(internal_VARIABLE_SELECTOR){
  valores_list_variable_selector <- reactiveValuesToList(internal_VARIABLE_SELECTOR)
  info_VS <- valores_list_variable_selector$"pack_output"
  req(info_VS)
  

  
  value_factor <- info_VS$"factor"
  value_rv <-     info_VS$"respuesta"
  vector_selected_vars <- info_VS$"vector_selected_vars"
  new_ncol <-  info_VS$"ncol_minidataset"
  new_nrow <-  info_VS$"nrow_minidataset"
  
  div(
    class = "p-3 rounded shadow-sm",
    style = "background: linear-gradient(to right, #f8f9fa, #ffffff);",
    
    # Título principal
    h4(
      class = "mb-3 pb-2",
      style = "border-bottom: 2px solid #0d6efd; color: #0d6efd;",
      icon("info-circle"), 
      "Variables Selection"
    ),
  div(
    class = "p-2 rounded",
    style = "background-color: rgba(255, 193, 7, 0.05); border-left: 4px solid #ffc107;",
    
    h5(class = "text-warning", icon("table-cells", class = "me-2"), "Selected variables"),
    
    div(
      class = "mt-2",
      span(class = "fw-bold", "All variables: "),
      div(
        class = "mt-1",
        lapply(vector_selected_vars, function(var) {
          span(
            class = "badge bg-light text-dark me-1 mb-1",
            style = "border: 1px solid #dee2e6; padding: 5px;",
            var
          )
        })
      )
    ),
    div(
      class = "d-flex flex-column",
      div(class = "me-4 mb-2",
          span(class = "fw-bold", "Factor: "),
          span(value_factor, style = "font-family: monospace;")),
      
      div(class = "me-4 mb-2",
          span(class = "fw-bold", "Response Variable: "),
          span(value_rv, style = "font-family: monospace;"))
    ),
    
    
    div(
      span(class = "fw-bold", "Shape: "),
      span(paste0(new_nrow, " rows × ", new_ncol, " columns"),
           style = "font-family: monospace;")
    )
  )
  )
  
}