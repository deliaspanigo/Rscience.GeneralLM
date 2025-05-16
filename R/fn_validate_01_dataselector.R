#' @export
fn_validate_01_dataselector <- function(output_list_database_rv) {
  output_list_database <- output_list_database_rv()
  
  if (is.null(output_list_database) || !is.function(output_list_database)) {
    return(list(status = FALSE, message = "Error on import dataset module."))
  }
  
  datos <- tryCatch(output_list_database(), error = function(e) e)
  
  if (inherits(datos, "error")) {
    return(list(status = FALSE, message = paste("Error trying obtain dataset:", datos$message)))
  }
  
  if (!is.list(datos) || is.null(datos$database) || !is.data.frame(datos$database)) {
    return(list(status = FALSE, message = "Please, select a dataset."))
  }
  
  esperado <- c("data_source", "selected_input_file", "temporal_file_path", 
                "original_file_name", "str_import_selected", "str_import_external", 
                "str_import_internal", "info_extra", "database", "error_message")
  
  if (!all(esperado %in% names(datos))) {
    return(list(
      status = FALSE, 
      message = "Observed and espected names are not equal."
    ))
  }
  
  return(list(status = TRUE, datos = datos))
}
