#' @export
GeneralLM_fix_anova1_FN_validate_vars <- function(output_list_variable_selector_rv) {
  
  output_list2 <- tryCatch(output_list_variable_selector_rv(), error = function(e) e)
  output_list <- tryCatch(output_list2(), error = function(e) e)
  
  # print(output_list)
  # output_list <- output_list_variable_selector_rv()
  
  if (is.null(output_list)) {
    return(list(status = FALSE, message = "Error on selection variable module."))
  }
  
  
  if (inherits(output_list, "error")) {
    return(list(status = FALSE, message = paste("Error trying obtain output_list for variable selection:", output_list$message)))
  }
  
  # if (!is.list(output_list) || is.null(datos$database) || !is.data.frame(datos$database)) {
  #   return(list(status = FALSE, message = "Please, select a dataset."))
  # }
  
  vector_observado <- names(output_list)
  vector_esperado <- c("factor", "respuesta", "vector_selected_vars",
                "check_not_equal", "nrow_minidataset", "ncol_minidataset")
  
  # print(vector_observado)
  # print(vector_esperado)
  
  if (!all(vector_esperado %in% vector_observado)) {
    return(list(
      status = FALSE, 
      message = "Observed and espected names are not equal."
    ))
  }
  ##############################################################################
  
  
  return(list(status = TRUE, output_list = output_list))
}
