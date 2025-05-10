



#' @export
GeneralLM_fix_anova1_take_code <- function(my_fn){
  
  # Obtener el código fuente de la función como texto
  function_code <- capture.output(print(my_fn))
  # Eliminar la primera y la última línea
  function_code <- function_code[-1]  # Eliminar primera línea
  function_code <- function_code[-length(function_code)]  # Eliminar última línea
  
  # Encontrar la posición de la última aparición de "return"
  function_code_text <- paste(function_code, collapse = "\n")
  lines_vector <- strsplit(function_code_text, "\n")[[1]]
  return_positions <- grep("return", lines_vector)
  
  if (length(return_positions) > 0) {
    last_return_position <- max(return_positions)
    # Mantener solo las líneas hasta antes de la posición del último return
    lines_vector <- lines_vector[1:(last_return_position-1)]
  }
  
  # Eliminar líneas que contienen objetos que comienzan con "._"
  lines_vector <- lines_vector[!grepl("\\._", lines_vector)]
  
  # Reconstruir el texto final
  function_code <- paste(lines_vector, collapse = "\n")


    return(function_code)
}


