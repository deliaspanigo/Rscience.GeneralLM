
#' @export
GeneralLM_fix_anova1_objects_in_order <- function(fn) {
  # Obtener el código de la función como texto
  
  # fn = GeneralLM_fix_anova1_RCode
  fn_text <- deparse(fn)
  # Eliminar la primera y última línea (definición de función y llave final)
  fn_body <- fn_text[2:(length(fn_text)-1)]
  
  
  # Unir el cuerpo de la función en un solo texto
  body_text <- paste(fn_body, collapse = "\n")
  body_text <- gsub("\\{[^\\{\\}]*\\}", "", body_text)
  
  
  # Dividir el texto en líneas
  lines <- strsplit(body_text, "\n")[[1]]
  
  # Filtrar líneas que comienzan con # (comentarios)
  lines <- lines[!grepl("^\\s*#", lines)]
  
  # Eliminar comentarios que pueden estar al final de las líneas
  lines <- gsub("#.*$", "", lines)
  
  lines <- sub("^[[:space:]]+", "", lines)
  
  lines <- lines[grepl(".+<-.+", lines)]
  lines <- lines[!grepl("^[^a-zA-Z0-9]+<-.+", lines)]  
  
  lines <- lines[!grepl("^\\._", lines)]
  
  
  # Unir nuevamente las líneas en un solo texto
  body_text <- paste(lines, collapse = "\n")
  
  # Buscar SOLO asignaciones con <- (no con =)
  assignments <- gregexpr("([[:alnum:]_\\.]+)\\s*<-\\s*", body_text)
  
  # Extraer los nombres de las variables
  var_names <- regmatches(body_text, assignments)[[1]]
  
  # Limpiar los nombres (quitar el operador de asignación <-)
  clean_names <- gsub("\\s*<-\\s*$", "", var_names)
  
  # Eliminar duplicados manteniendo el orden de primera aparición
  vector_unique_names <- unique(clean_names)
  
  # Devolver la lista de nombres en orden
  return(vector_unique_names)
}


