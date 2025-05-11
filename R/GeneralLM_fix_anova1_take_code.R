



#' @export
GeneralLM_fix_anova1_take_code <- function(str_fn_name){
  
  str_fn_name <- "GeneralLM_fix_anova1_RCode"
  fn_file <- paste0(str_fn_name, ".R")
  
  # Local ---------------------------------------------------------------------- # Retirar al final
  the_local_folder <- file.path(getwd(), "../../../", "inst")

  if(dir.exists(the_local_folder)){
    the_folder_path <- file.path(the_local_folder, "copies")  
  } 
  # ----------------------------------------------------------------------------
  if(!dir.exists(the_local_folder)){
    #From packages
    the_package_path <- find.package("Rscience.GeneralLM")
    the_folder_path <- file.path(the_package_path, "copies") # NO DETALLAR "inst"!!!!

  }
  # ----------------------------------------------------------------------------
  
  file_path <- file.path( the_folder_path, fn_file)
  
  function_code<- readLines(file_path)
  linea_inicio <- which(grepl(" <- function\\(", function_code))[1]
  
  # Si se encontró, mantener desde esa línea en adelante
  if (!is.na(linea_inicio)) {
    function_code <- function_code[linea_inicio:length(function_code)]
  } 
  
  # function_code <- function_code[!grepl("\\#'", function_code)]
  function_code <- function_code[-1]  # Eliminar primera línea
  function_code <- function_code[-length(function_code)]  # Eliminar última línea
  
  return_positions <- grep("return", function_code)
  
  if (length(return_positions) > 0) {
    last_return_position <- max(return_positions)
    # Mantener solo las líneas hasta antes de la posición del último return
    function_code <- function_code[1:(last_return_position-1)]
  }
  
  function_code <- function_code[!grepl("\\._", function_code)]
  

    return(function_code)
}


