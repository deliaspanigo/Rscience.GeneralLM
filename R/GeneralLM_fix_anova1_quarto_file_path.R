



#' @export
GeneralLM_fix_anova1_quarto_file_path <- function(){
  
  str_fn_name <- "anova"
  fn_file <- paste0(str_fn_name, ".qmd")
  the_package_path <- find.package("Rscience.GeneralLM")
  #the_folder_path <- file.path(the_package_path, "inst", "copies")
  the_folder_path <- file.path(the_package_path, "quarto") # NO DETALLAR "inst"!!!!
  
  file_path <- file.path( the_folder_path, fn_file)
  
  
  
  
  return(file_path)
}


