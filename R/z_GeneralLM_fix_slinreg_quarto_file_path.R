



#' @export
GeneralLM_fix_slinreg_quarto_file_path <- function(){
  
  str_fn_name <- "anova"
  fn_file <- paste0(str_fn_name, ".qmd")
  
  # Local ---------------------------------------------------------------------- # RETIRAR AL FINAL
  the_local_folder <- file.path(getwd(), "../../../", "inst")
  
  if(dir.exists(the_local_folder)){
    the_folder_path <- file.path(the_local_folder, "quarto")  
  } 
  # ----------------------------------------------------------------------------
  if(!dir.exists(the_local_folder)){
    #From packages
    the_package_path <- find.package("Rscience.GeneralLM")
    the_folder_path <- file.path(the_package_path, "quarto") # NO DETALLAR "inst"!!!!
    
  }
  # ----------------------------------------------------------------------------
  
  
  file_path <- file.path( the_folder_path, fn_file)
  
  
  
  
  return(file_path)
}


