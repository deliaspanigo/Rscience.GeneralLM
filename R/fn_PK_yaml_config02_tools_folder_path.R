



#' @export
fn_PK_yaml_config02_tools_folder_path <- function(){
  
  #   # Local ---------------------------------------------------------------------- # RETIRAR AL FINAL
  # the_local_folder <- file.path(getwd(), "../../../", "inst")
  # 
  # if(dir.exists(the_local_folder)){
  #   the_folder_path <- file.path(the_local_folder, "quarto")  
  # } 
  # ----------------------------------------------------------------------------
  # if(!dir.exists(the_local_folder)){
  #From packages
  the_package_path <- find.package("Rscience.GeneralLM")
  vector_folder_paths <- list.dirs(path = the_package_path, recursive = T)
  dt_selected_quarto_folder <- grepl("yaml/config02_tools$", vector_folder_paths, ignore.case = TRUE)
  selected_quarto_folder_path <- vector_folder_paths[dt_selected_quarto_folder]
  
  if(length(selected_quarto_folder_path)>1) selected_quarto_folder_path <- selected_quarto_folder_path[1]
  
  selected_quarto_folder_path    
  # }
  # ----------------------------------------------------------------------------
  
  
  
  
  
  return(selected_quarto_folder_path)
}


