#' @export
fn_R_extract_code_between_markers <- function(vector_code_lines, 
                                              start_marker = "### INIT CODE ###", 
                                              end_marker = "### END CODE ###") {
  # # Obtener el nombre de la función actual
  # function_name <- as.character(match.call()[[1]])
  # 
  # # Check if the file exists
  # if (!file.exists(file_path)) {
  #   stop(paste0("Error in ", function_name, ": The file does not exist: ", file_path))
  # }
  # 
  # # Read lines from the file
  # vector_code_lines <- readLines(file_path)
  
  # Find positions of the markers
  start_pos <- grep(start_marker, vector_code_lines, fixed = TRUE)[1]
  end_pos <- grep(end_marker, vector_code_lines, fixed = TRUE)[1]
  
  # Check if markers are found
  if (is.na(start_pos)) {
    stop(paste0("Error in ", function_name, ": Start marker '", start_marker, "' not found in file: ", file_path))
  }
  if (is.na(end_pos)) {
    stop(paste0("Error in ", function_name, ": End marker '", end_marker, "' not found in file: ", file_path))
  }
  if (start_pos > end_pos) {
    stop(paste0("Error in ", function_name, ": Start marker appears after the end marker in file: ", file_path))
  }
  
  # Extract the code between the markers
  code_block <- vector_code_lines[start_pos:end_pos]
  
  return(code_block)
}
