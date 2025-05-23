#' @export

fn_R_separate_vector2list_groups <- function(vector_opt, str_grepl){
  
  
  
  vector_is_plot <- grepl(str_grepl, vector_opt)
  
  # El índice en el que empieza cada nuevo bloque
  indices <- cumsum(vector_is_plot)
  
  for (k in 1:(length(vector_opt)-1)) {
    if (vector_is_plot[k]) {
      if (indices[k] == indices[k+1]) {
        indices[(k+1):length(vector_opt)] <- indices[(k+1):length(vector_opt)] + 1
      }
    }
  }
  
  
  list_separated <- split(vector_opt, indices)
  
  return(list_separated)
  
}