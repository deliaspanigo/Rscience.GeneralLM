#' @export
fn_R_obj_name_in_order_from_fn <- function(fn){
  fn_text <- deparse(fn)
  first_left_key_pos <- which(grepl("\\{", fn_text))[1]
  
  # Encontrar la última posición con llave derecha '}'
  last_right_key_pos <- tail(which(grepl("\\}", fn_text)), 1)
  vector_lines <- fn_text[(first_left_key_pos+1):(last_right_key_pos-1)]
  
  # Separar y eliminar por str
  #########
  
  fn_R_eliminate_blocks <- function(str_left, str_right, vector_lines){
    
    code_left  <- paste0("(?=\\", str_left,")")
    code_right <- paste0("(?=\\", str_right,")")
    
    find_left  <-   paste0("\\", str_left)
    find_right <-   paste0("\\", str_right)
    
    vector_lines <- unlist(strsplit(vector_lines, code_left, perl=TRUE))
    vector_lines <- unlist(strsplit(vector_lines, code_right, perl=TRUE))
    pos_left_key  <- which(grepl(find_left, vector_lines))
    pos_right_key <- which(grepl(find_right, vector_lines))
    
    # Combinar los vectores y etiquetar
    combined_numbers <- c(pos_right_key, pos_left_key)
    labels <- c(rep("R", length(pos_right_key)), rep("L", length(pos_left_key)))
    
    # Ordenar los números y los labels
    ordenados_indices <- order(combined_numbers)
    sorted_numbers <- combined_numbers[ordenados_indices]
    sorted_labels <- labels[ordenados_indices]
    sorted_labels_mod <- sorted_labels
    
    check_run <- TRUE
    while(check_run){
      i <- 1
      while(i < length(sorted_labels_mod)){
        if (length(sorted_labels_mod) < 2) {
          break
        }
        if(sorted_labels_mod[i] == "L" && sorted_labels_mod[i+1] == "R"){
          num1 <- sorted_numbers[i]
          num2 <- sorted_numbers[i+1]
          # Aquí asumo que vector_lines es de la misma longitud y tiene los datos
          vector_lines[num1:num2] <- NA
          # Eliminar los elementos marcados como NA
          sorted_labels_mod <- sorted_labels_mod[-c(i, i+1)]
          sorted_numbers <- sorted_numbers[-c(i, i+1)]
          # resetear el índice para volver a revisar
          i <- 0
        }
        i <- i + 1
      }
      
      if(length(sorted_labels_mod) == 0){
        check_run <- FALSE
      }
    }
    vector_lines <- na.omit(vector_lines)
    return(vector_lines)
  }
  
  vector_lines <- fn_R_eliminate_blocks(str_left = "{", str_right = "}", vector_lines)
  vector_lines <- fn_R_eliminate_blocks(str_left = "[", str_right = "]", vector_lines)
  vector_lines <- fn_R_eliminate_blocks(str_left = "(", str_right = ")", vector_lines)
  
  vector_lines <- vector_lines[grepl("<-",    vector_lines)]
  vector_lines <- vector_lines[!grepl("#",    vector_lines)]
  vector_lines <- vector_lines[!grepl("\\._", vector_lines)]
  
  
  vector_lines <- trimws(vector_lines, which = "left")
  vector_lines <- unlist(sapply(vector_lines, strsplit, "<-"))[c(T, F)]
  vector_lines <- trimws(vector_lines, which = "right")
  vector_lines <- unique(vector_lines)
  vector_lines <- vector_lines[vector_lines != ""]
  patron3 <- "^[a-zA-Z0-9_]+$"
  vector_lines <- vector_lines[grepl(patron3, vector_lines)]
  vector_lines <- unique(vector_lines)
  
  return(vector_lines)

}