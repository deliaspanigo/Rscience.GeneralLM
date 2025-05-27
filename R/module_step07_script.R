#' @export
module_step07_script_ui <- function(id) {
  ns <- NS(id)
  
  
}

#' @export
module_step07_script_server <- function(id, step_pos, number_current_step, 
                                     STR_STEP_NAME, default_list_step, 
                                     APP_TOTEM, internal_DATASET_SELECTOR, internal_CFG, internal_SETTINGS) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observe({
      
      # Requeriments -----------------------------------------------------------
      req(number_current_step() == step_pos)
      req(internal_DATASET_SELECTOR, internal_CFG)
      req(internal_DATASET_SELECTOR$"check_output", internal_CFG$"check_output")
      
      # Hardcoded --------------------------------------------------------------
      current_label <- "Step 07: Script"
      current_step <- number_current_step()
      # print(paste0("Adentro del: ", current_label))
      
      # Basics and plague control ----------------------------------------------
      current_step_name <- paste0(STR_STEP_NAME, current_step)
      fn_shiny_remove_future_steps(APP_TOTEM, current_step, STR_STEP_NAME)
      
      # Check previous totem ---------------------------------------------------
      my_previous_step <- current_step - 1
      my_previous_step_name <- paste0(STR_STEP_NAME, my_previous_step)
      check_previous <- APP_TOTEM[[my_previous_step_name]]$"check_output"
      
      # Error message for check previous totem
      if(!check_previous){
        error_message <- paste0("Step: ", current_step)
        return(NULL)
      }
      
      # Action for this step - Create pack_output!!!!!!!!!! --------------------
      ### Script - copy path
      str_pk_copies_folder_path <- fn_PK_copies_folder_path()
      sub_list_script     <- internal_CFG$"pack_output"$"script"
      str_copie_file_name <- sub_list_script$"copie_file_name" 
      the_file_path  <- file.path(str_pk_copies_folder_path, str_copie_file_name) 
        
      # Reading file...
      vector_code_lines <- readLines(the_file_path)
      
      # Rcode
      Rcode_original <- fn_R_extract_code_between_markers(vector_code_lines = vector_code_lines, 
                                                          start_marker = "### INIT CODE ###", 
                                                          end_marker   = "### END CODE ###")
      
      
      # Extraer todas las coincidencias en cada posición del vector
      base_A <- "_A_"
      patron_A <- paste0(base_A, ".*?", base_A)
      vector_pattern_A  <- regmatches(Rcode_original, gregexpr(patron_A, Rcode_original))
      vector_pattern_A  <- unlist(vector_pattern_A)
      vector_names_A <- gsub(pattern = base_A, replacement = "", vector_pattern_A)
      
      vector_replacement_A_script <- internal_DATASET_SELECTOR$"pack_output"$"str_import_external"
      vector_changes_A_script <- vector_replacement_A_script
      names(vector_changes_A_script) <- vector_pattern_A
      
      vector_replacement_A_quarto <- internal_DATASET_SELECTOR$"pack_output"$"str_import_internal"
      vector_changes_A_quarto <- vector_replacement_A_quarto
      names(vector_changes_A_quarto) <- vector_pattern_A
      
      
      base_B <- "_B_"
      patron_B <- paste0(base_B, ".*?", base_B)
      vector_pattern_B  <- regmatches(Rcode_original, gregexpr(patron_B, Rcode_original))
      vector_pattern_B  <- unlist(vector_pattern_B)
      vector_names_B <- gsub(pattern = base_B, replacement = "", vector_pattern_B)
      vector_replacement_B <- internal_SETTINGS$"pack_output"[vector_names_B]
      vector_changes_B <- vector_replacement_B
      names(vector_changes_B) <- vector_pattern_B
      
      vector_changes_C <- c("#---" =  "")
      
      # Rcode Script (user visual use)
      list_vector_changes_script <- list(vector_changes_A_script, vector_changes_B, vector_changes_C)
      Rcode_script <- Rcode_original
      for (k in 1:length(list_vector_changes_script)){
        Rcode_script <- stringi::stri_replace_all_fixed(str = Rcode_script, 
                                                        pattern = names(list_vector_changes_script[[k]]), 
                                                        replacement = list_vector_changes_script[[k]],
                                                        vectorize_all = FALSE)
      }
      
      # Rcode Quarto (processing use)
      list_vector_changes_quarto <- list(vector_changes_A_quarto, vector_changes_B, vector_changes_C)
      Rcode_quarto <- Rcode_original
      for (k in 1:length(list_vector_changes_quarto)){
        Rcode_quarto <- stringi::stri_replace_all_fixed(str = Rcode_quarto, 
                                                        pattern = names(list_vector_changes_quarto[[k]]), 
                                                        replacement = list_vector_changes_quarto[[k]],
                                                        vectorize_all = FALSE)
      }
      
      # Pack output
      pack_output <- list("Rcode_original" = Rcode_original, 
                          "Rcode_script" = Rcode_script, 
                          "Rcode_quarto" = Rcode_quarto)
      
      # Check output -----------------------------------------------------------
      check_output <- !is.null(pack_output)
      button_state <- "confirmed"
      
      
      # Message error for check output -----------------------------------------
      if(!check_output){
        error_message <- paste0("Step: ", current_step)
        return(NULL)
      }
      
      
      # The new list step ------------------------------------------------------
      new_list_step <- default_list_step
      new_list_step$"current_step"   <- current_step
      new_list_step$"current_label"  <- current_label
      new_list_step$"key"            <- "script"#sys.function()
      new_list_step$"check_previous" <- check_previous
      new_list_step$"pack_output"    <- pack_output
      new_list_step$"check_output"   <- check_output
      new_list_step$"button_state"   <- button_state
      new_list_step$"the_time"       <- fn_R_the_time_beauty()
      new_list_step$"error_message"  <- ""
      
      
      # Validating the new totem list ------------------------------------------
      check_list_step <- fn_R_validate_new_list(new_list = new_list_step,  
                                                ref_list = default_list_step)
      
      # Error message for new list step ----------------------------------------
      if (!check_list_step) {
        fn_shiny_show_error_new_list(step_number = current_step, 
                                     new_list = new_list_step, 
                                     ref_list = default_list_step)
        
        return()  # Stop further execution
      }
      
      
      
      # Add --------------------------------------------------------------------
      isolate({
        APP_TOTEM[[current_step_name]] <- new_list_step
        number_current_step(current_step+1)
      })
      
      
    })
    
  })
  
  
}

