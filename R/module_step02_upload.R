#' @export
module_step02_upload_ui <- function(id) {
  ns <- NS(id)
  
  
}

#' @export
module_step02_upload_server <- function(id, step_pos, number_current_step, 
                                      STR_STEP_NAME, default_list_step, APP_TOTEM) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observe({
      
      # Requeriments -----------------------------------------------------------
      req(number_current_step() == step_pos)
      print(number_current_step())
      
      # Hardcoded --------------------------------------------------------------
      current_label <- "Step 02: Up resources"
      current_step <- number_current_step()
      
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
      list_all_config02_tools <- fn_R_load_config02_yaml()
      pack_output <- list_all_config02_tools
      
      # Check output and more --------------------------------------------------
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
      new_list_step$"key"            <- "upload_resources"
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

