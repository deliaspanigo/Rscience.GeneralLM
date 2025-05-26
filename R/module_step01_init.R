#' @export
module_step01_init_ui <- function(id) {
  ns <- NS(id)
  
  
}

#' @export
module_step01_init_server <- function(id, step_pos, number_current_step, 
                                      STR_STEP_NAME, default_list_step, APP_TOTEM) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observe({
      
      # Requeriments -----------------------------------------------------------
      req(number_current_step() == step_pos)
      print(number_current_step())
      
      # Hardcoded --------------------------------------------------------------
      current_label <- "Step 01 - Initial"
      current_step <- number_current_step()
      
      # Basic for step and plague control --------------------------------------
      current_step_name <- paste0(STR_STEP_NAME, current_step)
      fn_shiny_remove_future_steps(APP_TOTEM, current_step, STR_STEP_NAME)
      
      # The new list step ------------------------------------------------------
      new_list_step <- default_list_step
      new_list_step$"current_step"   <- current_step
      new_list_step$"current_label"  <- current_label
      new_list_step$"key"        <- "initial"#sys.function()
      new_list_step$"check_previous" <- TRUE
      new_list_step$"pack_output"    <- "Good Luck!"
      new_list_step$"check_output"   <- TRUE
      new_list_step$"button_state"   <- "confirmed"
      new_list_step$"the_time"           <- fn_R_the_time_beauty()
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

