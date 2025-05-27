#' @export
module_step08_play_ui <- function(id) {
  ns <- NS(id)
  
  
}

#' @export
module_step08_play_server <- function(id, step_pos, number_current_step, 
                                        STR_STEP_NAME, default_list_step, 
                                        APP_TOTEM, internal_CFG, internal_PLAY) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    # Valores por defecto
    THE_CAPSULE <- reactiveValues()
    sub_step <- reactiveVal(1)
    ALL_DONE <- reactiveVal(FALSE)
    local_reset <- reactiveVal(FALSE)
    
    # Determinar el reset local
    observe({
      
      check_there_is_content <- sub_step()>1 
      check_no_play_pressed  <- !internal_PLAY$"check_output" 
      
      super_check <- all(check_there_is_content, check_no_play_pressed)
      local_reset(super_check)
    })
    
    # Aplicar reset local
    observeEvent(local_reset(), {
      req(local_reset())
      
      for (name in ls(THE_CAPSULE)) {
        THE_CAPSULE[[name]] <- NULL
      }
      sub_step(1)
      ALL_DONE(FALSE)
      local_reset(FALSE)
      
    })  

    ###-------------------------------------------------------------------------
    
    # Sub_step 01 - Enviroment
    observe({
      # Requeriments -----------------------------------------------------------
        req(sub_step() == 1)
        req(!ALL_DONE())
        req(number_current_step() == step_pos)
        req(internal_PLAY)
        req(internal_PLAY$"check_output")
        
        isolate({
          THE_CAPSULE$"current_step" = number_current_step()
          THE_CAPSULE$"current_step_name" = paste0(STR_STEP_NAME, step_pos)
          THE_CAPSULE$"current_label" ="Step 08: Play"
          THE_CAPSULE$"new_list_step" = NA
        })
  
    
        sub_step(sub_step()+1)
    })
    
    # Sub_step 02 - Check Previuos
    observe({
      
      req(sub_step() == 2)
      # Requeriments -----------------------------------------------------------
      req(number_current_step() == step_pos)
      req(internal_PLAY)
      req(internal_PLAY$"check_output")
      
      # Hardcoded --------------------------------------------------------------
      current_label <- "Step 08: Play"
      current_step <- number_current_step()
      #----------------------------------------------#
      THE_CAPSULE$"current_step"       <- current_step
      THE_CAPSULE$"current_label"      <- current_label
      #----------------------------------------------#
      
      # Basics and plague control ----------------------------------------------
      current_step_name <- paste0(STR_STEP_NAME, current_step)
      fn_shiny_remove_future_steps(APP_TOTEM, current_step, STR_STEP_NAME)
      
      #----------------------------------------------#
      THE_CAPSULE$"current_step_name" <- current_step_name
      #----------------------------------------------#
      
      # Check previous totem ---------------------------------------------------
      my_previous_step <- current_step - 1
      my_previous_step_name <- paste0(STR_STEP_NAME, my_previous_step)
      check_previous <- APP_TOTEM[[my_previous_step_name]]$"check_output"
      
      # Error message for check previous totem
      if(!check_previous){
        error_message <- paste0("Step: ", current_step)
        return(NULL)
      }
      
      sub_step(sub_step()+1)
    })
    
    # Sub_step 03 - Action: pack_output, check_output y new_list_step
    observe({
      req(sub_step() == 3)
      
      
      current_step      <- THE_CAPSULE$"current_step" 
      current_step_name <- THE_CAPSULE$"current_step_name" 
      current_label     <- THE_CAPSULE$"current_label" 
      
      # Action for this step - Create pack_output!!!!!!!!!! --------------------
      pack_output       <- internal_PLAY$"pack_output"
      
      
      # Check output and more --------------------------------------------------
      check_output <- internal_PLAY$"check_output"
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
      new_list_step$"key"            <- "play"
      new_list_step$"check_previous" <- TRUE
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
      
      #----------------------------------------------#
      THE_CAPSULE$"new_list_step" <- new_list_step
      #----------------------------------------------#
      sub_step(sub_step()+1)
      
    })
    
    # Sub_step 04 - Add to TOTEM
    observe({
      req(sub_step() == 4)
      
      current_step <- THE_CAPSULE$"current_step" 
      current_step_name <- THE_CAPSULE$"current_step_name"
      new_list_step <- THE_CAPSULE$"new_list_step"
      
      # Add --------------------------------------------------------------------
      isolate({
        APP_TOTEM[[current_step_name]] <- new_list_step
        number_current_step(current_step+1)
      })
      
      ALL_DONE(TRUE)
    })
    
  })
  
  
}

