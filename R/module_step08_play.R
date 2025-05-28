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
    
    # Sub_step 04 - Copy files from pk to folder_work
    observe({
      req(sub_step() == 4)
      
      # PK details
      quarto_folder_path <- fn_PK_quarto_folder_path()
      check_ok01 <- dir.exists(quarto_folder_path)
      if(!check_ok01){
        showNotification("No encuentra o no existe la carpeta del package.", type = "error")
        return(NULL)
      }
      
      # Tool PK details
      selected_tool <- internal_CFG$"pack_output"$"id"
      pk_quarto_tool_path <- file.path(quarto_folder_path, selected_tool)
      check_ok02 <- dir.exists(pk_quarto_tool_path)
      if(!check_ok02){
        showNotification("No encuentra o no existe la carpeta de la herramienta en el package.", type = "error")
        return(NULL)
      }
      
      pk_quarto_tool_list_files <- list.files(pk_quarto_tool_path, full.names = TRUE, recursive = TRUE)
      vector_check_file_names <- sapply(pk_quarto_tool_list_files, function(x){file.exists(x)})
      check_ok03 <- all(vector_check_file_names)
      if(!check_ok03){
        showNotification("No se encuentran archivos de qurto - selected tool.", type = "error")
        return(NULL)
      }
      
      
      # Temporal Work folder path
      new_list_step <-  THE_CAPSULE$"new_list_step"
      path_folder_work <- new_list_step$"pack_output"$"path_folder_work"
      check_ok04 <- dir.exists(path_folder_work)
      if(!check_ok04){
        showNotification("No encuentra o no existe la carpeta temporal de WORK.", type = "error")
        return(NULL)
      }
      
     
      # Copiar cada archivo a la carpeta destino
      # print(vector_check_file_names)
      # print(basename(vector_check_file_names[1]))
      for (archivo in pk_quarto_tool_list_files) {
        # print(archivo)
        # Crear la ruta de destino manteniendo la estructura de subcarpetas
        ruta_destino <- file.path(path_folder_work, basename(archivo))
        
        # Copiar el archivo
        file.copy(archivo, ruta_destino)
      }
      
      
      temporal_work_list_files <- list.files(path_folder_work, full.names = TRUE, recursive = TRUE)
      vector_check_temporal_work_file_names <- sapply(temporal_work_list_files, function(x){file.exists(x)})
      check_ok04 <- all(vector_check_temporal_work_file_names)
      if(!check_ok04){
        showNotification("No se encuentran archivos de temporal work.", type = "error")
        return(NULL)
      }
      
      
      sub_step(sub_step()+1)
      
    })
    
    # Sub_step 05 - Add to TOTEM
    observe({
      req(sub_step() == 5)
      
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

