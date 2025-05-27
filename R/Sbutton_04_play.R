
#' @export
Sbutton_04_play_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("my_action_button"))
  
  
  
}

#' @export
Sbutton_04_play_server <- function(id, 
                                   internal_DATASET_SELECTOR, 
                                   internal_TOOLS_SELECTOR, internal_CFG,
                                   internal_SETTINGS,
                                   internal_PLAY, APP_TOTEM, number_current_step) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    #---------------------------------------------------------------------------
    # UI BUTTON - state button entails a specific color
    
    # My button
    button_state <- reactiveVal(NULL)
    
    observe({ button_state(internal_PLAY$button_state) })
    

    
    output$my_action_button <- renderUI({
      
     btn_class <- fn_R_switch_class_from_button_state(button_state = button_state())
      
     # Botón para elegir variables
     actionButton(
        ns("btn_play"),
        HTML(paste0('<i class="fa fa-play" style="font-size: 75px; display: block; margin-bottom: 8px; "></i>', 
                    '<span></span>')),
        class = btn_class, 
        style = "height: 100px; width: 140px; display: flex; flex-direction: column; justify-content: center; align-items: center; font-size: 14px;",
        title = "PLAY!"
      )
    })
    
    #---------------------------------------------------------------------------
    
    # Activar la visualización cuando se presiona PLAY
    observeEvent(input$btn_play, {
      req(!internal_PLAY$"check_output")
      req(internal_PLAY$"button_state" == "initial")
      # Esto anterior es para que si ya hizo clic, no vueva a correr el PLAY.
      
      if (!internal_DATASET_SELECTOR$check_output) {
        showNotification(
          "Please, select a dataset first!",
          type = "warning"
        )
        return(NULL)  # No hacer nada si no se ha seleccionado una base de datos
      }
      
      if (!internal_TOOLS_SELECTOR$check_output) {
        showNotification(
          "Please, select a statistic tool!",
          type = "warning"
        )
        return(NULL)  # No hacer nada si no se ha seleccionado una base de datos
      }
      
      if (!internal_CFG$check_output) {
        showNotification(
          "Select your settings!!!",
          type = "warning"
        )
        return(NULL)  # No hacer nada si no se ha seleccionado una base de datos
      }
      # No hacer nada si no se ha seleccionado una base de datos
      if (!internal_SETTINGS$check_output) {
        showNotification(
          "Please, select your settings!",
          type = "warning"
        )
        return(NULL)  
      }

      
      ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- 
      my_list <- reactiveValuesToList(APP_TOTEM)
      
      vector_check <- sapply(my_list, function(x){x$"check_output"})
      vector_check <- vector_check[1:(number_current_step()-1)]
      
      all_previous_ok <- all(vector_check)

      if (!all_previous_ok) {
        showNotification(
          "Some problems on previous steps!",
          type = "warning"
        )
        fn_shiny_apply_changes_reactiveValues(rv = internal_PLAY, list(
          "pack_output"  = "",
          "check_output" = FALSE,
          "button_state" = "error"))
        return(NULL)  
      }
      
      ### ---
      str_selected_tool     <-  internal_CFG$"pack_output"$"id"
      str_temp_folder       <-  tempdir()
      str_Rscience          <-  "Rscience"
      current_time_pritty   <-  fn_R_the_time_beauty()
      str_current_time      <-  gsub("[^0-9]", "_", current_time_pritty)
      str_tool_and_time     <-  paste0(str_selected_tool, "_", str_current_time)
      str_01_folder_work    <-  "folder_work"
      str_02_folder_output  <-  "folder_output"
      
      # Folder Work
      path_folder_work <- file.path(str_temp_folder, str_Rscience,
                                    str_tool_and_time, str_01_folder_work)
      
      if (!dir.exists(path_folder_work)) {
        dir.create(path_folder_work, recursive = TRUE)
      }
      check_folder_work <- dir.exists(path_folder_work)
      if(!check_folder_work){
        
      }
      # ----
      # Folder output
      path_folder_output <- file.path(str_temp_folder, str_Rscience,
                                    str_tool_and_time, str_02_folder_output)
      
      if (!dir.exists(path_folder_output)) {
        dir.create(path_folder_output, recursive = TRUE)
      }
      check_folder_output <- dir.exists(path_folder_output)
      if(!check_folder_output){
        
      }
      # ----
      pack_output <- list(
        "current_time_pritty"   = current_time_pritty,
        "str_current_time"      = str_current_time, 
        "path_folder_work"      = path_folder_work,
        "check_folder_work"     = check_folder_work,
        "path_folder_output"    = path_folder_output,
        "check_folder_output"   = check_folder_output
      )
      
      
      # Check output -----------------------------------------------------------
      check_output <- all(check_folder_work, check_folder_output)
      button_state <- "confirmed"
      
      if(!check_output){
        
      }
      
      
      # Message error for check output -----------------------------------------
      if(!check_output){
        error_message <- paste0("Step: ", current_step)
        return(NULL)
      }
      
      ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- 
      fn_shiny_apply_changes_reactiveValues(rv = internal_PLAY, list(
        "pack_output"  = pack_output,
        "check_output" = check_output,
        "button_state" = button_state))
      
   
    

      if(internal_PLAY$"check_output"){
        
       
          
              
       
        showNotification(
          ui = tags$div(
            style = "background-color: #d1e7dd; color: #0f5132; font-size: 15px; font-weight: bold; padding: 10px; border-radius: 4px; border-left: 5px solid #0f5132; display: flex; align-items: center;",
            tags$i(
              class = "fa fa-check-circle",
              style = "font-size: 50px; margin-right: 5px;"  
            ),
            "PLAY!!!"
          ),
          duration = 2,
          closeButton = TRUE
        )
        
      }
    
      
      
      
    })
      
      
    
      
    })
  }


