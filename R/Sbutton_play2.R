
#' @export
Sbutton_play2_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("my_action_button"))
  
  
  
}

#' @export
Sbutton_play2_server <- function(id, 
                                 default_structure, 
                                 internal_DATASET_SELECTOR,  active_DATASET_SELECTOR,
                                 internal_TOOLS_SELECTOR,    active_TOOLS_SELECTOR,
                                 internal_STR,               active_STR,
                                 internal_CFG,               active_CFG,
                                 internal_VARIABLE_SELECTOR, active_VARIABLE_SELECTOR,
                                 internal_PLAY_SELECTOR,     active_PLAY_SELECTOR) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    # My button
    button_state <- reactiveVal(NULL)
    
    observe({
      button_state(internal_PLAY_SELECTOR$button_class)
      # if(!valores_internos$check_import_dataset) button_state("initial")
    })
    
    observe({
      if(!internal_PLAY_SELECTOR$"check_output"){



        fn_shiny_apply_changes_reactiveValues(rv = active_DATASET_SELECTOR,  changes_list = default_structure)
        fn_shiny_apply_changes_reactiveValues(rv = active_TOOLS_SELECTOR,    changes_list = default_structure)
        fn_shiny_apply_changes_reactiveValues(rv = active_STR,               changes_list = default_structure)
        fn_shiny_apply_changes_reactiveValues(rv = active_CFG,               changes_list = default_structure)
        fn_shiny_apply_changes_reactiveValues(rv = active_VARIABLE_SELECTOR, changes_list = default_structure)
        # fn_shiny_apply_changes_reactiveValues(rv = active_PLAY_SELECTOR,     changes_list = default_structure)




      }
    })
    
    output$my_action_button <- renderUI({
      
      # btn_class <- switch(button_state(),
      #                     "initial"   = "btn-primary",    # Azul inicial
      #                     "confirmed" = "btn-success",    # Verde después de confirmar
      #                     "modified"  = "btn-primary")    # Vuelve a azul si se modifica
      
      btn_class <- switch(button_state(),
                          "initial"   = "btn-outline-primary",    # Azul inicial
                          "confirmed" = "btn-outline-success",    # Verde después de confirmar
                          "modified"  = "btn-outline-primary")    # Vuelve a azul si se modifica
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
    
    
    
    
  
    
    # Activar la visualización cuando se presiona PLAY
    observeEvent(input$btn_play, {
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
      
      # No hacer nada si no se ha seleccionado una base de datos
      if (!internal_VARIABLE_SELECTOR$check_output) {
        showNotification(
          "Please, select your variables!",
          type = "warning"
        )
        return(NULL)  
      }

      
      fn_shiny_apply_changes_reactiveValues(rv = internal_PLAY_SELECTOR, list(
        "pack_input"   = "",
        "check_input"  = TRUE,
        "pack_output"  = "",
        "check_output" = TRUE,
        "button_class" = "confirmed"))
      
   
    

      if(internal_PLAY_SELECTOR$"check_output"){
        
       
        req(internal_PLAY_SELECTOR$"check_output")
        req(!active_PLAY_SELECTOR$"check_output")
        # De esta forma solo continua si es la primera vez que da clic en el boton.
        
        fn_shiny_apply_changes_reactiveValues(rv = active_DATASET_SELECTOR,  changes_list = internal_DATASET_SELECTOR)
        fn_shiny_apply_changes_reactiveValues(rv = active_TOOLS_SELECTOR,    changes_list = internal_TOOLS_SELECTOR)
        fn_shiny_apply_changes_reactiveValues(rv = active_STR,               changes_list = internal_STR)
        fn_shiny_apply_changes_reactiveValues(rv = active_CFG,               changes_list = internal_CFG)
        fn_shiny_apply_changes_reactiveValues(rv = active_VARIABLE_SELECTOR, changes_list = internal_VARIABLE_SELECTOR)
        fn_shiny_apply_changes_reactiveValues(rv = active_PLAY_SELECTOR,     changes_list = internal_PLAY_SELECTOR)
        
              
       
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


