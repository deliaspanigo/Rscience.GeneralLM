
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
    
    
    
    
    # observeEvent(valores_internos$pack_var_selection, {
    #   
    #   # Estamos en variables...
    #   # Si el dataset cambia, damos reset al pack de variables, al check y al color del boton.
    #   # Si el dataset cambia, reset el estado de variables_seleccionadas
    #   valores_internos$pack_play         <-  default_structure$pack_play
    #   valores_internos$check_play        <-  default_structure$check_play
    #   valores_internos$button_class_play <-  default_structure$button_class_play
    #   
    #   # Restablecer el color del botón a primario (azul)
    #   runjs(sprintf("$('#%s').removeClass('btn-success').addClass('btn-primary');", ns("btn_play")))
    #   
    # }, ignoreInit = TRUE)
    
    
    # observeEvent(!default_structure$check_play, {
    #   
    #   valores_activos$pack_import_dataset  <- default_structure$pack_import_dataset
    #   valores_activos$check_import_dataset <- default_structure$check_import_dataset
    #   
    #   valores_activos$pack_tool_selection  <- default_structure$pack_tool_selection
    #   valores_activos$check_tool_selection <- default_structure$check_tool_selection
    #   
    #   valores_activos$pack_var_selection   <- default_structure$pack_var_selection
    #   valores_activos$check_var_selection  <- default_structure$check_var_selection
    #   
    #   valores_activos$valores_internos$check_play <- default_structure$check_play
    #   valores_activos$the_results <- default_structure$the_results
    #   
    #   
    # }, ignoreInit = TRUE)
    
    
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
      
      if (!internal_VARIABLE_SELECTOR$check_output) {
        showNotification(
          "Please, select your variables!",
          type = "warning"
        )
        return(NULL)  # No hacer nada si no se ha seleccionado una base de datos
      }
      # # Verificar si se han seleccionado variables
      # if (!valores_internos$check_var_selection) {
      #   showNotification(
      #     "Por favor, seleccione variables primero.",
      #     type = "warning"
      #   )
      #   return()  # No hacer nada si no se han seleccionado variables
      # }
      
      fn_shiny_apply_changes_reactiveValues(rv = internal_PLAY_SELECTOR, list(
        "pack_input"   = "",
        "check_input"  = TRUE,
        "pack_output"  = "",
        "check_output" = TRUE,
        "button_class" = "confirmed"))
      
      # internal_PLAY_SELECTOR$"pack_input"   = ""
      # internal_PLAY_SELECTOR$"check_input"  = TRUE
      # internal_PLAY_SELECTOR$"pack_output"  = ""
      # internal_PLAY_SELECTOR$"check_output" = TRUE
      # internal_PLAY_SELECTOR$"button_class" = "confirmed"
    

      if(internal_PLAY_SELECTOR$"check_output"){
        
       
        req(internal_PLAY_SELECTOR$"check_output")
        req(!active_PLAY_SELECTOR$"check_output")
        # De esta forma solo continua si es la primera vez que da clic en el boton.
        
        fn_shiny_apply_changes_reactiveValues(rv = active_DATASET_SELECTOR,  changes_list = internal_DATASET_SELECTOR)
        fn_shiny_apply_changes_reactiveValues(rv = active_TOOLS_SELECTOR,    changes_list = internal_TOOLS_SELECTOR)
        fn_shiny_apply_changes_reactiveValues(rv = active_STR,               changes_list = internal_STR)
        fn_shiny_apply_changes_reactiveValues(rv = active_VARIABLE_SELECTOR, changes_list = internal_VARIABLE_SELECTOR)
        fn_shiny_apply_changes_reactiveValues(rv = active_PLAY_SELECTOR,     changes_list = internal_PLAY_SELECTOR)
        
              
       
        showNotification(
          ui = tags$div(
            style = "background-color: #d1e7dd; color: #0f5132; font-size: 15px; font-weight: bold; padding: 10px; border-radius: 4px; border-left: 5px solid #0f5132; display: flex; align-items: center;",
            tags$i(
              class = "fa fa-check-circle",
              style = "font-size: 50px; margin-right: 5px;"  # Tamaño de ícono más grande
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


