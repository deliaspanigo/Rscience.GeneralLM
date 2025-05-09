
#' @export
Sbutton_play_ui <- function(id) {
  ns <- NS(id)
  
  actionButton(
    ns("btn_play"),
    HTML(paste0('<i class="fa fa-play" style="font-size: 75px; display: block; margin-bottom: 8px; "></i>', 
                '<span></span>')),
    class = "btn-sidebar", #"btn-primary",
    style = "height: 100px; width: 140px; display: flex; flex-direction: column; justify-content: center; align-items: center; font-size: 14px;",
    title = "PLAY!"
  )
  
  # # Botón PLAY
  # actionButton(
  #   ns("btn_play"),
  #   "PLAY",
  #   icon = icon("play"),
  #   class = "btn-sm btn-primary"
  # )
  
}

#' @export
Sbutton_play_server <- function(id, valores_default, valores_internos, valores_activos) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    # observe({
    #   special_check <- valores_internos$check_var_selection && !valores_internos$check_play
    #
    #   if(special_check) runjs(sprintf("$('#%s').css('border', '2px dotted #3355FF');", ns("btn_play")))
    # })
    
    observeEvent(valores_internos$pack_var_selection, {
      
      # Estamos en variables...
      # Si el dataset cambia, damos reset al pack de variables, al check y al color del boton.
      # Si el dataset cambia, reset el estado de variables_seleccionadas
      valores_internos$pack_play  <-  valores_default$pack_play
      valores_internos$check_play <-  valores_default$check_play
      
      # Restablecer el color del botón a primario (azul)
      runjs(sprintf("$('#%s').removeClass('btn-neon-green').addClass('btn-primary');", ns("btn_play")))
      
    }, ignoreInit = TRUE)
    
    
    observeEvent(!valores_default$check_play, {
      
      valores_activos$pack_import_dataset <- valores_default$pack_import_dataset
      valores_activos$check_import_dataset <- valores_default$check_import_dataset
      
      valores_activos$pack_tool_selection <- valores_default$pack_tool_selection
      valores_activos$check_tool_selection <- valores_default$check_tool_selection
      
      valores_activos$pack_var_selection <- valores_default$pack_var_selection
      valores_activos$check_var_selection <- valores_default$check_var_selection
      
      valores_activos$valores_internos$check_play <- valores_default$check_play
      valores_activos$the_results <- valores_default$the_results
    }, ignoreInit = TRUE)
    
    
    # Activar la visualización cuando se presiona PLAY
    observeEvent(input$btn_play, {
      # Verificar si se ha seleccionado una base de datos
      if (!valores_internos$check_import_dataset) {
        showNotification(
          "Por favor, seleccione una base de datos primero.",
          type = "warning"
        )
        return()  # No hacer nada si no se ha seleccionado una base de datos
      }
      
      # Verificar si se han seleccionado una herramienta estadistica
      if (!valores_internos$check_tool_selection) {
        showNotification(
          "Por favor, seleccione una herramienta estadística.",
          type = "warning"
        )
        return()  # No hacer nada si no se ha seleccionado una base de datos
      }
      
      # Verificar si se han seleccionado variables
      if (!valores_internos$check_var_selection) {
        showNotification(
          "Por favor, seleccione variables primero.",
          type = "warning"
        )
        return()  # No hacer nada si no se han seleccionado variables
      }
      
      valores_internos$check_play <- TRUE
      
      if(valores_internos$check_play){
        
        valores_activos$pack_import_dataset <- valores_internos$pack_import_dataset
        valores_activos$check_import_dataset <- valores_internos$check_import_dataset
        valores_activos$pack_tool_selection <-  valores_internos$pack_tool_selection
        valores_activos$check_tool_selection <- valores_internos$check_tool_selection
        valores_activos$pack_var_selection <- valores_internos$pack_var_selection
        valores_activos$check_var_selection <- valores_internos$check_var_selection
        valores_activos$check_play <- valores_internos$check_play
        valores_activos$the_results <- valores_internos$the_results
        
        
        
        # Actualizar valores_internos activos
        # valores_internos$seleccion_activa <- TRUE
        # valores_internos$dataset_activo <- valores_internos$dataset_name
        # valores_internos$variables_activas <- valores_internos$var_selection_col_names
        # valores_internos$datos_activos <- valores_internos$dataset_df
        # valores_activos <- do.call(reactiveValues, valores_default)
        
        # Cambiar el color del botón usando jQuery para asegurar que funcione
        ### runjs(sprintf("$('#%s').css('border', 'none');", ns("btn_play")))
        runjs(sprintf("$('#%s').removeClass('btn-success').addClass('btn-neon-green');", ns("btn_play")))
        
        showNotification(
          ui = tags$div(
            style = "background-color: #d1e7dd; color: #0f5132; font-size: 15px; font-weight: bold; padding: 10px; border-radius: 4px; border-left: 5px solid #0f5132; display: flex; align-items: center;",
            tags$i(
              class = "fa fa-check-circle",
              style = "font-size: 50px; margin-right: 5px;"  # Tamaño de ícono más grande
            ),
            "PLAY!!!"
          ),
          duration = 15,
          closeButton = TRUE
        )
        
      }
      
    })
    
    # Función para restablecer este botón (accesible desde el exterior)
    return(list(
      reset = function() {
        #runjs(sprintf("$('#%s').css('border', 'none');", ns("btn_play")))
        runjs(sprintf("$('#%s').removeClass('btn-neon-green').addClass('btn-primary');", ns("btn_play")))
      }
    ))
  })
}

