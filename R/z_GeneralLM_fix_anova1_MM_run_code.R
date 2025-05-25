
#' @export
GeneralLM_fix_anova1_MM_run_code_ui <- function(id) {
  ns <- NS(id)
  
  
  
}

#' @export
GeneralLM_fix_anova1_MM_run_code_server <- function(id, show_dev, 
                                                  active_DATASET_SELECTOR, 
                                                  active_TOOLS_SELECTOR,
                                                  active_VARIABLE_SELECTOR,
                                                  active_PLAY_SELECTOR,
                                                  active_R_CODE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    OK_ALL_ACTIVE <- reactive({
      req(active_DATASET_SELECTOR, active_TOOLS_SELECTOR, 
          active_VARIABLE_SELECTOR, active_PLAY_SELECTOR)
      
      req(active_DATASET_SELECTOR$"check_output", 
          active_TOOLS_SELECTOR$"check_output",
          active_VARIABLE_SELECTOR$"check_output",
          active_PLAY_SELECTOR$"check_output")
      
      return(TRUE)
      
    })
    
    # Nota: si o si debo ejecutar la funcion que arme. 
    # No puedo ejecutar ni el codigo que tengo como "Rcode_script" ni el codigo
    # que tengo armard para "Rcode_quarto".
    # Sucede que esos objetos no maneja una salida, o sea, generan los objetos
    # pero despues no hay un return armado. Si o si debo ejecutar mi funcion.
    
    # Nota2: Todas las funciones tienen dataset como primer parametro.
    # El "dataset" siempre es tomado del activa_DATASET_SELECTOR.
    # El resto de los parametros es toamdo de active_VARIABLE_SELECTOR.
    # 2) R - Results
    mis_valores <- reactive({
      # req(Rcode_quarto())
      req(OK_ALL_ACTIVE())
      
      database        <- active_DATASET_SELECTOR$"pack_output"$"database" # mtcars #valores_internos_list$pack_import_dataset$"database"
      var_name_factor <- active_VARIABLE_SELECTOR$"pack_output"$"var_name_factor" #valores_internos_list$pack_var_selection$"factor"
      var_name_rv     <- active_VARIABLE_SELECTOR$"pack_output"$"var_name_rv"  #     #valores_internos_list$pack_var_selection$"respuesta"
      alpha_value     <- active_VARIABLE_SELECTOR$"pack_output"$"alpha_value"
      

      the_results        <- GeneralLM_fix_anova1_RCode(database, var_name_factor, var_name_rv, alpha_value)
      vector_order_names <- GeneralLM_fix_anova1_objects_in_order(fn = GeneralLM_fix_anova1_RCode)
      the_results        <- the_results[vector_order_names]
      the_results
      
    })
    
    ############################################################################
    
    
   
  
    
    
    
    return(mis_valores)
    
    
    
  })
}