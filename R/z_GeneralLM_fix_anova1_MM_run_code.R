
#' @export
GeneralLM_fix_anova1_MM_run_code_ui <- function(id) {
  ns <- NS(id)
  
  
  
}

#' @export
GeneralLM_fix_anova1_MM_run_code_server <- function(id, show_dev, 
                                                  active_DATASET_SELECTOR, 
                                                  active_TOOLS_SELECTOR,
                                                  active_VARIABLE_SELECTOR,
                                                  active_PLAY_SELECTOR) {
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
    
    ############################################################################
    # 1) R Code
    Rcode_original <- reactive({
      req(OK_ALL_ACTIVE())
      
      the_code   <- GeneralLM_fix_anova1_take_code(str_fn_name="GeneralLM_fix_anova1_RCode")
      the_code
    })
    
    Rcode_script   <- reactive({
      req(OK_ALL_ACTIVE())
      
      the_code   <- Rcode_original()
      str_import <- active_DATASET_SELECTOR$"pack_output"$"str_import_external"
      the_code   <- sub(pattern = "_my_import_sentence_", replacement = str_import, x = the_code)
      the_code   <- gsub(pattern = "#---", replacement = "", x = the_code)
      the_code   <- gsub(pattern = "#---", replacement = "", x = the_code)
      
      the_code   <- gsub(pattern = "#---", replacement = "", x = the_code)
      
      the_code
    })
    
    Rcode_quarto <- Rcode_script   <- reactive({
      req(OK_ALL_ACTIVE(), Rcode_original())
      
      str_import      <- active_DATASET_SELECTOR$"pack_output"$"str_import_external"
      var_name_factor <- active_VARIABLE_SELECTOR$"pack_output"$"factor" #valores_internos_list$pack_var_selection$"factor"
      var_name_vr     <- active_VARIABLE_SELECTOR$"pack_output"$"respuesta"  #     #valores_internos_list$pack_var_selection$"respuesta"
      alpha_value     <- 0.05
      
      the_code   <- Rcode_original()
      the_code   <- sub(pattern = "_my_import_sentence_", replacement = str_import, x = the_code)
      the_code   <- gsub(pattern = "#---", replacement = "", x = the_code)
      the_code   <- sub(pattern = "_var_name_factor_", replacement = var_name_factor, x = the_code)
      the_code   <- sub(pattern = "_var_name_vr_", replacement = var_name_vr, x = the_code)
      the_code   <- sub(pattern = "_alpha_value_", replacement = alpha_value, x = the_code)
      the_code
      
    })
    
    ############################################################################
    
    # 2) R - Results
    mis_valores <- reactive({
      
      req(OK_ALL_ACTIVE())
      
      database        <- active_DATASET_SELECTOR$"pack_output"$"database" # mtcars #valores_internos_list$pack_import_dataset$"database"
      var_name_factor <- active_VARIABLE_SELECTOR$"pack_output"$"factor" #valores_internos_list$pack_var_selection$"factor"
      var_name_vr     <- active_VARIABLE_SELECTOR$"pack_output"$"respuesta"  #     #valores_internos_list$pack_var_selection$"respuesta"
      alpha_value     <- 0.05
      
      the_results        <- GeneralLM_fix_anova1_RCode(database, var_name_factor, var_name_vr, alpha_value)
      vector_order_names <- GeneralLM_fix_anova1_objects_in_order(fn = GeneralLM_fix_anova1_RCode)
      the_results        <- the_results[vector_order_names]
      the_results
      
    })
    
    ############################################################################
    
    
   
  
    
    
    
    return(mis_valores)
    
    
    
  })
}