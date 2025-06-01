#' @export
module_extra01_output_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("the_output"))
  
  
}

#' @export
module_extra01_output_server <- function(id, APP_TOTEM, internal_TOOLS_SELECTOR) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
  
    output$the_output <- renderUI({
      GeneralLM_fix_anova1_MM_output_ui(id =ns("experiment"))
    })  
    
    GeneralLM_fix_anova1_MM_output_server(id ="experiment", 
                                          mis_valores = reactive(APP_TOTEM[["step9"]]$"pack_output"), 
                                          active_TOOLS_SELECTOR = internal_TOOLS_SELECTOR)
    
  
  })
}
