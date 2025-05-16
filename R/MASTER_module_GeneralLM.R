
#' @export
MASTER_module_GeneralLM_ui <- function(id) {
  ns <- NS(id)
  
  MASTER_module_fixed_anova_1_way_ui(id = "MASTER_fixed_anova_1_way") 
}

#' @export
MASTER_module_GeneralLM_server <- function(id, show_dev) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    MASTER_module_fixed_anova_1_way_server(id = "MASTER_fixed_anova_1_way", show_dev = F)
    
  })
}