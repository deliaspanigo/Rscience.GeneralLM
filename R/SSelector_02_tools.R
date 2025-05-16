#' @export
SSelector_02_tools_ui <- function(id) {
  ns <- NS(id)
  
  SSSelector_02_tools_GeneralLM_ui(id = ns("SSSelector_02_tools_GeneralLM"))
  
   
}

#' @export
SSelector_02_tools_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Valores reactivos para almacenar la información de la selección
    
    SSSelector_02_tools_GeneralLM_server(id = "SSSelector_02_tools_GeneralLM")
    
})
  
}