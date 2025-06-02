#' @export
module_step09_download_ui <- function(id) {
  ns <- NS(id)
  
  div(
    # Agregar el código JavaScript
    tags$head(
      tags$script(HTML(paste0("
        $(document).on('click', '#", ns("download_RCode"), "', function() {
          Shiny.setInputValue('", ns("clic_descarga"), "', true);
        });
      ")))
    ),
    
    uiOutput(ns("el_cartel")),
    card(
      card_header(
        h3("Download")
      ),
      card_body(
        fluidRow(
          column(10,
                 module_step10_01_script_download_ui(id = ns("f01_script")),
                 hr(),
                 module_step10_02_all_download_ui(id = ns("f02_all")),
                 hr(),
                 module_step10_03_report_download_ui(id = ns("f03_report")),
                 hr()
          )
        ),
        br()
      ),
      # Permitir que el card crezca según sea necesario
      height = "auto"
    )
  )
}


#' @export
module_step09_download_server <- function(id, step_pos, number_current_step, 
                                           STR_STEP_NAME, default_list_step, 
                                           APP_TOTEM, internal_TOOLS_SELECTOR,
                                           internal_CFG, internal_PLAY) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    module_render_01_report_download_server(id ="f01_report", step_pos, number_current_step, 
                                            STR_STEP_NAME, default_list_step, 
                                            APP_TOTEM, internal_TOOLS_SELECTOR,
                                            internal_CFG, internal_PLAY)
    
    module_render_01_script_download_server(id ="f02_script", step_pos, number_current_step, 
                                                        STR_STEP_NAME, default_list_step, 
                                                        APP_TOTEM, internal_TOOLS_SELECTOR,
                                                        internal_CFG, internal_PLAY)
    
    
    
    module_render_03_all_download_server(id ="f03_all", step_pos, number_current_step, 
                                            STR_STEP_NAME, default_list_step, 
                                            APP_TOTEM, internal_TOOLS_SELECTOR,
                                            internal_CFG, internal_PLAY)
    

  })
  
  
}

