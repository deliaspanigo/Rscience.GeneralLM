
#' @export
GeneralLM_fix_slinreg_MM_script_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("shiny_ace_editor_OUTPUT"))
  
  
}

#' @export
GeneralLM_fix_slinreg_MM_script_server <- function(id, show_dev, 
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
    
    ############################################################################
    # 1) R Code
    Rcode_original <- reactive({
      req(OK_ALL_ACTIVE())
      
      the_code   <- GeneralLM_fix_slinreg_take_code(str_fn_name="GeneralLM_fix_slinreg_RCode")
      the_code
    })
    
    Rcode_script   <- reactive({
      req(OK_ALL_ACTIVE(), Rcode_original())
      
      str_import      <- active_DATASET_SELECTOR$"pack_output"$"str_import_external"
      x_var_name      <- active_VARIABLE_SELECTOR$"pack_output"$"x_selected" #valores_internos_list$pack_var_selection$"factor"
      vr_var_name     <- active_VARIABLE_SELECTOR$"pack_output"$"y_selected"  #     #valores_internos_list$pack_var_selection$"respuesta"
      alpha_value     <- 0.05
      
      the_code   <- Rcode_original()
      the_code   <- sub(pattern = "_my_import_sentence_", replacement = str_import, x = the_code)
      the_code   <- gsub(pattern = "#---", replacement = "", x = the_code)
      the_code   <- sub(pattern = "_x_var_name_", replacement = x_var_name, x = the_code)
      the_code   <- sub(pattern = "_vr_var_name_", replacement = vr_var_name, x = the_code)
      the_code   <- sub(pattern = "_alpha_value_", replacement = alpha_value, x = the_code)
      the_code
      
    })
    
    Rcode_quarto <-  reactive({
      req(OK_ALL_ACTIVE(), Rcode_original())
      
      str_import      <- active_DATASET_SELECTOR$"pack_output"$"str_import_internal"
      x_var_name      <- active_VARIABLE_SELECTOR$"pack_output"$"x_selected" #valores_internos_list$pack_var_selection$"factor"
      vr_var_name     <- active_VARIABLE_SELECTOR$"pack_output"$"y_selected"  #     #valores_internos_list$pack_var_selection$"respuesta"
      alpha_value     <- 0.05
      
      the_code   <- Rcode_original()
      the_code   <- sub(pattern = "_my_import_sentence_", replacement = str_import, x = the_code)
      the_code   <- gsub(pattern = "#---", replacement = "", x = the_code)
      the_code   <- sub(pattern = "_x_var_name_", replacement = x_var_name, x = the_code)
      the_code   <- sub(pattern = "_vr_var_name_", replacement = vr_var_name, x = the_code)
      the_code   <- sub(pattern = "_alpha_value_", replacement = alpha_value, x = the_code)
      the_code
      
    })
    
    pack_code <- reactive({
      list(Rcode_original = Rcode_original(),
           Rcode_script = Rcode_script(),
           Rcode_quarto = Rcode_quarto())
      })
    
    
    fn_shiny_apply_changes_reactiveValues(rv = active_R_CODE,  changes_list = list(
      "pack_output" = pack_code(),
      "check_output" = TRUE,
      "button_class" = "confirmed"
    ))
    
    ############################################################################
    
    
    
    
    output$el_cartel <- renderUI({
      my_cartel <- reactiveValuesToList(active_TOOLS_SELECTOR)$"pack_output"$"selected_cartel"
      fn_html_cartel(my_text = my_cartel)
    })
    # style = "height: 100%; width: 100%; max-width: 100%; box-sizing: border-box; overflow-x: hidden;",  # Ajustes para evitar el scroll horizontal
    
   
    
    ##############################################################################
    # Tab05 - RCode
    
    output$shiny_ace_editor_MENU <- renderUI({

      #Rcode_script <- GeneralLM_fix_anova1_take_code(my_fn=GeneralLM_fix_anova1_RCode)
      # Calcular la altura adecuada para el editor basado en el número de líneas
      
      
 
          
          fluidRow(
            column(3, 
                   selectInput(ns("theme"), "Editor Theme:", 
                               choices = c("xcode", "monokai", "github", "eclipse", "tomorrow", 
                                           "solarized_light", "solarized_dark", "textmate", "twilight"),
                               selected = "solarized_dark")),
            column(3,
                   sliderInput(ns("fontSize"), "Font Size:", min = 8, max = 40, value = 14, step = 1)
            ),
            column(3, downloadButton(ns("download_btn"), "Download", icon = icon("download")))
            
          )
      
      
    })
    
    output$shiny_ace_CODE <- renderUI({
      req(Rcode_script(), input$"theme", input$"fontSize")
      
      
      line_count <- length(strsplit(Rcode_script(), "\n")[[1]])
      line_count <- line_count + 5
      # Asignar aproximadamente 20px por línea para el alto del editor
      editor_height <- paste0(max(300, line_count * 20), "px")
      
      shinyAce::aceEditor(
        outputId = "script_part1",
        value = Rcode_script(),
        mode = "r",
        theme = input$"theme", #"chrome",
        height = editor_height,#"200px",
        fontSize = input$"fontSize", #14,
        showLineNumbers = TRUE,
        readOnly = TRUE,
        autoScrollEditorIntoView = TRUE,
        maxLines = 1000,  # Un número grande para evitar scroll
        minLines = line_count 
      )
      
    })
    
    output$shiny_ace_editor_OUTPUT <- renderUI({
      # req(mis_valores())
      
      div(
        uiOutput(ns("el_cartel")),
          card(
            card_header("Editor Options"),
            card_body(
              uiOutput(ns("shiny_ace_editor_MENU")),
              uiOutput(ns("shiny_ace_CODE"))
              )
          )
      )
      
    })
    
    # Función para descargar el código como archivo .R
    output$download_btn <- downloadHandler(
      filename = function() {
        "code_generalLM_fixed_simple_linear_reg.R"
      },
      content = function(file) {
        writeLines(Rcode_script(), file)
      }
    )
    
    
    
    
    ############################################################################
    
    
    
    
    
    
  })
}