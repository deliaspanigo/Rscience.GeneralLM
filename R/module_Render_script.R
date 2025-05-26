
#' @export
module_Render_script_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("shiny_ace_editor_OUTPUT"))
  
  
}
# GeneralLM_fix_anova1_MM_script_server
#' @export
module_Render_script_server <- function(id, show_dev, 
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
    
    file_path <- reactive({
      copies_folder_path <- fn_PK_copies_folder_path()
      selected_file <- "GeneralLM_fix_anova1_RCode.R"
      the_file_path <- file.path(copies_folder_path, selected_file)
      the_file_path
    })
    ############################################################################
    # 1) R Code
    Rcode_original <- reactive({
      req(OK_ALL_ACTIVE())
      req(file_path)
      # the_code   <- GeneralLM_fix_anova1_take_code(str_fn_name="GeneralLM_fix_anova1_RCode")
      # the_code
      vector_code_lines <- readLines(file_path())
      
      the_code <- fn_R_extract_code_between_markers(vector_code_lines = vector_code_lines, 
                                               start_marker = "### INIT CODE ###", 
                                               end_marker   = "### END CODE ###")

      the_code
    })
    
    Rcode_script   <- reactive({
      req(OK_ALL_ACTIVE(), Rcode_original())
      
      the_code   <- Rcode_original()
      # Extraer todas las coincidencias en cada posición del vector
      base_A <- "_A_"
      patron_A <- paste0(base_A, ".*?", base_A)
      vector_pattern_A  <- regmatches(the_code, gregexpr(patron_A, the_code))
      vector_pattern_A  <- unlist(vector_pattern_A)
      vector_names_A <- gsub(pattern = base_A, replacement = "", vector_pattern_A)
      
      vector_replacement_A_script <- active_DATASET_SELECTOR$"pack_output"$"str_import_external"
      vector_changes_A_script <- vector_replacement_A_script
      names(vector_changes_A_script) <- vector_pattern_A

      vector_replacement_A_quarto <- active_DATASET_SELECTOR$"pack_output"$"str_import_internal"
      vector_changes_A_quarto <- vector_replacement_A_quarto
      names(vector_changes_A_quarto) <- vector_pattern_A
      
            
      base_B <- "_B_"
      patron_B <- paste0(base_B, ".*?", base_B)
      vector_pattern_B  <- regmatches(the_code, gregexpr(patron_B, the_code))
      vector_pattern_B  <- unlist(vector_pattern_B)
      vector_names_B <- gsub(pattern = base_B, replacement = "", vector_pattern_B)
      vector_replacement_B <- active_VARIABLE_SELECTOR$"pack_output"[vector_names_B]
      vector_changes_B <- vector_replacement_B
      names(vector_changes_B) <- vector_pattern_B
    
      vector_changes_C <- c("#---" =  "")

      list_vector_changes_script <- list(vector_changes_A_script, vector_changes_B, vector_changes_C)
      Rcode_script <- Rcode_original()
      for (k in 1:length(list_vector_changes_script)){
        Rcode_script <- stringi::stri_replace_all_fixed(str = Rcode_script, 
                                                    pattern = names(list_vector_changes_script[[k]]), 
                                                    replacement = list_vector_changes_script[[k]],
                                                    vectorize_all = FALSE)
      }
      
      
      list_vector_changes_quarto <- list(vector_changes_A_quarto, vector_changes_B, vector_changes_C)
      Rcode_quarto <- Rcode_original()
      for (k in 1:length(list_vector_changes_quarto)){
        Rcode_quarto <- stringi::stri_replace_all_fixed(str = Rcode_quarto, 
                                                        pattern = names(list_vector_changes_quarto[[k]]), 
                                                        replacement = list_vector_changes_quarto[[k]],
                                                        vectorize_all = FALSE)
      }
      
      
      
      Rcode_script
      
    })
    
    Rcode_quarto <-  reactive({
      req(OK_ALL_ACTIVE(), Rcode_original())
      
      the_code   <- Rcode_original()
      # Extraer todas las coincidencias en cada posición del vector
      base_A <- "_A_"
      patronA <- paste0(base_A, ".*?", base_A)
      vector_pattern_A  <- regmatches(the_code, gregexpr(patronA, the_code))
      vector_pattern_A  <- unlist(vector_pattern_A)
      vector_names_A <- gsub(pattern = base_A, replacement = "", vector_pattern_A)
      vector_replacement_A <- active_DATASET_SELECTOR$"pack_output"$"str_import_internal"
      
      base_B <- "_B_"
      patronB <- paste0(base_B, ".*?", base_B)
      vector_pattern_B  <- regmatches(the_code, gregexpr(patronB, the_code))
      vector_pattern_B  <- unlist(vector_pattern_B)
      vector_names_B <- gsub(pattern = base_B, replacement = "", vector_pattern_B)
      vector_replacement_B <- active_VARIABLE_SELECTOR$"pack_output"[vector_names_B]
      
      
      the_code <- stringi::stri_replace_all_fixed(str = the_code, pattern = vector_pattern_A, 
                                         replacement = vector_replacement_A,
                                         vectorize_all = FALSE)
      
      the_code <- stringi::stri_replace_all_fixed(str = the_code, pattern = vector_pattern_B, 
                                         replacement = vector_replacement_B,
                                         vectorize_all = FALSE)
      
      the_code <- stringi::stri_replace_all_fixed(str = the_code, pattern = "#---", 
                                         replacement = "",
                                         vectorize_all = FALSE)
      
      
      the_code
      
    })
    
    pack_code <- reactive({
      list("Rcode_original" = Rcode_original(),
           "Rcode_script" = Rcode_script(),
           "Rcode_quarto" = Rcode_quarto())
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
        "code_generalLM_fixed_anova_1way.R"
      },
      content = function(file) {
        writeLines(Rcode_script(), file)
      }
    )
    
    
    
    
    ############################################################################
    
    
    
    
    
    
  })
}