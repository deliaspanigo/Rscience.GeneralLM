#' @export
Rs_LM_fix_anova_01_1way_MM_settings_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Información sobre los datos
    textOutput(ns("dataset_info")),
    
    hr(),
    fluidRow(
      column(3,
             uiOutput(ns("rv_selector")),
             uiOutput(ns("factor_selector"))
      ),
      column(3, 
             uiOutput(ns("alpha_value"))
             )
      )
  )
    
    
    
    
  
}

#' @export
Rs_LM_fix_anova_01_1way_MM_settings_server <- function(id, my_dataset) {
  moduleServer(id, function(input, output, session) {
    # Mostrar información sobre el dataset
    output$dataset_info <- renderText({
      paste("Dimensiones del conjunto de datos:",
            nrow(my_dataset), "filas x", ncol(my_dataset), "columnas")
    })
    
    
    # Generar UI para el selector de variable respuesta
    output$"rv_selector" <- renderUI({
      req(my_dataset)
      ns <- session$ns
      choices <- colnames(my_dataset)
      names(choices) <- paste0("(", openxlsx::int2col(1:length(choices)), ") - ", choices)
      # names(choices) <- paste0(choices - "(", openxlsx::int2col(1:length(choices)), ")")
      choices <- c("Select one..." = "", choices)
      
      selectInput(
        inputId  = ns("var_name_rv"), 
        label    = "Response variable:",
        choices  = choices,
        selected = choices[1]
      )
    })
    
    # Generar UI para el selector de factor
    output$"factor_selector" <- renderUI({
      ns <- session$ns
      
      choices <- colnames(my_dataset)
      names(choices) <- paste0("(", openxlsx::int2col(1:length(choices)), ") - ", choices)
      choices <- c("Select one..." = "", choices)
      
      selectInput(
        inputId = ns("var_name_factor"), 
        label = "Factor:",
        choices = choices,
        selected = choices[1]
      )
    })
    

    
    
    vector_choices_alpha <- c("0.01 (1%)"  = "0.01",
                              "0.05 (5%)"  = "0.05", 
                              "0.10 (10%)" = "0.10")
    
    # Generar UI para el selector de variable respuesta
    output$"alpha_value" <- renderUI({
      ns <- session$ns
      
      
      
      selectInput(
        inputId = ns("alpha_value"), 
        label = "Alpha value:",
        choices = vector_choices_alpha,
        selected = vector_choices_alpha[2]
      )
      
    })
    
    output_list <- reactive({
      # Crear el objeto inicialmente con valores NA
      result <- list(
        var_name_rv = NA,
        var_name_factor = NA,
        vector_selected_vars = NA, #c("RV" = NA, "FACTOR" = NA),
        check_not_equal = NA,
        alpha_value = NA,
        external_alpha_value = NA
      )
      
      req(input$"var_name_factor", input$"var_name_rv", input$"alpha_value")
      
      result$"var_name_rv" <- input$"var_name_rv"
      result$"var_name_factor" <- input$"var_name_factor"
      
      result$"vector_selected_vars" <- c("RV" = input$"var_name_rv",
                                         "FACTOR" = input$"var_name_factor")

      
      
      result$"check_not_equal" <- input$"var_name_rv" != input$"var_name_factor"
      
      minidataset <- na.omit(my_dataset[result$"vector_selected_vars"])
      
      result$"nrow_minidataset" <- nrow(minidataset)
      result$"ncol_minidataset" <- ncol(minidataset)
      
      result$"alpha_value"      <- as.numeric(as.character(input$"alpha_value"))
      result$"external_alpha_value"  <- names(vector_choices_alpha)[grepl(input$"alpha_value", vector_choices_alpha)]
      
      return(result)
    })
    
    return(output_list)
    
  })
}
