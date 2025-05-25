#' @export
GeneralLM_fix_anova1_MM_settings_ui <- function(id) {
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
GeneralLM_fix_anova1_MM_settings_server <- function(id, my_dataset) {
  moduleServer(id, function(input, output, session) {
    # Mostrar información sobre el dataset
    output$dataset_info <- renderText({
      paste("Dimensiones del conjunto de datos:",
            nrow(my_dataset), "filas x", ncol(my_dataset), "columnas")
    })
    
    
    # Generar UI para el selector de variable respuesta
    output$rv_selector <- renderUI({
      ns <- session$ns
      choices <- names(my_dataset)
      choices <- c("Select one..." = "", choices)
      
      selectInput(
        ns("var_name_rv"),
        "Response variable:",
        choices = choices,
        selected = choices[2]
        # selected = if (length(choices) > 0) choices[1] else NULL
      )
    })
    
    # Generar UI para el selector de factor
    output$factor_selector <- renderUI({
      ns <- session$ns
      choices <- names(my_dataset)
      choices <- c("Select one..." = "", choices)
      
      selectInput(
        ns("var_name_factor"),
        "Factor:",
        choices = choices,
        selected = choices[3]
        # selected = if (length(choices) > 0) choices[1] else NULL
      )
    })
    

    
    
    vector_choices_alpha <- c("0.01 (1%)"  = "0.01",
                              "0.05 (5%)"  = "0.05", 
                              "0.10 (10%)" = "0.10")
    
    # Generar UI para el selector de variable respuesta
    output$alpha_value <- renderUI({
      ns <- session$ns
      
      
      
      selectInput(
        ns("alpha_value"),
        "Alpha value:",
        choices = vector_choices_alpha,
        selected = vector_choices_alpha[2]
      )
      
    })
    
    output_list <- reactive({
      # Crear el objeto inicialmente con valores NA
      result <- list(
        var_name_factor = NA,
        var_name_rv = NA,
        vector_selected_vars = c("FACTOR" = NA, "RV" = NA),
        check_not_equal = NA,
        alpha_value = NA,
        external_alpha_value = NA
      )
      
      req(input$var_name_factor, input$var_name_rv, input$alpha_value)
      
      
      result$var_name_factor <- input$var_name_factor
      result$vector_selected_vars[1] <- input$var_name_factor
      
      result$var_name_rv <- input$var_name_rv
      result$vector_selected_vars[2] <- input$var_name_rv
      
      result$check_not_equal <- input$var_name_rv != input$var_name_factor
      
      minidataset <- na.omit(my_dataset[result$vector_selected_vars])
      
      result$nrow_minidataset <- nrow(minidataset)
      result$ncol_minidataset <- ncol(minidataset)
      
      result$alpha_value      <- as.numeric(as.character(input$alpha_value))
      result$external_alpha_value  <- names(vector_choices_alpha)[grepl(input$alpha_value, vector_choices_alpha)]
      
      return(result)
    })
    
    return(output_list)
    
  })
}
