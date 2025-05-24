#' @export
GeneralLM_fix_anova1_MM_variable_selector_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Información sobre los datos
    textOutput(ns("dataset_info")),
    
    hr(),
    fluidRow(
      column(3,
             uiOutput(ns("respuesta_selector")),
             uiOutput(ns("factor_selector"))
      ),
      column(3, 
             uiOutput(ns("alpha_value"))
             )
      )
  )
    
    
    
    
  
}

#' @export
GeneralLM_fix_anova1_MM_variable_selector_server <- function(id, my_dataset) {
  moduleServer(id, function(input, output, session) {
    # Mostrar información sobre el dataset
    output$dataset_info <- renderText({
      paste("Dimensiones del conjunto de datos:",
            nrow(my_dataset), "filas x", ncol(my_dataset), "columnas")
    })
    
    
    # Generar UI para el selector de variable respuesta
    output$respuesta_selector <- renderUI({
      ns <- session$ns
      choices <- names(my_dataset)
      choices <- c("Select one..." = "", choices)
      
      selectInput(
        ns("respuesta"),
        "Select a response variable:",
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
        ns("factor"),
        "Select a factor:",
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
        "Select a response variable:",
        choices = vector_choices_alpha,
        selected = vector_choices_alpha[2]
      )
      
    })
    
    output_list <- reactive({
      # Crear el objeto inicialmente con valores NA
      result <- list(
        factor = NA,
        respuesta = NA,
        vector_selected_vars = c("FACTOR" = NA, "RV" = NA),
        check_not_equal = NA,
        alpha_value = NA
      )
      
      req(input$factor, input$respuesta, input$alpha_value)
      
      
      result$factor <- input$factor
      result$vector_selected_vars[1] <- input$factor
      
      result$respuesta <- input$respuesta
      result$vector_selected_vars[2] <- input$respuesta
      
      result$check_not_equal <- input$respuesta != input$factor
      
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
