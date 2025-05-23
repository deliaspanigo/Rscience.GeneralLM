#' @export
GeneralLM_fix_slinreg_MM_variable_selector_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Información sobre los datos
    textOutput(ns("dataset_info")),
    
    hr(),
    uiOutput(ns("y_selector")),
    uiOutput(ns("x_selector"))
    
    
    
    
  )
}

#' @export
GeneralLM_fix_slinreg_MM_variable_selector_server <- function(id, my_dataset) {
  moduleServer(id, function(input, output, session) {
    # Mostrar información sobre el dataset
    output$dataset_info <- renderText({
      paste("Dimensiones del conjunto de datos:",
            nrow(my_dataset), "filas x", ncol(my_dataset), "columnas")
    })
    
    # Generar UI para el selector de factor
    output$x_selector <- renderUI({
      ns <- session$ns
      choices <- names(my_dataset)
      choices <- c("Select one..." = "", choices)
      
      selectInput(
        ns("x_selected"),
        "Select a regresor variable (x):",
        choices = choices,
        selected = if (length(choices) > 0) choices[1] else NULL
      )
    })
    
    # Generar UI para el selector de variable respuesta
    output$y_selector <- renderUI({
      ns <- session$ns
      choices <- names(my_dataset)
      choices <- c("Select one..." = "", choices)
      
      selectInput(
        ns("y_selected"),
        "Select a response variable (y):",
        choices = choices,
        selected = if (length(choices) > 0) choices[1] else NULL
      )
    })
    
    
    output_list <- reactive({
      # Crear el objeto inicialmente con valores NA
      result <- list(
        x_selected = NA,
        y_selected = NA,
        vector_selected_vars = c("X" = NA, "Y" = NA),
        check_not_equal = NA
      )
      
      req(input$x_selected, input$y_selected)
      
      
      result$x_selected <- input$x_selected
      result$vector_selected_vars[1] <- input$x_selected
      
      result$y_selected <- input$y_selected
      result$vector_selected_vars[2] <- input$y_selected
      
      result$check_not_equal <- input$y_selected != input$x_selected
      
      minidataset <- na.omit(my_dataset[result$vector_selected_vars])
      
      result$nrow_minidataset <- nrow(minidataset)
      result$ncol_minidataset <- ncol(minidataset)
      
      return(result)
    })
    
    return(output_list)
    
  })
}
