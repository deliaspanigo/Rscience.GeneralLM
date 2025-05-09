
#' @export
SSelector_anova_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Información sobre los datos
    textOutput(ns("dataset_info")),
    
    hr(),
    
    uiOutput(ns("factor_selector")),
    uiOutput(ns("respuesta_selector"))
    
    
    
  )
}

#' @export
SSelector_anova_server <- function(id, my_dataset) {
  moduleServer(id, function(input, output, session) {
    # Mostrar información sobre el dataset
    output$dataset_info <- renderText({
      paste("Dimensiones del conjunto de datos:",
            nrow(my_dataset), "filas x", ncol(my_dataset), "columnas")
    })
    
    # Generar UI para el selector de factor
    output$factor_selector <- renderUI({
      ns <- session$ns
      choices <- names(my_dataset)
      choices <- c("Select one..." = "", choices)
      
      selectInput(
        ns("factor"),
        "Seleccione un factor:",
        choices = choices,
        selected = if (length(choices) > 0) choices[1] else NULL
      )
    })
    
    # Generar UI para el selector de variable respuesta
    output$respuesta_selector <- renderUI({
      ns <- session$ns
      choices <- names(my_dataset)
      choices <- c("Select one..." = "", choices)
      
      selectInput(
        ns("respuesta"),
        "Seleccione variable respuesta:",
        choices = choices,
        selected = if (length(choices) > 0) choices[1] else NULL
      )
    })
    
    
    output_list <- reactive({
      # Crear el objeto inicialmente con valores NA
      result <- list(
        factor = NA,
        respuesta = NA,
        vector_selected_vars = c("FACTOR" = NA, "RV" = NA),
        check_not_equal = NA
      )
      
      req(input$factor, input$respuesta)
      
      
      result$factor <- input$factor
      result$vector_selected_vars[1] <- input$factor
      
      result$respuesta <- input$respuesta
      result$vector_selected_vars[2] <- input$respuesta
      
      result$check_not_equal <- input$respuesta != input$factor
      
      return(result)
    })
    
    return(output_list)
    
  })
}
