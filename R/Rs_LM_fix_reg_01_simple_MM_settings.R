#' @export
Rs_LM_fix_reg_01_simple_MM_settings_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Información sobre los datos
    textOutput(ns("dataset_info")),
    
    hr(),
    fluidRow(
      column(3,
             uiOutput(ns("y_selector")),
             uiOutput(ns("x_selector"))
      ),
      column(3, 
             uiOutput(ns("alpha_value"))
             )
      )
  )
    
    
    
    
  
}

#' @export
Rs_LM_fix_reg_01_simple_MM_settings_server <- function(id, my_dataset) {
  moduleServer(id, function(input, output, session) {
    # Mostrar información sobre el dataset
    output$dataset_info <- renderText({
      paste("Dimensiones del conjunto de datos:",
            nrow(my_dataset), "filas x", ncol(my_dataset), "columnas")
    })
    
    
    # Generar UI para el selector de variable respuesta
    output$y_selector <- renderUI({
      req(my_dataset)
      ns <- session$ns
      choices <- colnames(my_dataset)
      names(choices) <- paste0("(", openxlsx::int2col(1:length(choices)), ") - ", choices)
      # names(choices) <- paste0(choices - "(", openxlsx::int2col(1:length(choices)), ")")
      choices <- c("Select one..." = "", choices)
      
      selectInput(
        ns("var_name_y"),
        "Y (response variable - dependent):",
        choices = choices,
        selected = choices[1]
        # selected = if (length(choices) > 0) choices[1] else NULL
      )
    })
    
    # Generar UI para el selector de factor
    output$x_selector <- renderUI({
      ns <- session$ns
      
      choices <- colnames(my_dataset)
      names(choices) <- paste0("(", openxlsx::int2col(1:length(choices)), ") - ", choices)
      choices <- c("Select one..." = "", choices)
      
      selectInput(
        ns("var_name_x"),
        "X (regresor - independent):",
        choices = choices,
        selected = choices[1]
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
        var_name_x = NA,
        var_name_y = NA,
        vector_selected_vars = c("X" = NA, "Y" = NA),
        check_not_equal = NA,
        alpha_value = NA,
        external_alpha_value = NA
      )
      
      req(input$var_name_x, input$var_name_y, input$alpha_value)
      
      
      result$var_name_x <- input$var_name_x
      result$vector_selected_vars[1] <- input$var_name_x
      
      result$var_name_y <- input$var_name_y
      result$vector_selected_vars[2] <- input$var_name_y
      
      result$check_not_equal <- input$var_name_y != input$var_name_x
      
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
