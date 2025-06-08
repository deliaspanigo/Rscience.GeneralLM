#' @export
Rs_LM_fix_reg_03_multiple_one_model_MM_settings_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Información sobre los datos
    textOutput(ns("dataset_info")),
    
    hr(),
    fluidRow(
      column(3,
             uiOutput(ns("rv_selector")),
             uiOutput(ns("reg_selector"))
      ),
      column(3, 
             uiOutput(ns("alpha_value_selector"))
             )
      )
  )
    
    
    
    
  
}

#' @export
Rs_LM_fix_reg_03_multiple_one_model_MM_settings_server <- function(id, my_dataset) {
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
        inputId = ns("var_name_rv"), 
        label = "Response Variable (Y - dependent):",
        choices = choices,
        selected = choices[1]
        # selected = if (length(choices) > 0) choices[1] else NULL
      )
    })
    
    # Generar UI para el selector de factor
    output$"reg_selector" <- renderUI({
      ns <- session$ns
      
      choices <- colnames(my_dataset)
      names(choices) <- paste0("(", openxlsx::int2col(1:length(choices)), ") - ", choices)
      # choices <- c("Select one..." = "", choices)
      
      checkboxGroupInput(
        inputId = ns("var_name_reg"), 
        label = "Regresors (X - independent):",
        choices = choices,
        selected = character(0)#choices[1]
        # selected = if (length(choices) > 0) choices[1] else NULL
      )
      
      # selectInput(
      #   inputId = ns("var_name_reg"), 
      #   label = "Regresor (X - independent):",
      #   choices = choices,
      #   selected = choices[1]
      #   # selected = if (length(choices) > 0) choices[1] else NULL
      # )
    })
    

    
    
    vector_choices_alpha <- c("0.01 (1%)"  = "0.01",
                              "0.05 (5%)"  = "0.05", 
                              "0.10 (10%)" = "0.10")
    
    # Generar UI para el selector de variable respuesta
    output$"alpha_value_selector" <- renderUI({
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
        var_name_reg = NA,
        vector_name_reg = NA,
        vector_selected_vars = NA,
        check_not_equal = NA,
        alpha_value = NA,
        external_alpha_value = NA
      )
      
      req(input$"var_name_reg", input$"var_name_rv", input$"alpha_value")
      
      result$"var_name_rv"  <- input$"var_name_rv"
      # result$"var_name_reg" <- input$"var_name_reg"
      vector_str <- paste0("\"", input$"var_name_reg", "\"", collapse = ", ")
      vector_reconstruido <- paste0("c(", vector_str, ")")
      result$"var_name_reg"  <- vector_reconstruido
      
      result$"vector_selected_vars" <- c(input$"var_name_rv", input$"var_name_reg")
      result$"check_not_equal" <- length(unique(result$"vector_selected_vars")) == length(result$"vector_selected_vars")
      
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
