#' @export
SSelector_tools_ui <- function(id) {
  ns <- NS(id)
  
  
  card(
    card_header("Selección de Modelos Estadísticos"),
    card_body(
      # Usar layout_columns para crear dos columnas
      layout_columns(
        col_widths = c(3, 9), # Primera columna con ancho 3, segunda con ancho 9
        
        # Primera columna: Selección del tipo de modelo
        card(
          card_body(
            p("Selecciona un tipo de modelo:"),
            
            # Radio buttons para seleccionar el tipo de modelo (uno debajo del otro)
            radioButtons(
              ns("tipo_modelo"),
              label = NULL,
              choices = c(
                "Fixed Models" = "fixed_models",
                "Random Models" = "random_models",
                "Mixed Models" = "mixed_models"
              ),
              selected = "fixed_models",
              inline = FALSE # Radio buttons uno debajo del otro
            )
          )
        ),
        
        # Segunda columna: Acordeones de modelos
        card(
          card_body(
            # Contenedor condicional para acordeones basados en la selección
            conditionalPanel(
              condition = paste0("input['", ns("tipo_modelo"), "'] == 'fixed_models'"),
              p("Selecciona un modelo de efectos fijos:"),
              accordion(
                open = FALSE,
                # ANOVA
                accordion_panel(
                  "ANOVA",
                  radioButtons(
                    ns("fixed_anova"),
                    label = NULL,
                    width = "100%",
                    choices = c(
                      "Anova 1 way" = "anova_1_way",
                      "Anova 2 ways" = "anova_2_ways"
                    ),
                    selected = character(0)
                  )
                ),
                # ANCOVA
                accordion_panel(
                  "ANCOVA",
                  radioButtons(
                    ns("fixed_ancova"),
                    label = NULL,
                    width = "100%",
                    choices = c(
                      "Ancova with interaction" = "ancova_with_interatcion",
                      "Ancova without interaction" = "ancova_without_interatcion",
                      "Ancova Dual (with and without interaction)" = "ancova_dual"
                    ),
                    selected = character(0)
                  )
                ),
                # Dummy
                accordion_panel(
                  "Dummy",
                  radioButtons(
                    ns("fixed_dummy"),
                    label = NULL,
                    width = "100%",
                    choices = c(
                      "Dummy with interaction" = "dummy_with_interaction",
                      "Dummy without interaction" = "dummy_without_interaction",
                      "Dummy Dual (with and without interaction)" = "dummy_dual"
                    ),
                    selected = character(0)
                  )
                ),
                # Regresión Lineal
                accordion_panel(
                  "Linear Regresion",
                  radioButtons(inputId = ns("fixed_linear_reg"),
                               label = NULL,
                               width = "100%",
                               choices = c(
                                 "Simple Linear Regresion" = "simple_linear_reg",
                                 "Multiple Linear Regresion (only selected model)" = "multiple_linear_reg_1_model",
                                 "MLR - Model Selection - Forward" = "multiple_linear_reg_forward",
                                 "MLR - Model Selection - Backward" = "multiple_linear_reg_backward",
                                 "MLR - Model Selection - Dual" = "multiple_linear_reg_dual",
                                 "MLR - All Models" = "multiple_linear_reg_all_models"
                               ),
                               selected = character(0)
                  )
                )
              )
            ),
            
            conditionalPanel(
              condition = paste0("input['", ns("tipo_modelo"), "'] == 'random_models'"),
              p("Selecciona un modelo de efectos aleatorios:"),
              accordion(
                open = FALSE,
                # ANOVA Random
                accordion_panel(
                  "ANOVA",
                  radioButtons(
                    ns("random_anova"),
                    label = NULL,
                    width = "100%",
                    choices = c(
                      "Anova a 1 Factor aleatorio" = "anova_1_way_random",
                      "Anova a 2 Factores aleatorio" = "anova_2_ways_random"
                    ),
                    selected = character(0)
                  )
                )
              )
            ),
            
            conditionalPanel(
              condition = paste0("input['", ns("tipo_modelo"), "'] == 'mixed_models'"),
              p("Selecciona un modelo de efectos mixtos:"),
              accordion(
                open = FALSE,
                # ANOVA Mixed
                accordion_panel(
                  "ANOVA",
                  radioButtons(
                    ns("mixed_anova"),
                    label = NULL,
                    width = "100%",
                    choices = c(
                      "Anova a 2 Factores Mixto" = "anova_2_mixed"
                    ),
                    selected = character(0)
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

#' @export
SSelector_tools_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Valores reactivos para almacenar la información de la selección
    valores <- reactiveValues(
      tipo_modelo = "fixed_models",
      acordeon = NULL,
      modelo_seleccionado = NULL
    )
    
    # Lista de todos los inputs de radio buttons agrupados por tipo de modelo
    radio_inputs_por_tipo <- list(
      fixed_models = c("fixed_anova", "fixed_ancova", "fixed_dummy", "fixed_linear_reg"),
      random_models = c("random_anova"),
      mixed_models = c("mixed_anova")
    )
    
    # Lista que mapea inputs a acordeones
    input_to_acordeon <- list(
      fixed_anova = "ANOVA",
      fixed_ancova = "ANCOVA",
      fixed_dummy = "Dummy",
      fixed_linear_reg = "Regresión Lineal",
      random_anova = "ANOVA",
      mixed_anova = "ANOVA"
    )
    
    # Función para obtener todos los inputs de radio buttons
    get_all_radio_inputs <- function() {
      unlist(radio_inputs_por_tipo)
    }
    
    # Función para actualizar la selección actual y limpiar los demás inputs
    update_selection <- function(selected_input, selected_value) {
      valores$modelo_seleccionado <- selected_value
      valores$acordeon <- input_to_acordeon[[selected_input]]
      
      # Limpiar todas las demás selecciones
      for (input_id in get_all_radio_inputs()) {
        if (input_id != selected_input) {
          updateRadioButtons(session, input_id, selected = character(0))
        }
      }
    }
    
    # Observar cambios en el tipo de modelo
    observeEvent(input$tipo_modelo, {
      valores$tipo_modelo <- input$tipo_modelo
      valores$modelo_seleccionado <- NULL
      valores$acordeon <- NULL
      
      # Limpiar todas las selecciones cuando se cambia el tipo de modelo
      for (input_id in get_all_radio_inputs()) {
        updateRadioButtons(session, input_id, selected = character(0))
      }
    })
    
    # Observar cambios en todos los radio buttons
    observeEvent(input$fixed_anova, {
      if (!is.null(input$fixed_anova) && input$fixed_anova != "") {
        update_selection("fixed_anova", input$fixed_anova)
      }
    })
    
    observeEvent(input$fixed_ancova, {
      if (!is.null(input$fixed_ancova) && input$fixed_ancova != "") {
        update_selection("fixed_ancova", input$fixed_ancova)
      }
    })
    
    observeEvent(input$fixed_dummy, {
      if (!is.null(input$fixed_dummy) && input$fixed_dummy != "") {
        update_selection("fixed_dummy", input$fixed_dummy)
      }
    })
    
    observeEvent(input$fixed_linear_reg, {
      if (!is.null(input$fixed_linear_reg) && input$fixed_linear_reg != "") {
        update_selection("fixed_linear_reg", input$fixed_linear_reg)
      }
    })
    
    observeEvent(input$random_anova, {
      if (!is.null(input$random_anova) && input$random_anova != "") {
        update_selection("random_anova", input$random_anova)
      }
    })
    
    observeEvent(input$mixed_anova, {
      if (!is.null(input$mixed_anova) && input$mixed_anova != "") {
        update_selection("mixed_anova", input$mixed_anova)
      }
    })
    
    # Devolver una lista reactiva con toda la información
    return(reactive({
      list(
        tipo_modelo = valores$tipo_modelo,
        acordeon = valores$acordeon,
        modelo_seleccionado = valores$modelo_seleccionado
      )
    }))
  })
}
