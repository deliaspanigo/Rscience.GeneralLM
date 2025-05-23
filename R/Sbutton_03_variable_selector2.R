#' @export
Sbutton_03_variable_selector2_ui <- function(id) {
  ns <- NS(id)
  
  # Botón para elegir variables
  
  uiOutput(ns("my_action_button"))
  
  
  
  
}


#' @export
Sbutton_03_variable_selector2_server <- function(id, my_list_str_rv, internal_DATASET_SELECTOR, 
                                                 internal_TOOLS_SELECTOR, internal_VARIABLE_SELECTOR) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    output_list_variable_selector_rv <- reactiveVal(NULL)
    str_01_MM_variable_selector <- reactiveVal(NULL)
    str_02_FN_validate_vars <- reactiveVal(NULL)
    
    str_server <- reactiveVal(NULL)
    str_ui <- reactiveVal(NULL)
    
    # My button
    button_state <- reactiveVal(NULL)
    
    observe({
      # req(my_list_str_rv())
      button_state(internal_VARIABLE_SELECTOR$button_class)
      
      str_01_MM_variable_selector(my_list_str_rv()$"str_01_MM_variable_selector")
      str_02_FN_validate_vars(my_list_str_rv()$"str_02_FN_validate_vars")
    })
    
    output$my_action_button <- renderUI({
      # btn_class <- switch(button_state(),
      #                     "initial"   = "btn-primary",    # Azul inicial
      #                     "confirmed" = "btn-success",    # Verde después de confirmar
      #                     "modified"  = "btn-primary")    # Vuelve a azul si se modifica
      
      btn_class <- switch(button_state(),
                          "initial"   = "btn-outline-primary",    # Azul inicial
                          "confirmed" = "btn-outline-success",    # Verde después de confirmar
                          "modified"  = "btn-outline-primary")    # Vuelve a azul si se modifica
      # Botón para elegir variables
      actionButton(
        ns("btn_variables"),
        HTML(paste0('<i class="fa fa-sliders" style="font-size: 75px; display: block; margin-bottom: 8px;"></i>', 
                    '<span></span>')),
        class = btn_class, 
        style = "height: 100px; width: 140px; display: flex; flex-direction: column; justify-content: center; align-items: center; font-size: 14px;",
        title = "Variables Selection"
      )
    })
    
    # Usar str_01_MM_variable_selector dinámicamente
    observe({
      req(str_01_MM_variable_selector())  # Asegurarse de que str_01_MM_variable_selector tenga un valor
      req(internal_DATASET_SELECTOR)
      req(internal_DATASET_SELECTOR$"pack_output")
      req(internal_DATASET_SELECTOR$"pack_output"$"database")
      
      new_server <- paste0(str_01_MM_variable_selector(), "_server")
      new_ui <- paste0(str_01_MM_variable_selector(), "_ui")
      
      str_server(new_server)
      str_ui(new_ui)
      
      args <- list(id = "the_selection", my_dataset = internal_DATASET_SELECTOR$"pack_output"$"database")

      vector_funciones <- ls("package:Rscience.GeneralLM")
      
      # Verificar si la función existe y ejecutarla
      if (str_server() %in% vector_funciones ) {
        resultado <- do.call(str_server(), args)
        output_list_variable_selector_rv(resultado)
        # print(resultado)  # Output: 5
      } else {
        print("La función no existe.")
      }
      
      # # Construir la cadena de la función
      # str_server <- paste0(
      #   str_01_MM_variable_selector(), "_server",
      #   '(id = "the_selection", my_dataset = internal_DATASET_SELECTOR$"pack_output"$"database")'
      # )
      # 
      # # Ejecutar la función dinámicamente
      # output_list_variable_selector_rv(eval(parse(text = str_server)))
    })
    
    # Renderizar la UI del selector de variables
    output$salida_general <- renderUI({
      req(str_01_MM_variable_selector())  # Asegurarse de que str_01_MM_variable_selector tenga un valor
      
      # str_ui(new_ui)
      args <- list(id = ns("the_selection"))
      resultado <- do.call(str_ui(), args)
      resultado
      
      # # Construir la cadena de la función UI
      # str_ui <- paste0(
      #   str_01_MM_variable_selector(),  # Nombre de la función
      #   '_ui(id = ns("the_selection"))'
      # )
      # 
      # # Ejecutar la función dinámicamente
      # eval(parse(text = str_ui))
    })
    
    # factor
    # respuesta
    # vector_selected_vars
    # check_not_equal
    
    # Selección de variables
    observeEvent(input$btn_variables, {
      
      if (!internal_DATASET_SELECTOR$check_output) {
        showNotification(
          "Please, select a dataset first!",
          type = "warning"
        )
        return(NULL)  # No hacer nada si no se ha seleccionado una base de datos
      }
      
      if (!internal_TOOLS_SELECTOR$check_output) {
        showNotification(
          "Please, select a statistic tool!",
          type = "warning"
        )
        return(NULL)  # No hacer nada si no se ha seleccionado una base de datos
      }
      
      
      # Obtener las columnas disponibles en el dataset seleccionado
      #columnas <- names(valores_internos$dataset_df)
      
      
      #columnas <- colnames(valores_internos$pack_import_dataset$"database")
      base_name <- internal_DATASET_SELECTOR$"pack_output"$"original_file_name"
      
      showModal(modalDialog(
        title = paste("Seleccionar variables para", base_name),
        size = "xl",
        easyClose = TRUE,
        
        tags$div(
          tags$style(HTML("
        /* Hacer que el modal sea más grande que xl - ancho y alto */
        .modal-xl {
          max-width: 95% !important; /* Aumentamos el ancho a 95% de la ventana */
          width: 95%;
        }
        
        /* Aumentar la altura del modal y posicionarlo más cerca del borde superior */
        .modal-dialog {
          height: 90vh !important; /* 90% de la altura de la ventana */
          max-height: 90vh !important;
          margin-top: 20px !important; /* Reducimos el margen superior (valor por defecto es 1.75rem ~28px) */
        }
        
        /* Hacer que el contenido del modal ocupe más espacio vertical */
        .modal-content {
          height: 100% !important;
          display: flex;
          flex-direction: column;
        }
        
        /* Ajustar el cuerpo del modal para que ocupe el espacio disponible */
        .modal-body {
          flex: 1;
          overflow: hidden; /* Evita scroll doble */
          padding: 0; /* Quitamos padding para maximizar espacio */
        }
        
        /* Asegurar que en pantallas muy grandes se mantenga un tamaño razonable */
        @media (min-width: 1400px) {
          .modal-xl {
            max-width: 1800px !important; /* O el tamaño máximo que prefieras */
          }
        }
      ")),
        ),
        
        # Instrucciones
        div(
          class = "alert alert-info",
          "Por favor, seleccione exactamente 2 variables."
        ),
        div(
          style = "height: 100%; overflow-y: auto; padding: 15px;", 
          uiOutput(ns("salida_general"))
        ),
        footer = tags$div(
          style = "display: flex; justify-content: center; width: 100%; gap: 10px;",
          # Botón Cancelar de ancho completo
          tags$button(
            id = ns("btn_cancelar"),
            type = "button",
            class = "btn btn-default",
            style = "width: 50%; height: 45px;", # Aumentado la altura
            "data-bs-dismiss" = "modal",
            "CANCEL"
          ),
          actionButton(inputId = ns("confirmar_variables"), label = "ADD", 
                       class = "btn-primary", style = "width: 100%; height: 45px;") # Aumentado la altura
          
        )
      ))
    })
    
    
    # Al confirmar selección de variables
    observeEvent(input$confirmar_variables, {
      
      if (is.null(output_list_variable_selector_rv)) {
        # print(output_list_variable_selector_rv)
        showNotification(
          "Please, select variables.",
          type = "warning"
        )
        return()  # No hacer nada si no se ha seleccionado una base de datos
      }
      
      
      # variables_seleccionadas <- output_list_variable_selector_rv()
      
      args <- list(output_list_variable_selector_rv = output_list_variable_selector_rv)
      # print(str_02_FN_validate_vars())
      resultado <- do.call(str_02_FN_validate_vars(), args)
      resultado
        
      # resultado <- GeneralLM_fix_anova1_FN_validate_vars(output_list_variable_selector_rv = output_list_variable_selector_rv)
      
      if (!resultado$status) {
        showNotification(resultado$message, type = "warning")
        return()
      }
      
      # vector_names_espected <- c("factor", "respuesta", "vector_selected_vars",
      #                            "check_not_equal", "nrow_minidataset", "ncol_minidataset")
      # 
      # vector_names_espected <- unname(vector_names_espected)
      # vector_names_observed <- names(variables_seleccionadas)
      # vector_names_observed <- unname(vector_names_observed)
      # 
      # vector_cantidad_espected <- c(1, 1, 2, 1, 1, 1)
      # vector_cantidad_observed <- sapply(variables_seleccionadas, length)
      
      # # print(vector_names_espected)
      # # print(vector_names_observed)
      # # Verificar que se haya seleccionado una opción válida
      # if (!all(vector_names_espected == vector_names_observed)) {
      #   #print(vector_names_espected == vector_names_observed)
      #   internal_VARIABLE_SELECTOR$"pack_input"   = ""
      #   internal_VARIABLE_SELECTOR$"check_input"  = FALSE
      #   internal_VARIABLE_SELECTOR$"pack_output"  = ""
      #   internal_VARIABLE_SELECTOR$"check_output" = FALSE
      #   internal_VARIABLE_SELECTOR$"button_class" = "initial"
      #   showNotification("Inconvenientes en la eleccion de Variables para ANOVA.", type = "warning")
      #   return()  # No hacer nada si no se seleccionó nada
      # }
      # 
      # # Verificar que se haya seleccionado una opción válida
      # if (!all(vector_cantidad_espected == vector_cantidad_observed)) {
      #   internal_VARIABLE_SELECTOR$"pack_input"   = ""
      #   internal_VARIABLE_SELECTOR$"check_input"  = FALSE
      #   internal_VARIABLE_SELECTOR$"pack_output"  = ""
      #   internal_VARIABLE_SELECTOR$"check_output" = FALSE
      #   internal_VARIABLE_SELECTOR$"button_class" = "initial"
      #   showNotification("Inconvenientes en la eleccion de Variables para ANOVA.
      #                    No coincide la cantidad de elementos.", type = "warning")
      #   return()  # No hacer nada si no se seleccionó nada
      # }
      # 
      # 
      
      fn_shiny_apply_changes_reactiveValues(rv = internal_VARIABLE_SELECTOR, list(
        "pack_input"   = resultado$output_list,
        "check_input"  = resultado$status,
        "pack_output"  = resultado$output_list,
        "check_output" = resultado$status,
        "button_class" = "confirmed"))
      
      # internal_VARIABLE_SELECTOR$"pack_input"   = variables_seleccionadas
      # internal_VARIABLE_SELECTOR$"check_input"  = TRUE
      # internal_VARIABLE_SELECTOR$"pack_output"  = variables_seleccionadas
      # internal_VARIABLE_SELECTOR$"check_output" = TRUE
      # internal_VARIABLE_SELECTOR$"button_class" = "confirmed"
      
      # # Guardar las variables seleccionadas
      # valores_internos$pack_var_selection         <- variables_seleccionadas
      # valores_internos$check_var_selection        <- TRUE
      # valores_internos$button_class_var_selection <- "confirmed"
      
      
      if(FALSE){
        # Cambiar el color del botón usando jQuery para asegurar que funcione
        runjs(sprintf("$('#%s').css('border', 'none');", ns("btn_variables")))
        runjs(sprintf("$('#%s').removeClass('btn-primary').addClass('btn-success');", ns("btn_variables")))
        
        showNotification(
          ui = tags$div(
            style = "background-color: #d1e7dd; color: #0f5132; font-size: 15px; font-weight: bold; padding: 10px; border-radius: 4px; border-left: 5px solid #0f5132; display: flex; align-items: center;",
            tags$i(
              class = "fa fa-check-circle",
              style = "font-size: 50px; margin-right: 5px;"  # Tamaño de ícono más grande
            ),
            "Variables seleccionadas exitosamente."
          ),
          duration = 3,
          closeButton = TRUE
        )
        
        
        
        # showNotification(
        #   ui = tags$div(
        #     style = "background-color: #d1e7dd; color: #0f5132; font-size: 15px; font-weight: bold; padding: 10px; border-radius: 4px; border-left: 5px solid #0f5132;",
        #     icon("check-circle"), "Variables seleccionadas exitosamente."
        #   ),
        #   duration = 15,
        #   closeButton = TRUE
        # )
        
        #showNotification("Variables seleccionadas.", type = "default")
        
      }
      removeModal()
    })
    
    return(NULL)
    # Función para restablecer este botón (accesible desde el exterior)
    # return(list(
    #   reset = function() {
    #     runjs(sprintf("$('#%s').css('border', 'none');", ns("btn_variables")))
    #     runjs(sprintf("$('#%s').removeClass('btn-success').addClass('btn-primary');", ns("btn_variables")))
    #   }
    # ))
  })
}
