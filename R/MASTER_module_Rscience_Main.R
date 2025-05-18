
#' @export
MASTER_module_Rscience_Main_ui <- function(id) {
  ns <- NS(id)
  # Contenido principal organizado en columnas para mantener los cards separados
  div(
    tags$head(
    tags$style(HTML("
       /* Establecer altura y centrado para todos los card headers */
      .card-header {
        height: 60px !important;
        display: flex !important;
        align-items: center !important;
      }
    "))
  ),
    # Usamos card() para envolver todo el contenido
    card(
      # Añadimos un card_header explícito
      card_header(
        h4("Rscience", class = "btn-sidebar")
      ),
      layout_sidebar(
        sidebar = sidebar(
          p("HOLA", class = "text-center fs-4 fw-bold py-4"),
          open = "closed"
        ),
        # Encabezado con botones en una fila
        div(
          class = "d-flex",  # Contenedor flexible
          style = "height: 80vh;",  # Altura del contenedor principal (80% de la ventana)
          # Primer bloque - 3/12
          div(
            style = "flex: 0 0 12.5%; max-width: 12.5%; padding: 10px; height: 100%;",  # Ancho del 25%, alto del 100%
            uiOutput(ns("card01_botonera_inicial"))
          ),
          div(
            style = "flex: 0 0 87.5%; max-width: 87.5%; padding: 10px; height: 100%;",# overflow-y: auto;",  # Altura del 100%
            bslib::navset_card_tab(
              title = "R for Science",
              height = "100%",  # Especificar altura explícitamente
              bslib::nav_panel("User selection", 
                               uiOutput(ns("card02_user_selection"))
              ),
              bslib::nav_panel("Outputs", 
                               uiOutput(ns("card03_botonera_output"))
              )
            )
          )
        )
      )
    )
  )
}


#' @export
MASTER_module_Rscience_Main_server <-  function(id, show_dev) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    default_structure <- list(
      pack_input = "",
      check_input = FALSE,
      pack_output = "",
      check_output = FALSE,
      button_class = "initial"
    )    
    
    my_selected_tool <- reactiveVal(NULL)
    
   
    
    internal_DATASET_SELECTOR <- do.call(reactiveValues, default_structure)
    active_DATASET_SELECTOR   <- do.call(reactiveValues, default_structure)
    
    internal_TOOLS_SELECTOR <- do.call(reactiveValues, default_structure)
    active_TOOLS_SELECTOR   <- do.call(reactiveValues, default_structure)
    
    observe({
      my_selected_tool(NULL)
      valores_internos_list <- reactiveValuesToList(internal_TOOLS_SELECTOR)
      
      info_output <- valores_internos_list$"pack_output"
      
      req(info_output)
      
      my_selected_tool(info_output$"selected_tool")
      
    })
    
    internal_VARIABLE_SELECTOR <- do.call(reactiveValues, default_structure)
    active_VARIABLE_SELECTOR   <- do.call(reactiveValues, default_structure)
    
    internal_PLAY_SELECTOR    <- do.call(reactiveValues, default_structure)
    active_PLAY_SELECTOR    <- do.call(reactiveValues, default_structure)
    
    
    Sbutton_01_dataselector2_server(id = "dataset_selector2", internal_DATASET_SELECTOR)
    
    Sbutton_02_tools2_server(id = "tools_selector2", internal_DATASET_SELECTOR, internal_TOOLS_SELECTOR)
    
    
    my_list_str_rv <- reactive({
      
      valores_internos_list <- reactiveValuesToList(internal_TOOLS_SELECTOR)
      info_output <- valores_internos_list$"pack_output"
      req(info_output)
      
      str_selected_tool <- info_output$"selected_tool"
      
      the_str_list <- list(
        str_01_MM_variable_selector = "_MM_variable_selector",
        str_02_FN_validate_vars =     "_FN_validate_vars",
        str_03_FN_zocalo =            "_FN_shiny_zocalo",
        str_04_MM_output =            "_MM_output"
        
        
      )
      vector_names <- names(the_str_list)
      
      the_str_list <- lapply(the_str_list, function(x){paste0(str_selected_tool, x)})
      names(the_str_list) <- vector_names
      the_str_list
      
    })
    
    Sbutton_03_variable_selector2_server(id = "variable_selector2", 
                                         my_list_str_rv, 
                                         internal_DATASET_SELECTOR, 
                                         internal_TOOLS_SELECTOR, internal_VARIABLE_SELECTOR)
    
    Sbutton_reset2_server(id = "reset2", default_structure, 
                          internal_DATASET_SELECTOR,  active_DATASET_SELECTOR,
                          internal_TOOLS_SELECTOR,    active_TOOLS_SELECTOR,
                          internal_VARIABLE_SELECTOR, active_VARIABLE_SELECTOR,
                          internal_PLAY_SELECTOR,     active_PLAY_SELECTOR)
    
    
    Sbutton_play2_server(id = "play2", 
                         internal_DATASET_SELECTOR,  active_DATASET_SELECTOR,
                         internal_TOOLS_SELECTOR,    active_TOOLS_SELECTOR,
                         internal_VARIABLE_SELECTOR, active_VARIABLE_SELECTOR,
                         internal_PLAY_SELECTOR,     active_PLAY_SELECTOR)
    
    
    #my_selected_tool <- reactiveVal(NULL)

    ############################################################################
    
    output$card01_botonera_inicial <- renderUI({
      div(
        style = "height: 100%;",  # Altura del contenedor (100% del contenedor padre)
        card(
          style = "height: 100%;",  # Altura de la card (100% del contenedor padre)
          card_header("Main menu"),
          card_body(
            div(
              class = "d-flex flex-column align-items-center",  # Para centrar horizontalmente
              style = "gap: 20px; height: 100%;",  # Altura del cuerpo de la card (100%)
              Sbutton_01_dataselector2_ui(ns("dataset_selector2")), 
              Sbutton_02_tools2_ui(id = ns("tools_selector2")),
              Sbutton_03_variable_selector2_ui(id = ns("variable_selector2")),
              Sbutton_reset2_ui(id = ns("reset2")),
              Sbutton_play2_ui(id = ns("play2"))
            )
          )
        )
      )
    })
    
    
    ############################################################################
    
    output$tarjeta01_dataset <- renderUI({
      valores_internos_list <- reactiveValuesToList(internal_DATASET_SELECTOR)
      
      info_output <- valores_internos_list$"pack_output"
      Rscience.import::fn_infoUI_zocalo_dataset(data_obj = info_output)
      
    })
    
    output$tarjeta02_tools <- renderUI({
      valores_internos_list <- reactiveValuesToList(internal_TOOLS_SELECTOR)
      
      info_output <- valores_internos_list$"pack_output"
      req(info_output)
      
      # print(info_output)
      df_tool_selection <- info_output$df_tool_selection
      
      Rscience.menu::fn_infoUI_zocalo_tools(df_data_obj = df_tool_selection) 
      
    })
    
    output$agregado_tools <- renderUI({
      req(my_selected_tool())
      # mi_selected_tool()
      paste0("Elegido: ", my_selected_tool())
      
    })
    
    output$tarjeta03_vars <- renderUI({
      
      req(my_list_str_rv())
      # print(my_list_str_rv())
      
      str_selected_tool <- my_list_str_rv()$"str_03_FN_zocalo"
      # Nombre de la función como string
      # str_selected_tool <- "GeneralLM_fix_anova1_FN_shiny_zocalo"
      
      # Argumentos
      args <- list(internal_VARIABLE_SELECTOR = internal_VARIABLE_SELECTOR)
      
      vector_funciones <- ls("package:Rscience.GeneralLM")
      
      # Verificar si la función existe y ejecutarla
      if (str_selected_tool %in% vector_funciones ) {
        resultado <- do.call(str_selected_tool, args)
        resultado
        # print(resultado)  # Output: 5
      } else {
        print("La función no existe.")
      }
      
      
      
    })
    
    
    output$card02_user_selection <- renderUI({
      div(
        style = "height: 100%; display: flex;",  # Altura del contenedor (100% del contenedor padre)
        div(
          style = "flex: 1 1 50%; max-width: 50%; padding: 10px; box-sizing: border-box; height: 100%;",  # Altura del 100%
          uiOutput(ns("tarjeta01_dataset")),
          uiOutput(ns("agregado_tools")),
          uiOutput(ns("tarjeta02_tools"))
        ),
        div(
          style = "flex: 1 1 50%; max-width: 50%; height: 100%; overflow-y: auto; padding: 10px; box-sizing: border-box;",  # Altura del 100%
          uiOutput(ns("tarjeta03_vars"))
        )
      )
    })
    
    
    ############################################################################
    
    
    # GeneralLM_fix_anova1_MM_output_server(id = "the_output")

    # output$card03_botonera_output   <- renderUI({
    #   GeneralLM_fix_anova1_MM_output_ui(id = ns("aver"))
    # })
    
    # str_server <- reactiveVal(NULL)
    # str_ui <- reactiveVal(NULL)
    # my_id <- reactiveVal(NULL)
# 
    observe({

      req(my_list_str_rv())
      str_selected_modulo <- my_list_str_rv()$"str_04_MM_output"
      new_server <- paste0(str_selected_modulo, "_server")
      new_ui <- paste0(str_selected_modulo, "_ui")
      new_id <- "the_output"

      print(new_server)
      # str_server(new_server)
      # str_ui(new_ui)
      # my_id(new_id)
      args <- list(id = new_id, show_dev = FALSE)

      vector_funciones <- ls("package:Rscience.GeneralLM")
      check_in <- new_server %in% vector_funciones
      print(check_in)

      # print(str_server())
      # Verificar si la función existe y ejecutarla
      if (check_in) {
        do.call(new_server, args)
        # print(resultado)  # Output: 5
      } else {
        print("El modulo no existe.")
      }


    })
#     
    
    ok_show_all <- reactive({
      req(active_DATASET_SELECTOR, active_TOOLS_SELECTOR, 
          active_VARIABLE_SELECTOR, active_PLAY_SELECTOR)
      
      req(active_DATASET_SELECTOR$"check_output", 
          active_TOOLS_SELECTOR$"check_output",
          active_VARIABLE_SELECTOR$"check_output",
          active_PLAY_SELECTOR$"check_output")
      
      return(TRUE)
      
    })
    
    # # Renderizar la UI del selector de variables
    output$card03_botonera_output <- renderUI({
      req(my_list_str_rv(), ok_show_all())
      
      str_selected_modulo <- my_list_str_rv()$"str_04_MM_output"
      new_modulo_ui <- paste0(str_selected_modulo, "_ui")
      new_id <- "the_output"
      
      print(new_modulo_ui)
      args <- list(id = ns(new_id))
      
      div(
        style = "height: 100%;",  # Altura del contenedor (100% del contenedor padre)
        do.call(new_modulo_ui, args)  # Altura del contenido (100% del contenedor padre)
      )
    })
    
    # 
    # 
    ############################################################################
  })
}