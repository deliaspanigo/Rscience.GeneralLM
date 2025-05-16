
#' @export
MASTER_module_Rscience_Main_ui <- function(id) {
  ns <- NS(id)
  # Contenido principal organizado en columnas para mantener los cards separados
  div(
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
          class = "d-flex flex-wrap",
          # Primer bloque - 3/12
          div(
            style = "flex: 0 0 12.5%; max-width: 12.5%;",
            uiOutput(ns("card01_botonera_inicial"))
          ),
          # Segundo bloque - 3/12
          div(
            style = "flex: 0 0 25%; max-width: 25%;",
            uiOutput(ns("card02_user_selection"))
          ),
          # Tercer bloque - 6/12
          div(
            style = "flex: 0 0 62.5%; max-width: 62.5%;",
            uiOutput(ns("card03_botonera_output"))
          )
        )
        ,
        
        #uiOutput(ns("mega_tabs")), br(),
        uiOutput(ns("show_dev_full")),
        uiOutput(ns("mensaje_seleccion"))
        
        
        
        # Card separado para Quarto
        # div(
        #   style = "margin-top: 20px; width: 100%;",
        #   ""
        #   # quartoRendererUI(id = "quarto_doc")
        # )
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
    
    internal_DATASET_SELECTOR <- do.call(reactiveValues, default_structure)
    active_DATASET_SELECTOR   <- do.call(reactiveValues, default_structure)
    
    internal_TOOLS_SELECTOR <- do.call(reactiveValues, default_structure)
    active_TOOLS_SELECTOR   <- do.call(reactiveValues, default_structure)
    
    internal_VARIABLE_SELECTOR <- do.call(reactiveValues, default_structure)
    active_VARIABLE_SELECTOR   <- do.call(reactiveValues, default_structure)
    
    internal_PLAY_SELECTOR    <- do.call(reactiveValues, default_structure)
    active_PLAY_SELECTOR    <- do.call(reactiveValues, default_structure)
    
    
    Sbutton_01_dataselector2_server(id = "dataset_selector2", internal_DATASET_SELECTOR)
    
    Sbutton_02_tools2_server(id = "tools_selector2", internal_DATASET_SELECTOR, internal_TOOLS_SELECTOR)
    
    Sbutton_03_variable_selector2_server(id = "variable_selector2", internal_DATASET_SELECTOR, 
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
    ############################################################################
    
    output$card01_botonera_inicial <- renderUI({
      card(
        card_header("Main menu"),
        card_body(
          div(
            class = "d-flex flex-column align-items-center", # Para centrar horizontalmente
            style = "gap: 20px;",
            Sbutton_01_dataselector2_ui(ns("dataset_selector2")), 
            Sbutton_02_tools2_ui(id = ns("tools_selector2")),
            Sbutton_03_variable_selector2_ui(id = ns("variable_selector2")),
            Sbutton_reset2_ui(id = ns("reset2")),
            Sbutton_play2_ui(id = ns("play2"))
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
    
    output$tarjeta03_vars <- renderUI({
      
      valores_list_variable_selector <- reactiveValuesToList(internal_VARIABLE_SELECTOR)
      info_VS <- valores_list_variable_selector$"pack_output"
      req(info_VS)
      
      # print(info_output)
      
      value_factor <- info_VS$"factor"
      value_rv <-     info_VS$"respuesta"
      vector_selected_vars <- info_VS$"vector_selected_vars"
      new_ncol <-  info_VS$"ncol_minidataset"
      new_nrow <-  info_VS$"nrow_minidataset"

      div(
        class = "p-2 rounded",
        style = "background-color: rgba(255, 193, 7, 0.05); border-left: 4px solid #ffc107;",

        h5(class = "text-warning", icon("table-cells", class = "me-2"), "Selected variables"),

      div(
          class = "mt-2",
          span(class = "fw-bold", "All variables: "),
          div(
            class = "mt-1",
            lapply(vector_selected_vars, function(var) {
              span(
                class = "badge bg-light text-dark me-1 mb-1",
                style = "border: 1px solid #dee2e6; padding: 5px;",
                var
              )
            })
          )
        ),
        div(
          class = "d-flex flex-column",
          div(class = "me-4 mb-2",
              span(class = "fw-bold", "Factor: "),
              span(value_factor, style = "font-family: monospace;")),

          div(class = "me-4 mb-2",
              span(class = "fw-bold", "Response Variable: "),
              span(value_rv, style = "font-family: monospace;"))
        ),


        div(
          span(class = "fw-bold", "Shape: "),
          span(paste0(new_nrow, " rows × ", new_ncol, " columns"),
               style = "font-family: monospace;")
        )
      )
      
    })
    
    
    output$card02_user_selection <- renderUI({
      # valores_internos_list <- reactiveValuesToList(valores_internos)
      # req(valores_internos_list)
      
      card(
        card_header("User selection"),
        card_body(
          uiOutput(ns("tarjeta01_dataset")),
          uiOutput(ns("tarjeta02_tools")),
          uiOutput(ns("tarjeta03_vars"))
        )
      )
      
      
      # # Contenedor principal
      # div(
      #   # class = "p-3 rounded shadow-sm",
      #   # style = "background: linear-gradient(to right, #f8f9fa, #ffffff);",
      #   
      #   # Título principal
      #   h4(
      #     # class = "mb-3 pb-2",
      #     # style = "border-bottom: 2px solid #0d6efd; color: #0d6efd;",
      #     # icon("info-circle"), 
      #     "Resumen de configuración"
      #   ),
      #  
      #   
      #   
      # )
      
    })
    
    
    ############################################################################
  })
}