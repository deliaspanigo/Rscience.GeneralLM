
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
  # Contenedor que ocupa toda la ventana
  div(
    style = "height: 100vh; display: flex; flex-direction: column; overflow-y: hidden;",
    # La tarjeta ocupa todo el espacio disponible

      layout_sidebar(
        sidebar = sidebar(
          id = "sidebar_izquierdo",
          title = h1("Main menu", style = "font-weight: bold;"),
          open = "open",
          div(
            style = "overflow-y: auto; padding-right: 10px; height: 100%;",
            div(
              uiOutput(ns("the_toggle")),
              conditionalPanel(
                condition = "input.toggle == false",
                ns = ns,
                uiOutput(ns("main_menu_input"))
              ),
              conditionalPanel(
                condition = "input.toggle == true",
                ns = ns,
                uiOutput(ns("main_menu_output"))
              )
            )
          )
        ),
        # Resto del layout. Para que ocupe todo el espacio, también usa flex y height
        div(
          # style = "overflow-y: hidden; margin-top: 5px; flex: 1; display: flex; flex-direction: column; min-height: 100%",
          style = "overflow-y: hidden; flex: 1; display: flex; flex-direction: column; min-height: 100%;",
            # Estos contenidos ahora llenan el espacio restante
            conditionalPanel(
              condition = "input.toggle == false",
              ns = ns,
              uiOutput(ns("soft_visual_input"))
            ),
            conditionalPanel(
              condition = "input.toggle == true",
              ns = ns,
              uiOutput(ns("soft_visual_output")),
              uiOutput(ns("soft_visual_output22"))
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
    
    
    message(crayon::green("**********************************************"))
    message(crayon::green("*                                            *"))
    message(crayon::green("*          WELCOME TO THE RSCIENCE WORLD!    *"))
    message(crayon::green("*                                            *"))
    message(crayon::green("**********************************************"))
    
    
    
    # Obtener fecha y hora actual
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    
    # Crear el mensaje en inglés
    mensaje <- paste0(
      crayon::blue("Welcome to Rscience!"), "\n",
      crayon::yellow("Current date and time:"), " ", crayon::green(timestamp)
    )
    
    # Mostrar en la consola con el color
    cat(mensaje, "\n")
    ############################################################################
    
    output$the_toggle <- renderUI({
      # Toggle estilo R/Python
      # Agregar CSS personalizado para los colores del toggle
      div(
      tags$head(
        tags$style(HTML("
      /* Estilo para el toggle */
      .form-check-input {
        background-color: #4c78dd !important; /* Color azul para R (por defecto) */
        border-color: #4c78dd !important;
        width: 3.5em !important; /* Aumentar el ancho del toggle */
        height: 1.8em !important; /* Aumentar la altura proporcionalmente */
      }
      
      /* Estilo cuando está activado (Python) */
      .form-check-input:checked {
        background-color: #4CAF50 !important; /* Color verde para Python */
        border-color: #4CAF50 !important;
      }
      
      /* Asegurar que la transición sea suave */
      .form-check-input {
        transition: background-color 0.3s, border-color 0.3s;
      }
      
      /* Ajustar el círculo indicador dentro del toggle */
      .form-switch .form-check-input:after {
        height: calc(1.8em - 4px) !important;
        width: calc(1.8em - 4px) !important;
      }
      
      /* Ajustar el espacio del contenedor */
      .form-switch {
        padding-left: 0 !important;
      }
    "))
      ),
      div(
        class = "d-flex align-items-center justify-content-between gap-2 mb-3",
        span("   ", class = "fw-bold"),
        tags$div(
          class = "form-check form-switch",
          tags$input(
            id = ns("toggle"),
            type = "checkbox",
            class = "form-check-input",
            role = "switch"
          )
        ),
        # span("Python", class = "fw-bold"),
        uiOutput(ns("toggle_state"), inline = TRUE)
      )
      )
    })
    
    # Muestra "input" o "output" según el estado del toggle
    output$toggle_state <- renderUI({
      the_selection <- ifelse(test = input$toggle, yes = "HELP!", no = "input")
      span(the_selection, class = "fw-bold")
    })
    
    # Main menu
    output$main_menu_input <- renderUI({
      
      # req(isFALSE(input$toggle))
      
      div(
        style = "height: 100%;",  # Altura del contenedor (100% del contenedor padre)
        card(
          style = "height: 100%;",  # Altura de la card (100% del contenedor padre)
          # card_header("Main menu"),
          # card_body(
            div(
              class = "d-flex flex-column align-items-center",  # Para centrar horizontalmente
              style = "gap: 20px; height: 100%;",  # Altura del cuerpo de la card (100%)
              Sbutton_01_dataselector_ui(ns("dataset_selector2")), 
              Sbutton_02_tools_ui(id = ns("tools_selector2")),
              Sbutton_03_settings_ui(id = ns("settings")),
              Sbutton_reset2_ui(id = ns("reset2")),
              Sbutton_04_play_ui(id = ns("play2"))
            )
          # )
        )
      )
    })
    

    
    output$main_menu_output <- renderUI({
      

      
      div(
        style = "height: 100%;",  # Altura del contenedor (100% del contenedor padre)
        card(
          style = "height: 100%;",  # Altura de la card (100% del contenedor padre)
          # card_header("Main menu"),
          # card_body(
          div(
            class = "d-flex flex-column align-items-center",  # Para centrar horizontalmente
            style = "gap: 20px; height: 100%;",  # Altura del cuerpo de la card (100%)
            actionButton(
              ns("btn_output_report"),
              tagList(
                icon("file-alt", style = "font-size: 75px; display: block; margin-bottom: 8px;"),
                span()
              ),
              # class = btn_class, 
              style = "height: 100px; width: 140px; display: flex; flex-direction: column; justify-content: center; align-items: center; font-size: 14px;",
              title = "Report"
            )
          ),
          div(
            class = "d-flex flex-column align-items-center",  # Para centrar horizontalmente
            style = "gap: 20px; height: 100%;",  # Altura del cuerpo de la card (100%)
            actionButton(
              ns("btn_output"),
              tagList(
                icon("file-alt", style = "font-size: 75px; display: block; margin-bottom: 8px;"),
                span()
              ),
              # class = btn_class, 
              style = "height: 100px; width: 140px; display: flex; flex-direction: column; justify-content: center; align-items: center; font-size: 14px;",
              title = "Import dataset"
            )
          ),
          div(
            class = "d-flex flex-column align-items-center",  # Para centrar horizontalmente
            style = "gap: 20px; height: 100%;",  # Altura del cuerpo de la card (100%)
            actionButton(
              ns("btn_dataset55"),
              tagList(
                icon("database", style = "font-size: 75px; display: block; margin-bottom: 8px;"),
                span()
              ),
              # class = btn_class, 
              style = "height: 100px; width: 140px; display: flex; flex-direction: column; justify-content: center; align-items: center; font-size: 14px;",
              title = "Import dataset"
            )
          )
          # )
        )
      )
    })
    
    output$"soft_visual_input" <- renderUI({
      

      bslib::navset_card_tab(
        title = h1("R for science", style = "font-weight: bold;"),
        id = ns("mynav"),  # Con ns() porque estás en un módulo
        height = "100%",  # Especificar altura explícitamente
        
        # bslib::nav_panel(title = "cy_01_totem",
        #                  uiOutput(ns("cy_01_totem"))
        # ),
        # bslib::nav_panel(title = "cy_02_internal",
        #                  uiOutput(ns("cy_02_internal"))
        # ),
        # bslib::nav_panel(title = "cy_03_temp",
        #                  uiOutput(ns("cy_03_temp"))
        # ),
        bslib::nav_panel(title = "user_selection",
                         uiOutput(ns("card01_user_selection"))
        ),
        bslib::nav_panel(title = "dataset",
                         uiOutput(ns("card02_dataset"))
        ),
        # bslib::nav_panel(title = "theory",
        #                  module_extra_theory_ui(id = ns("extra_theory"))
        # ),
        # bslib::nav_panel(title = "download",
        #                  uiOutput(ns("card04_download"))
        # ),
        bslib::nav_panel(title = "report",
                         uiOutput(ns("card03_report"))
        ),
        bslib::nav_panel(title = "script",
                         uiOutput(ns("card04_script"))
        ),
        bslib::nav_panel(title = "full_output",
                         uiOutput(ns("card05_full_output"))
        )
        
      )
    })
    
   
    
    
    ############################################################################
    
    # List steps
    # 1) Initial OK!
    # 2) Import tool options OK!
    # 3) Import dataset    
    # 4) Tool selection
    # 5) Config
    # 6) Choosing settings...
    # 8) Press PLAY and pass internal to active
    # 9) Create temporal folder
    # 10) Make script for user and quarto
    # 11) Run report
    # 12) Download
    # -----------------------------------------
    # Theory
    # Reset
    
    # Module Extra - Theory ----------------------------------------------------
    module_extra_theory_server(id = "extra_theory")
    # --------------------------------------------------------------------------
    
    # Initial values
    number_current_step  <- reactiveVal(1)
    APP_TOTEM <- reactiveValues()
    STR_STEP_NAME <- "step"
    
    default_list_step <- list(
      "current_step" = NA,
      "current_label" = NA,
      "key" = NA, 
      "check_previous" = FALSE,
      "pack_output" = "",
      "check_output" = FALSE,
      "button_state" = "initial",
      "the_time"           = "",
      "error_message" = ""
    )
    
    
    default_list_button <- list(
      "pack_output"  = "",
      "check_output" = FALSE,
      "button_state" = "initial")

    # --------------------------------------------------------------------------
    
    # --------------------------------------------------------------------------

    # Reset button 
    Sbutton_reset2_server(id = "reset2", number_current_step, default_list_button, 
                          internal_DATASET_SELECTOR, internal_TOOLS_SELECTOR, internal_CFG,
                          internal_SETTINGS,
                          internal_PLAY)

    # --------------------------------------------------------------------------
    # --------------------------------------------------------------------------
    # --------------------------------------------------------------------------
    
    

    # List steps
    # Step 01) Initial ---------------------------------------------------------
    step01_id <- paste0(STR_STEP_NAME, "01")
    module_step01_init_server(id = step01_id, step_pos = 1, 
                              current_label = "Step 01 - Initial",     
                              str_key = "initial",
                              number_current_step, STR_STEP_NAME, default_list_step, APP_TOTEM)
    
    # Step 02) Upload ----------------------------------------------------------
    module_step02_upload_server(id = "step02", step_pos = 2, 
                                number_current_step, STR_STEP_NAME, default_list_step, APP_TOTEM)
    
    # Step 03) Dataset Selector ------------------------------------------------
    ### Internal - dataset selector
    internal_DATASET_SELECTOR <- do.call(reactiveValues, default_list_button)
    
    ### Button and options - Data selector
    Sbutton_01_dataselector_server(id = "dataset_selector2", step_pos = 3, number_current_step, internal_DATASET_SELECTOR)
    
    ### Modulo - Data selector
    module_step03_import_dataset_server(id = "step03", step_pos = 3,
                                number_current_step, STR_STEP_NAME, default_list_step, APP_TOTEM, internal_DATASET_SELECTOR)


    # Step 04) Tool Selector ---------------------------------------------------
    ### Internal - Tools Selector
    internal_TOOLS_SELECTOR <- do.call(reactiveValues, default_list_button)

    ### Button and options - Tools selector
    Sbutton_02_tools_server(id = "tools_selector2", step_pos = 4, number_current_step, internal_DATASET_SELECTOR, internal_TOOLS_SELECTOR)
    
    ### Reactive value for selected tool
    MY_SELECTED_TOOL <- reactiveVal(NULL)
    observe({
      req(internal_TOOLS_SELECTOR, internal_TOOLS_SELECTOR)
      req(internal_TOOLS_SELECTOR$"check_output")
      
      MY_SELECTED_TOOL(NULL)
      valores_internos_list <- reactiveValuesToList(internal_TOOLS_SELECTOR)
      
      info_output <- valores_internos_list$"pack_output"
      
      req(info_output)
      
      MY_SELECTED_TOOL(info_output$"selected_tool")
    })
    
    ### Modulo - Tools selector
    module_step04_tools_server(id = "step04", step_pos = 4,
                                        number_current_step, STR_STEP_NAME, default_list_step, APP_TOTEM, internal_TOOLS_SELECTOR)
    
    
    # Step 05) CFG from selected tool ------------------------------------------
    ### Internal - CFG
    internal_CFG <- do.call(reactiveValues, default_list_button)
    
    ### Modulo - CFG
    module_step05_cfg_server(id = "step05", step_pos = 5,
                           number_current_step, STR_STEP_NAME, default_list_step, 
                           APP_TOTEM, internal_TOOLS_SELECTOR, internal_CFG)
    
    
    # Step 06) Settings --------------------------------------------------------
    ### Internal - Settings
    internal_SETTINGS <- do.call(reactiveValues, default_list_button)
    
    ### Button and options - Settings
    Sbutton_03_settings_server(id = "settings", step_pos = 6, number_current_step, 
                               internal_DATASET_SELECTOR, internal_TOOLS_SELECTOR, 
                               internal_CFG, internal_SETTINGS)
    
    ### Module - Settings
    module_step06_settings_server(id = "step06", step_pos = 6,
                                  number_current_step,
                                  STR_STEP_NAME, default_list_step,
                                  APP_TOTEM, internal_SETTINGS)
    
    # Step 07) Script ----------------------------------------------------------
    ### Module - Script
    module_step07_script_server(id = "step07", step_pos = 7,
                                number_current_step, 
                                STR_STEP_NAME, default_list_step, 
                                APP_TOTEM, internal_DATASET_SELECTOR, internal_CFG, internal_SETTINGS)
   
    
    # Step 08) Play ------------------------------------------------------------
    ### Internal - Play
    internal_PLAY <- do.call(reactiveValues, default_list_button)
    
    ### Button and options - Play
    Sbutton_04_play_server(id = "play2", internal_DATASET_SELECTOR, 
                           internal_TOOLS_SELECTOR, internal_CFG,
                           internal_SETTINGS,
                           internal_PLAY, APP_TOTEM, number_current_step)
    
    ### Module - Play
    module_step08_play_server(id = "step08", step_pos = 8, number_current_step, 
                              STR_STEP_NAME, default_list_step, 
                              APP_TOTEM, internal_CFG, internal_PLAY)
    
    # # Step 9) Download --------------------------------------------------------
    # module_step09_download_server(id = "step09", step_pos = 10, number_current_step,
    #                               STR_STEP_NAME, default_list_step,
    #                               APP_TOTEM, internal_TOOLS_SELECTOR,
    #                               internal_CFG, internal_PLAY)
    
    # --------------------------------------------------------------------------
    # --------------------------------------------------------------------------
    # --------------------------------------------------------------------------
    # --------------------------------------------------------------------------
    
    # --------------------------------------------------------------------------
    # --------------------------------------------------------------------------
    # --------------------------------------------------------------------------
    
       
    # 01 - Crystal 01 - TOTEM ALL
    output$"SUPER_A" <- renderPrint({

      # "current_step" = NA,
      # "current_label" = NA,
      # "check_previous" = FALSE,
      # "check_output" = FALSE,
      # "pack_output" = "",
      # "button_state" = "initial",
      # "error_message" = ""
      #
      number_current_step()
    })
    output$"SUPER_B" <- renderPrint({
      the_list <- reactiveValuesToList(APP_TOTEM)
      the_list <- Filter(Negate(is.null), the_list)
      the_list <- lapply(the_list, function(x){
        x$"pack_output" <- ""
        rbind.data.frame(x)
      })
      the_list
      the_df <- do.call(rbind.data.frame, the_list)
      selected_cols <- c("current_step", "current_label", "key", "button_state", "the_time")
      the_df[selected_cols]
    })
    output$"dynamic_tabs" <- renderUI({
      the_list <- reactiveValuesToList(APP_TOTEM)
      the_list <- Filter(Negate(is.null), the_list)
      if(!is.null(the_list[["step9"]])) the_list[["step9"]]$"pack_output" <- "Silenced!"
      
      # Creamos las pestañas y sus contenidos de forma dinámica
      tabs <- lapply(1:length(the_list), function(selected_pos) {
        
        list_name <- names(the_list)[selected_pos]
        # Obtenemos el contenido de la lista actual (que ahora es una lista anidada)
        list_content <- the_list[[list_name]]
        selected_label <- list_content$"current_label"
        
        # Convertimos la lista a formato de texto para mostrar
        list_text <- capture.output(str(list_content, max.level = 3))
        
        # Para cada lista, crear una pestaña con su contenido
        nav_panel(
          title = selected_label,
          div(
            h4(paste("Contenido de", list_name)),
            # Mostramos la estructura de la lista anidada
            pre(
              code(
                paste(list_text, collapse = "\n")
              )
            )
          )
        )
      })
      
      # Devolver el conjunto de pestañas
      navset_tab(
        id = "tab_panel",
        !!!tabs  # Desempaquetar la lista de pestañas
      )
    })
    output$"SUPER_C" <- renderPrint({
      the_list <- reactiveValuesToList(APP_TOTEM)
      the_list <- Filter(Negate(is.null), the_list)
      if(!is.null(the_list[["step9"]])) the_list[["step9"]]$"pack_output" <- "Silenced!"
      
      the_list
    })
    output$"cy_01_totem" <- renderUI({
      div(
        h2("Por iniciar"),
        verbatimTextOutput(ns("SUPER_A")),
        br(),
        h2("Finalizados"),
        verbatimTextOutput(ns("SUPER_B")),
        br(),
        h2("AVER"),
        uiOutput(ns("dynamic_tabs")),
        br(),
        h2("Finalizados"),
        verbatimTextOutput(ns("SUPER_C"))
      )
    })
    # --------------------------------------------------------------------------

    # 02 - Crystal 02 - Internal ALL
    output$"crystal02_internal_DATASET_SELECTOR" <- renderPrint({
      the_list <- list(
        "la1" = reactiveValuesToList(internal_DATASET_SELECTOR)
      )
      
      the_list
    })
    output$"cy_02_internal" <- renderUI({
      lista_principal <- list(
        "internal_DATASET_SELECTOR" = reactiveValuesToList(internal_DATASET_SELECTOR),
        "internal_TOOLS_SELECTOR" = reactiveValuesToList(internal_TOOLS_SELECTOR),
        "internal_CFG" = reactiveValuesToList(internal_CFG),
        "internal_SETTINGS" = reactiveValuesToList(internal_SETTINGS),
        "internal_PLAY" = reactiveValuesToList(internal_PLAY)
      )
      lapply(names(lista_principal), function(nom) {
        tags$div(
          tags$h4(nom),  # título para identificar cada sección
          tags$pre(
            paste(capture.output(str(lista_principal[[nom]])), collapse = "\n")
          ),
          hr()  # línea separadora
        )
      })
    })
    # --------------------------------------------------------------------------

    # 03 - Crystal 03 - Work and Delivery
    output$"crystal03_01_work" <- renderPrint({
      the_list <- reactiveValuesToList(APP_TOTEM)
      the_output <-  the_list[["step8"]]$"pack_output"
      
      path_folder_work <- the_output$"path_folder_work"
      check_folder_work <- the_output$"check_folder_work" 
      vector_list_files <- list.files(path = path_folder_work, all.files = T, full.names = T, recursive = T)
      
      list_show <- list(path_folder_work = path_folder_work,
                        check_folder_work = check_folder_work,
                        vector_list_files = vector_list_files)
      list_show
    })
    output$"crystal03_02_output" <- renderPrint({
      the_list <- reactiveValuesToList(APP_TOTEM)
      the_output <-  the_list[["step8"]]$"pack_output"
      
      path_folder_output <- the_output$"path_folder_output"
      check_folder_output <- the_output$"check_folder_output" 
      vector_list_files <- list.files(path = path_folder_output, all.files = T, full.names = T, recursive = T)
      
      list_show <- list(path_folder_work = path_folder_output,
                        check_folder_work = check_folder_output,
                        vector_list_files = vector_list_files)
      list_show
    })
    output$"cy_03_temp" <- renderUI({
      
      div(
        fluidPage(
          h2("Work folder"),
          verbatimTextOutput(ns("crystal03_01_work"))
        ),
        fluidPage(
          h2("Output folder"),
          verbatimTextOutput(ns("crystal03_02_output"))
        )
      )
        
      
    })
    

    ############################################################################
    ############################################################################
    ############################################################################
    
    # Card 01) User selection
    output$card01_user_selection <- renderUI({
      div(
        style = "display: flex;",  # Altura del contenedor (100% del contenedor padre)
        div(
          style = "flex: 1 1 50%; max-width: 50%; padding: 10px; box-sizing: border-box;",  # Altura del 100%
          uiOutput(ns("tarjeta01_dataset")),
          uiOutput(ns("agregado_tools")),
          uiOutput(ns("tarjeta02_tools"))
        ),
        div(
          style = "flex: 1 1 50%; max-width: 50%; ; padding: 10px; box-sizing: border-box;",  # Altura del 100%
          uiOutput(ns("tarjeta03_vars"))
        )
      )
    })
    
    output$tarjeta01_dataset <- renderUI({
      req(internal_DATASET_SELECTOR)
      valores_internos_list <- reactiveValuesToList(internal_DATASET_SELECTOR)
      
      info_output <- valores_internos_list$"pack_output"
      Rscience.import::fn_infoUI_zocalo_dataset(data_obj = info_output)
      
    })
    
    output$tarjeta02_tools <- renderUI({
      req(internal_TOOLS_SELECTOR)
      valores_internos_list <- reactiveValuesToList(internal_TOOLS_SELECTOR)
      
      info_output <- valores_internos_list$"pack_output"
      req(info_output)
      
      df_tool_selection <- info_output$df_tool_selection
      
      Rscience.menu::fn_infoUI_zocalo_tools(df_data_obj = df_tool_selection) 
      
    })
    
    output$agregado_tools <- renderUI({
      req(MY_SELECTED_TOOL())
      # mi_selected_tool()
      paste0("Elegido: ", MY_SELECTED_TOOL())
      
    })
    
    output$tarjeta03_vars <- renderUI({
      req(internal_SETTINGS$"check_output")
      req(internal_CFG$"check_output")
      # mi_super_lista <- reactiveValuesToList(internal_SETTINGS) 
      
      # ------------------------------------------------------------------------
      #
      # Hardcoded
      
      full_name_FN <- internal_CFG$"pack_output"$"zocalo_fn_name"
      # ------------------------------------------------------------------------
      #
      # Running fn for obtain zocalo
      args <- list(internal_VARIABLE_SELECTOR = internal_SETTINGS)
      the_zocalo <- do.call(get(full_name_FN), args)
      the_zocalo
      # ------------------------------------------------------------------------
      
      
      
    })
    
    #---------------------------------------------------------------------------
    
    # Card 02) Dataset
    output$"card02_dataset" <- renderUI({
      
      DT::DTOutput(ns("visual_dataset"))
    })
    # UI - Dataset
    output$visual_dataset <- DT::renderDataTable({
      # Obtener la lista de reactiveValues
      the_list <- reactiveValuesToList(APP_TOTEM)
      
      # Intentar acceder al dataset
      my_dataset <- NULL
      if (!is.null(the_list[[3]]$"pack_output"$"my_dataset")) {
        my_dataset <- the_list[[3]]$"pack_output"$"my_dataset"
      }
      
      # Solo continuar si my_dataset existe y es válido
      req(my_dataset)
      
      # Devuelve el dataset si existe, si no, no muestra nada
      my_dataset
    })
    #---------------------------------------------------------------------------
    
  
      
    # Card 03) Report
    id_report01 <- "f01_report"
    output$"card03_report" <- renderUI({
      
      module_render_01_report_download_ui(id = ns(id_report01))
      
    })
    
    run_render01 <- reactiveVal(FALSE)
    show_internal_modal01 <- reactive({FALSE})
    ALL_DONE_report <- module_render_01_report_download_server(id = id_report01, step_pos = 9, number_current_step, 
                                            STR_STEP_NAME, default_list_step, 
                                            APP_TOTEM, internal_TOOLS_SELECTOR,
                                            internal_CFG, internal_PLAY, 
                                            show_internal_modal = reactive(show_internal_modal01()), 
                                            run_render = reactive(run_render01()))
    
    
    #---------------------------------------------------------------------------
    # Clic en play
    # Abrir modal
    # run_render
    # Confirm file exists
    # Close modal
    #---------------------------------------------------------------------------
    fn_my_modal <- function(){
      showModal(
        modalDialog(
          id = "miModalEspecifico2",  # Asignar un ID al modal
          title = "Processing R code file...",
          # Definición CSS de la animación incluida directamente
          tags$head(
            tags$style("
            @keyframes spin {
              0% { transform: rotate(0deg); }
              100% { transform: rotate(360deg); }
            }
          ")
          ),
          tags$div(
            style = "text-align: center;",
            tags$div(
              class = "spinner",
              style = "border: 4px solid #f3f3f3; border-top: 4px solid #3498db; border-radius: 50%; width: 40px; height: 40px; animation: spin 1s linear infinite; margin: 20px auto;"
            ),
            tags$p("This may take a few moments. Please wait.")
          ),
          footer = NULL,  # No incluir botones en el modal
          easyClose = FALSE  # Evitar que el usuario cierre el modal manualmente
        )
      )
    }
    #---------------------------------------------------------------------------
    
    fn_my_modal01 <- function(){
      showModal(
        div(
        tags$head(
          tags$style(
            HTML("
        .blue-icon {
          color: blue;
        }
        .green-icon {
          color: green;
        }
      ")
          )
        ),
        modalDialog(
          title = "Proceso",
          "Anova 1 Way - Fixed Model - General Linear Models",
          tags$ul(
            tags$li(span(id = "line1_icon", icon("spinner", class = "fas fa-spinner fa-spin blue-icon fa-2x")), " R script"),
            tags$li(span(id = "line2_icon", icon("spinner", class = "fas fa-spinner fa-spin blue-icon fa-2x")), " Plots: 20"),
            tags$li(span(id = "line3_icon", icon("spinner", class = "fas fa-spinner fa-spin blue-icon fa-2x")), " Test: 4"),
            tags$li(span(id = "line4_icon", icon("spinner", class = "fas fa-spinner fa-spin blue-icon fa-2x")), " Auto Analysis")
          ),
          footer = modalButton("Cerrar") # Agrega un botón para cerrar el modal
        )
        )
      )
    }
    SHOW_MODAL <- reactiveVal(NULL)
    observeEvent(SHOW_MODAL(),{
      
      if(SHOW_MODAL()){
        # Mostrar el modal de carga
        # Mostrar el modal de carga con un spinner
        
      }
      
      if(!SHOW_MODAL()){
        shinyjs::delay(2000, {
          removeModal()
        })
        SHOW_MODAL(NULL)
      }
      
    })
    #---------------------------------------------------------------------------
    
    sss <- reactiveVal(0)
    play_activado <- reactive({
      my_list <- reactiveValuesToList(internal_PLAY)
      my_list$"check_output"
    })
    
    #---------------------------------------------------------------------------
    

    
    observe({
      if(sss() == 0){
        if(play_activado()) sss(sss()+1)
      }
      
      if(sss() == 1){
        if(is.null(SHOW_MODAL())){
          # SHOW_MODAL(TRUE)
          updateTabsetPanel(session, "mynav", selected = "report") 
          
          sss(sss()+1)
          
      }
        }

      
      if(sss() == 2){
        req(!run_render01())
        if(!run_render01()){ 
          fn_my_modal01()
            run_render01(TRUE)
         
          sss(sss()+1)
        }
      }
      
      if(sss() == 3){
        req(ALL_DONE_report())
        if(ALL_DONE_report()){ 
          
          sss(sss()+1)
        }
      }
      
      if(sss() == 4){
        # JavaScript para cambiar los iconos después de 3 segundos
        runjs('
      setTimeout(function() {
        $("#line1_icon").html("<i class=\\"fas fa-check-circle green-icon fa-2x\\"></i>");
      }); // 2 segundos

      setTimeout(function() {
        $("#line2_icon").html("<i class=\\"fas fa-check-circle green-icon fa-2x\\"></i>");
      }); // 6 segundos

      setTimeout(function() {
        $("#line3_icon").html("<i class=\\"fas fa-check-circle green-icon fa-2x\\"></i>");
      }); // 9 segundos
      
        setTimeout(function() {
        $("#line4_icon").html("<i class=\\"fas fa-check-circle green-icon fa-2x\\"></i>");
      }); // 9 segundos
    ')
          SHOW_MODAL(FALSE)
          sss(sss()+1)
      }
      
    })    

    #---------------------------------------------------------------------------
    #---------------------------------------------------------------------------
    #---------------------------------------------------------------------------
    #---------------------------------------------------------------------------
    
    id_report02 <- "f02_report"
    output$"card04_script" <- renderUI({

      module_render_02_script_download_ui(id = ns(id_report02))

    })
    
    run_render02 <- reactiveVal(FALSE)
    show_internal_modal02 <- reactive({TRUE})
    ALL_DONE_report02 <- module_render_02_script_download_server(id = id_report02, step_pos = 9, number_current_step,
                                                               STR_STEP_NAME, default_list_step,
                                                               APP_TOTEM, internal_TOOLS_SELECTOR,
                                                               internal_CFG, internal_PLAY,
                                                               show_internal_modal = reactive(show_internal_modal02()),
                                                               run_render = reactive(run_render02()))
    #---------------------------------------------------------------------------
    #---------------------------------------------------------------------------
    #---------------------------------------------------------------------------
    #---------------------------------------------------------------------------
    #---------------------------------------------------------------------------
    
    id_report03 <- "f03_report"
    output$"card05_full_output" <- renderUI({

      module_render_03_full_output_download_ui(id = ns(id_report03))

    })

    run_render03 <- reactiveVal(FALSE)
    show_internal_modal03 <- reactive({TRUE})
    ALL_DONE_report03 <- module_render_03_full_output_download_server(id = id_report03, step_pos = 9, number_current_step,
                                                                 STR_STEP_NAME, default_list_step,
                                                                 APP_TOTEM, internal_TOOLS_SELECTOR,
                                                                 internal_CFG, internal_PLAY,
                                                                 show_internal_modal = reactive(show_internal_modal03()),
                                                                 run_render = reactive(run_render03()))


    
    #---------------------------------------------------------------------------
    if(FALSE){
    # Card 04) Download
    output$card04_download <- renderUI({
    
      # req(my_list_str_rv())
      module_step09_download_ui(id=ns("step09"))
    })

    }
    #---------------------------------------------------------------------------

    
  })
}

