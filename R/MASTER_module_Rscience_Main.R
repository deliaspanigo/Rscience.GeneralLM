
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
  
  div(
    style = "height: 100vh; display: flex; flex-direction: column;",
    card(
      style = "height: 100vh, flex: 1; display: flex; flex-direction: column;",
      card_header(
        h4("Rscience", class = "btn-sidebar")
      ),
      layout_sidebar(
        sidebar = sidebar(
          id = "sidebar_izquierdo",
          title = "Main menu",
          open = "open",
          div(
            style = "overflow-y: auto; padding-right: 10px; margin-top: 0px;",  # Aquí le das altura y scroll independiente
            div(
              uiOutput(ns("the_toggle")),
              conditionalPanel(
                condition = "input.toggle == false", ns = ns,
                uiOutput(ns("main_menu_input"))
              ),
              conditionalPanel(
                condition = "input.toggle == true", ns = ns,
                uiOutput(ns("main_menu_output"))
              )
            )
          )
        ),
        # El resto del layout puede seguir igual
        div(
          style = "height: 100vh, margin-top: 0px;",
          conditionalPanel(
            condition = "input.toggle == false", ns = ns,
            uiOutput(ns("soft_visual_input"))
          ),
          conditionalPanel(
            condition = "input.toggle == true", ns = ns,
            uiOutput(ns("soft_visual_output")),
            uiOutput(ns("soft_visual_output22"))
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
    
    
    message(crayon::green("**********************************************"))
    message(crayon::green("*                                            *"))
    message(crayon::green("*          WELCOME TO THE RSCIENCE WORLD!    *"))
    message(crayon::green("*                                            *"))
    message(crayon::green("**********************************************"))
    
    
    
    # Obtener fecha y hora actual
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    
    # Crear el mensaje en inglés
    mensaje <- paste0(
      blue("Welcome to Rscience!"), "\n",
      yellow("Current date and time:"), " ", green(timestamp)
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
      the_selection <- ifelse(test = input$toggle, yes = "output", no = "input")
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
        title = "R for Science",
        id = "mynav",  # Con ns() porque estás en un módulo
        
        # bslib::nav_panel(title = "cy_01_totem",
        #                  uiOutput(ns("cy_01_totem"))
        # ),
        # bslib::nav_panel(title = "cy_02_internal",
        #                  uiOutput(ns("cy_02_internal"))
        # ),
        # bslib::nav_panel(title = "cy_03_temp",
        #                  uiOutput(ns("cy_03_temp"))
        # ),
        # bslib::nav_panel(title = "cy_04_output",
        #                  uiOutput(ns("cy_04_output"))
        # ),
        bslib::nav_panel(title = "user_selection",
                         uiOutput(ns("card01_user_selection"))
        ),
        bslib::nav_panel(title = "dataset",
                         uiOutput(ns("card02_dataset"))
        ),
        bslib::nav_panel(title = "report",
                         module_step10_04_report_download_ui(id = ns("f04_report"))
        ),
        bslib::nav_panel(title = "theory",
                         module_extra_theory_ui(id = ns("extra_theory"))
        ),
        bslib::nav_panel(title = "download",
                         uiOutput(ns("card04_download"))
        ),
        bslib::nav_panel(title = "experiment",
                         uiOutput(ns("experiment"))
        )
      )
    })
    
 
    
    # button_output <- reactiveVal()
    # observeEvent(input$"btn_output_report", {
    #   button_output("output_report")
    # })
    # output$"soft_visual_output" <- renderUI({
    #   
    #   if(button_output() == "output_report"){
    #   module_step10_04_report_download_ui(id = ns("f04_report"))
    #   } else NULL
    #   
    # })
    # 
    # 
    # output$"soft_visual_output22" <- renderUI({
    #   bslib::navset_card_tab(
    #     title = "R for Science",
    #     id = ns("mynav"),
    #     
    #     
    #     bslib::nav_panel(title = "cy_04_output",
    #                      uiOutput(ns("cy_04_output"))
    #     ),
    #     bslib::nav_panel(title = "output",
    #                      module_step10_04_report_download_ui(id = ns("f04_report"))#uiOutput(ns("card03_output"))
    #     ),
    #     bslib::nav_panel(title = "theory",
    #                      module_extra_theory_ui(id = ns("extra_theory"))
    #     ),
    #     bslib::nav_panel(title = "download",
    #                      uiOutput(ns("card04_download"))
    #     )
    #     
    #   )
    # })
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
    # 7) Make script for user and quarto
    # 10) Execute fn to obtain R objets
    # 11) Download
    # -----------------------------------------
    # Theory
    # Reset
    module_extra_theory_server(id = "extra_theory")

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

    # Button 01 - Import Dataset     
    
    
    # Reset button -------------------------------------------------------------
    Sbutton_reset2_server(id = "reset2", number_current_step, default_list_button, 
                          internal_DATASET_SELECTOR, internal_TOOLS_SELECTOR, internal_CFG,
                          internal_SETTINGS,
                          internal_PLAY)
    
    # List steps
    # Step 01) Initial ---------------------------------------------------------
    module_step01_init_server(id = "step01", step_pos = 1, 
                              number_current_step, STR_STEP_NAME, default_list_step, APP_TOTEM)
    
    # Step 02) Upload ----------------------------------------------------------
    module_step02_upload_server(id = "step02", step_pos = 2, 
                                number_current_step, STR_STEP_NAME, default_list_step, APP_TOTEM)
    
    # Step 03) Dataset Selector ------------------------------------------------
    internal_DATASET_SELECTOR <- do.call(reactiveValues, default_list_button)
    Sbutton_01_dataselector_server(id = "dataset_selector2", step_pos = 3, number_current_step, internal_DATASET_SELECTOR)
    module_step03_import_dataset_server(id = "step03", step_pos = 3,
                                number_current_step, STR_STEP_NAME, default_list_step, APP_TOTEM, internal_DATASET_SELECTOR)


    # Step 04) Tool Selector ---------------------------------------------------
    internal_TOOLS_SELECTOR <- do.call(reactiveValues, default_list_button)
    MY_SELECTED_TOOL <- reactiveVal(NULL)
    Sbutton_02_tools_server(id = "tools_selector2", step_pos = 4, number_current_step, internal_DATASET_SELECTOR, internal_TOOLS_SELECTOR)
    # Observe - MY_SELECTED_TOOL()
    observe({
      req(internal_TOOLS_SELECTOR, internal_TOOLS_SELECTOR)
      req(internal_TOOLS_SELECTOR$"check_output")
      
      MY_SELECTED_TOOL(NULL)
      valores_internos_list <- reactiveValuesToList(internal_TOOLS_SELECTOR)
      
      info_output <- valores_internos_list$"pack_output"
      
      req(info_output)
      
      MY_SELECTED_TOOL(info_output$"selected_tool")
    })
    module_step04_tools_server(id = "step04", step_pos = 4,
                                        number_current_step, STR_STEP_NAME, default_list_step, APP_TOTEM, internal_TOOLS_SELECTOR)
    
    # Step 05) CFG from selected tool ------------------------------------------
    internal_CFG <- do.call(reactiveValues, default_list_button)
    module_step05_cfg_server(id = "step05", step_pos = 5,
                           number_current_step, STR_STEP_NAME, default_list_step, 
                           APP_TOTEM, internal_TOOLS_SELECTOR, internal_CFG)
    
    # Step 06) Settings --------------------------------------------------------
    internal_SETTINGS <- do.call(reactiveValues, default_list_button)
    Sbutton_03_settings_server(id = "settings", step_pos = 6, number_current_step, 
                               internal_DATASET_SELECTOR, internal_TOOLS_SELECTOR, 
                               internal_CFG, internal_SETTINGS)
    
    module_step06_settings_server(id = "step06", step_pos = 6,
                                  number_current_step,
                                  STR_STEP_NAME, default_list_step,
                                  APP_TOTEM, internal_SETTINGS)
    
    # Step 07) Script ----------------------------------------------------------
    module_step07_script_server(id = "step07", step_pos = 7,
                                number_current_step, 
                                STR_STEP_NAME, default_list_step, 
                                APP_TOTEM, internal_DATASET_SELECTOR, internal_CFG, internal_SETTINGS)
   
    
    # Step 08) Play ------------------------------------------------------------
    internal_PLAY <- do.call(reactiveValues, default_list_button)
    Sbutton_04_play_server(id = "play2", internal_DATASET_SELECTOR, 
                           internal_TOOLS_SELECTOR, internal_CFG,
                           internal_SETTINGS,
                           internal_PLAY, APP_TOTEM, number_current_step)
    module_step08_play_server(id = "step08", step_pos = 8, number_current_step, 
                              STR_STEP_NAME, default_list_step, 
                              APP_TOTEM, internal_CFG, internal_PLAY)
    # Step 09) Run R code ------------------------------------------------------
    module_step09_run_Rcode_server(id = "step09", step_pos = 9, number_current_step,
                              STR_STEP_NAME, default_list_step,
                              APP_TOTEM, internal_DATASET_SELECTOR,
                              internal_SETTINGS)
    # Step 10) Download --------------------------------------------------------
    module_step10_download_server(id = "step10", step_pos = 10, number_current_step,
                                  STR_STEP_NAME, default_list_step,
                                  APP_TOTEM, internal_TOOLS_SELECTOR,
                                  internal_CFG, internal_PLAY)
    
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
    

    # --------------------------------------------------------------------------
    #04 - Crystal 04 - All outputs
    output$"cy_04_output" <- renderUI({

 
      
        the_list <- reactiveValuesToList(APP_TOTEM)
        the_output <-  the_list[["step9"]]
        resultados <- the_output$"pack_output"
        
        # Asignar salidas dinámicas
        for (nombre in names(resultados)) {
          if (startsWith(nombre, "plot")) {
            local({
              nm <- nombre
              output[[nm]] <- renderPlotly({ resultados[[nm]]} )  # Corrección de cierre
            })
          } else {
            local({
              nm <- nombre
              output[[nm]] <- renderPrint({ resultados[nm]} )
            })
          }
        }
        
        ui_list <- list()
        for (nombre in names(resultados)) {
          new_ui <- c()
          if (startsWith(nombre, "plot")) {
            new_ui <- div(
              HTML(paste0("<b><u>R plot object:</u></b> ", nombre)),
              shinycssloaders::withSpinner(plotlyOutput(ns(nombre)))
              )
          } else {
            new_ui <- div(
              shinycssloaders::withSpinner(verbatimTextOutput(ns(nombre)))
              )
          }
          
          new_ui<- div(new_ui, hr())
          
          ui_list[[length(ui_list) + 1]] <- new_ui
        }
        do.call(tagList, ui_list)

    })
    # --------------------------------------------------------------------------
    
   
    ############################################################################
    ############################################################################
    ############################################################################
    
    # Card 01) User selection
    output$card01_user_selection <- renderUI({
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
      mi_super_lista <- reactiveValuesToList(internal_SETTINGS) 
      
      # ------------------------------------------------------------------------
      #
      # Hardcoded
      
      full_name_FN <- "GeneralLM_fix_anova1_FN_shiny_zocalo"
      # ------------------------------------------------------------------------
      #
      # Running fn for obtain zocalo
      args <- list(internal_VARIABLE_SELECTOR = internal_SETTINGS)
      the_zocalo <- do.call(full_name_FN, args)
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
      if (!is.null(the_list[[3]]$"pack_output"$"database")) {
        my_dataset <- the_list[[3]]$"pack_output"$"database"
      }
      
      # Solo continuar si my_dataset existe y es válido
      req(my_dataset)
      
      # Devuelve el dataset si existe, si no, no muestra nada
      my_dataset
    })
    #---------------------------------------------------------------------------
    
    # Card 03) output
    crear_outputs_y_ui33 <- function(prefix, mis_valores_reactive, output, ns) {
      
      the_names <- names(mis_valores_reactive())
      the_names <- na.omit(the_names)
      # the_names <- the_names[1:10]
      # Identifico plot y text
      dt_plots <- grepl("^plot", the_names)
      dt_text <- !dt_plots
      
      vector_render <- rep(NA, length(the_names))
      vector_render[dt_plots] <- "plotly"
      vector_render[dt_text] <- "text"
      
      
      # Crea los outputs en bucle, en ámbitos independientes para evitar sobrescrituras
      for (i in seq_along(the_names)) {
        id_output <- paste0(prefix, i)
        obj_name <- the_names[i]
        el_render <- vector_render[i]
        
        # Crear un ámbito local para que cada output sea independiente
        local({
          n <- i
          id <- id_output
          obj <- obj_name
          
          if(el_render == "text"){
            output[[id]] <- renderPrint({
              req(mis_valores_reactive())
              mis_valores_reactive()[obj]
            })
          }
          
          # if(el_render == "plotly"){
          #   output[[id]] <- plotly::renderPlotly({
          #     req(mis_valores_reactive())
          #     mis_valores_reactive()[[obj]]
          #   })
          # }
          
        })
        
      }
      
      # Crear UI dinámicamente
      ui_list <- lapply(seq_along(the_names), function(i) {
        id_output <- paste0(prefix, i)
        el_render <- vector_render[i]
        obj_name <- the_names[i]
        
        if(el_render == "text"){
          list(
            fluidRow(
              #h4(list_objetos[[i]]$"title"),
              verbatimTextOutput(ns(id_output)),
              br()
            )
          )
        }
        # if(el_render == "plotly"){
        #   list(
        #     fluidRow(
        #     HTML(paste0("<b><u>R plot object:</u></b> ", obj_name)),
        #     #h4(list_objetos[[i]]$"title"),
        #     plotlyOutput(ns(id_output)),
        #     br()
        #     )
        #   )
        # }
        
      })
      
      return(do.call(tagList, ui_list))
    }
    output$"card03_output" <- renderUI({
      
      
      
      if(FALSE){
        the_list <- reactiveValuesToList(APP_TOTEM)
        the_output <-  the_list[["step9"]]
        resultados <- the_output$"pack_output"
        
        vector_opt <- names(resultados)
        str_grepl  <- "^plot"
        
        list_groups <- fn_R_separate_vector2list_groups(vector_opt, str_grepl)
        # Asignar salidas dinámicas
        for (nombre in names(resultados)) {
          if (startsWith(nombre, "plot")) {
            local({
              nm <- nombre
              output[[nm]] <- renderPlotly({ resultados[[nm]]} )  # Corrección de cierre
            })
          } else {
            local({
              nm <- nombre
              output[[nm]] <- renderPrint({ resultados[nm]} )
            })
          }
        }
        
        ui_list <- list()
        for (nombre in names(resultados)) {
          new_ui <- c()
          if (startsWith(nombre, "plot")) {
            new_ui <- div(
              HTML(paste0("<b><u>R plot object:</u></b> ", nombre)),
              shinycssloaders::withSpinner(plotlyOutput(ns(nombre)))
            )
          } else {
            new_ui <- div(
              shinycssloaders::withSpinner(verbatimTextOutput(ns(nombre)))
            )
          }
          
          new_ui<- div(new_ui, hr())
          
          ui_list[[length(ui_list) + 1]] <- new_ui
        }
        do.call(tagList, ui_list)
      }
    })
    #---------------------------------------------------------------------------
    
    # Card 04) Download
    output$card04_download <- renderUI({
    
      # req(my_list_str_rv())
      module_step10_download_ui(id=ns("step10"))
    })

    #---------------------------------------------------------------------------
    
    output$"experiment" <- renderUI({
      
      module_extra01_output_ui(id =ns("experiment"))
    })
    
    module_extra01_output_server(id ="experiment", 
                                 APP_TOTEM = APP_TOTEM, 
                                 internal_TOOLS_SELECTOR = internal_TOOLS_SELECTOR)
    ############################################################################
    
    module_step10_04_report_download_server(id ="f04_report", step_pos = 10, number_current_step, 
                                            STR_STEP_NAME, default_list_step, 
                                            APP_TOTEM, internal_TOOLS_SELECTOR,
                                            internal_CFG, internal_PLAY)
    ############################################################################
    if(FALSE){
    
    
 
  
    
    
   
 
    ############################################################################
    
    THE_MODAL <- reactiveVal(NULL)
    
    observeEvent(THE_MODAL(),{
      
      if(THE_MODAL()){
       
      }
      
      if(!THE_MODAL()){
        shinyjs::delay(2000, {
          removeModal()
        })
        THE_MODAL(NULL)
      }
      
    })
    
   
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
   
    
    
    
    ############################################################################

    
    }
    
  })
}

