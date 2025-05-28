
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
              id = ns("mynav"),
              
              
              
              height = "100%",  # Especificar altura explícitamente
              bslib::nav_panel(title = "cy_01_totem",
                               uiOutput(ns("cy_01_totem"))
              ),
              bslib::nav_panel(title = "cy_02_internal",
                                 uiOutput(ns("cy_02_internal"))
              ),
              bslib::nav_panel(title = "cy_04_output",
                               uiOutput(ns("cy_04_output"))
              ),
              bslib::nav_panel(title = "user_selection",
                               uiOutput(ns("card02_user_selection"))
              ),
              bslib::nav_panel(title = "dataset",
                               DT::DTOutput(ns("visual_dataset"))
              ),
              bslib::nav_panel(title = "output",
                               uiOutput(ns("card05_output"))
              ),
              bslib::nav_panel(title = "theory",
                               module_extra_theory_ui(id = ns("extra_theory"))
              ),
              bslib::nav_panel(title = "script",
                               uiOutput(ns("card06_script"))
              ),
              bslib::nav_panel(title = "download",
                               uiOutput(ns("card07_download"))
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
    
    # Main menu
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
              Sbutton_01_dataselector_ui(ns("dataset_selector2")), 
              Sbutton_02_tools_ui(id = ns("tools_selector2")),
              Sbutton_03_settings_ui(id = ns("settings")),
              Sbutton_reset2_ui(id = ns("reset2")),
              Sbutton_04_play_ui(id = ns("play2"))
            )
          )
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
    
    # observe(print(reactiveValuesToList(internal_SETTINGS)))
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
    # Step 09) Run ------------------------------------------------------------
    module_step09_run_Rcode_server(id = "step09", step_pos = 9, number_current_step, 
                              STR_STEP_NAME, default_list_step, 
                              APP_TOTEM, internal_DATASET_SELECTOR,
                              internal_SETTINGS)
    
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
    output$dynamic_tabs <- renderUI({
      the_list <- reactiveValuesToList(APP_TOTEM)
      the_list <- Filter(Negate(is.null), the_list)
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
    # Generar las pestañas y los outputs dinámicamente en un solo renderUI

    
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
    output$cy_04_output <- renderUI({
      
      the_list <- reactiveValuesToList(APP_TOTEM)
      the_output <-  the_list[["step9"]]
      
      if(the_output$"check_output"){
        crear_outputs_y_ui33(prefix = "jajja", mis_valores_reactive = reactive(the_output$"pack_output"), output, ns)    
      } else "NADA"
      
    })
    ############################################################################ 
   
    # $step8$pack_output
    # $step8$pack_output$current_time_pritty
    # [1] "2025-05-28 01:55 40.575"
    # 
    # $step8$pack_output$str_current_time
    # [1] "2025_05_28_01_55_40_575"
    # 
    # $step8$pack_output$path_folder_work
    # [1] "/tmp/RtmpwUPpfD/Rscience/GeneralLM_fix_anova1_2025_05_28_01_55_40_575/folder_work"
    # 
    # $step8$pack_output$check_folder_work
    # [1] TRUE
    # 
    # $step8$pack_output$path_folder_output
    # [1] "/tmp/RtmpwUPpfD/Rscience/GeneralLM_fix_anova1_2025_05_28_01_55_40_575/folder_output"
    # 
    # $step8$pack_output$check_folder_output
    # [1] TRUE
    # 
    # 
    # $step8$check_output
    # [1] TRUE
    
    ############################################################################
    
    # UI - Dataset
    output$visual_dataset <- DT::renderDataTable({
      # Obtener la lista de reactiveValues
      the_list <- reactiveValuesToList(APP_TOTEM)

      # Intentar acceder al dataset
      my_dataset <- NULL
      if (!is.null(the_list[[3]]$"pack_output"$"database")) {
        my_dataset <- the_list[[3]]$"pack_output"$"database"
        print(my_dataset)
      }

      # Solo continuar si my_dataset existe y es válido
      req(my_dataset)
      
      # Devuelve el dataset si existe, si no, no muestra nada
      my_dataset
    })
    
    
    
    # Card 02) "user_selection"
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
      
      # print(info_output)
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
      mi_super_lista <- reactiveValuesToList(internal_SETTINGS) #print(paste0("AVER: ", internal_STR$"pack_output"$"vector_str"$"str_01_MM_variable_selector"))
      
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
    
    ############################################################################
    
    
    output$el_cartel <- renderUI({
      my_cartel <- reactiveValuesToList(internal_TOOLS_SELECTOR)$"pack_output"$"selected_cartel"
      fn_html_cartel(my_text = my_cartel)
    })
    # style = "height: 100%; width: 100%; max-width: 100%; box-sizing: border-box; overflow-x: hidden;",  # Ajustes para evitar el scroll horizontal
    
    
    
    ##############################################################################
    # Tab05 - RCode
    output$shiny_ace_editor_MENU <- renderUI({
      
      # req(Rcode_script())
      #Rcode_script <- GeneralLM_fix_anova1_take_code(my_fn=GeneralLM_fix_anova1_RCode)
      # Calcular la altura adecuada para el editor basado en el número de líneas
      
      
      
      
      fluidRow(
        column(3, 
               selectInput(ns("theme"), "Editor Theme:", 
                           choices = c("xcode", "monokai", "github", "eclipse", "tomorrow", 
                                       "solarized_light", "solarized_dark", "textmate", "twilight"),
                           selected = "solarized_dark")),
        column(3,
               sliderInput(ns("fontSize"), "Font Size:", min = 8, max = 40, value = 14, step = 1)
        ),
        column(3, downloadButton(ns("download_btn"), "Download", icon = icon("download")))
        
      )
      
      
    })
    
    output$shiny_ace_CODE <- renderUI({
      
      req( input$"theme", input$"fontSize")
      
      Rcode_script <- APP_TOTEM[["step7"]]$"pack_output"$"Rcode_script"
        
      line_count <- length(strsplit(Rcode_script, "\n")[[1]])
      line_count <- line_count + 5
      # Asignar aproximadamente 20px por línea para el alto del editor
      editor_height <- paste0(max(300, line_count * 20), "px")
      
      shinyAce::aceEditor(
        outputId = "script_part1",
        value = Rcode_script,
        mode = "r",
        theme = input$"theme", #"chrome",
        height = editor_height,#"200px",
        fontSize = input$"fontSize", #14,
        showLineNumbers = TRUE,
        readOnly = TRUE,
        autoScrollEditorIntoView = TRUE,
        maxLines = 1000,  # Un número grande para evitar scroll
        minLines = line_count 
      )
      
    })
    
    output$card06_script <- renderUI({
      # req(mis_valores())
      
      div(
        style = "height: 100%;",
          uiOutput(ns("el_cartel")),
          card(
            card_header("Editor Options"),
            card_body(
              uiOutput(ns("shiny_ace_editor_MENU")),
              uiOutput(ns("shiny_ace_CODE"))
            )
          )
      )
      
    })
    
    
    
    
    
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
    
   
    
   
    
    observe({
      
      # Mostrar modal de carga
      
      if (!OK_ALL_ACTIVE()) {
        fn_shiny_apply_changes_reactiveValues(rv = active_R_OBJECTS, 
                                              changes_list = default_structure)
      } else {
        if(FALSE){
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
        
      tryCatch({
        # Todo tu código principal aquí
  
        showNotification("Processing...", type = "warning")
        
        req(internal_STR$"check_output")
        mi_super_lista <- reactiveValuesToList(internal_STR) #print(paste0("AVER: ", internal_STR$"pack_output"$"vector_str"$"str_01_MM_variable_selector"))
        
        # ----------------------------------------------------------------------
        #
        # Hardcoded
        my_df <- mi_super_lista$"pack_output"$"df_03_RRC"
        vector_short_names  <- my_df$"short_name"
        vector_full_names   <- my_df$"resource_name"
        str_local_id  <- "the_RRC"
        str_MM_server <- "MM_server"
        str_MM_ui     <- "MM_ui"
        #-----------------------------------------------------------------------
        #
        # server and ui
        ### MM server
        dt_str_MM_server    <- vector_short_names == str_MM_server
        full_name_MM_server <- vector_full_names[dt_str_MM_server]
        my_str_MM_server    <- full_name_MM_server
        
        ### MM ui
        dt_str_MM_ui     <- vector_short_names == str_MM_ui
        full_name_MM_ui  <- vector_full_names[dt_str_MM_ui]
        my_str_MM_ui <- full_name_MM_ui
        #-----------------------------------------------------------------------
        #
        # Running Code!!!!
        # And my objects!!!!!!!!
        #
        ### args...
          args_server <- list(
            id = str_local_id, 
            show_dev = FALSE,
            active_DATASET_SELECTOR, 
            active_TOOLS_SELECTOR,
            active_VARIABLE_SELECTOR,
            active_PLAY_SELECTOR,
            active_R_CODE
          )
        #  
        ### Run Run Run
        the_results <- NULL
        the_results <- do.call(my_str_MM_server, args_server)
        
        ### Saving results on active_R_OBJECTS
        if (!is.null(the_results)) {
            fn_shiny_apply_changes_reactiveValues(rv = active_R_OBJECTS,  changes_list = list(
              "pack_output" = the_results(),
              "check_output" = TRUE,
              "button_state" = "confirmed"
            ))
          }
          
          
        
      }, error = function(e) {
        # THE_MODAL(FALSE)
        
        # Aquí capturamos *todo* error: muestre modal con el mensaje
        showModal(modalDialog(
          title = paste0("Error en el procesamiento de: ", MY_SELECTED_TOOL()),
          paste("Ocurrió un error:", e$message),
          easyClose = TRUE,
          footer = modalButton("Cerrar")
        ))
      })
        
      }
      
    })
    
    
    observe({
      
      if(active_R_OBJECTS$"check_output"){
      # mi_ventana <- ifelse(test = el_check, yes = "output", no = "user_selection")
        mi_ventana <- "output"
      # print(paste("Intentando cambiar a la pestaña:", mi_ventana))
      
      # Cambiar la pestaña usando updateTabsetPanel en lugar de nav_select
      updateTabsetPanel(session, inputId = "mynav", selected = mi_ventana)
      
      # shinyjs::delay(2000, {
      #   removeModal()
      # })
      THE_MODAL(FALSE)
      }
    })
    
     
    
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
    
    
    output$crystal01_run_code <- renderUI({
      # req(active_R_OBJECTS)
      # req(active_R_OBJECTS$"check_output")
      if(active_R_OBJECTS$"check_output"){
      crear_outputs_y_ui33(prefix = "jajja", mis_valores_reactive = reactive(active_R_OBJECTS$"pack_output"), output, ns)    
      } else "NADA"
      
      })
    
    ############################################################################

  
    # # Render 04 - ORRS (output R Results Shiny)
    # Rendedir server and ui modules inside of renderUI!!!!!!
    output$card05_output <- renderUI({
      req(OK_ALL_ACTIVE())
      # req(my_list_str_rv())
      
      req(internal_STR$"check_output")
      mi_super_lista <- reactiveValuesToList(internal_STR) #print(paste0("AVER: ", internal_STR$"pack_output"$"vector_str"$"str_01_MM_variable_selector"))
      
      # ----------------------------------------------------------------------
      #
      # Hardcoded
      my_df <- mi_super_lista$"pack_output"$"df_04_ORRS"
      vector_short_names  <- my_df$"short_name"
      vector_full_names   <- my_df$"resource_name"
      str_local_id  <- "the_04_ORRS"
      str_MM_server <- "MM_server"
      str_MM_ui     <- "MM_ui"
      #-----------------------------------------------------------------------
      #
      # server and ui
      ### MM server
      dt_str_MM_server    <- vector_short_names == str_MM_server
      full_name_MM_server <- vector_full_names[dt_str_MM_server]
      my_str_MM_server    <- full_name_MM_server
      
      ### MM ui
      dt_str_MM_ui     <- vector_short_names == str_MM_ui
      full_name_MM_ui  <- vector_full_names[dt_str_MM_ui]
      my_str_MM_ui <- full_name_MM_ui
      #-----------------------------------------------------------------------
      #
      ### ARGs
      args_server <- list(id = str_local_id, 
                   show_dev = FALSE,
                   mis_valores = reactive(active_R_OBJECTS$"pack_output"),
                   active_TOOLS_SELECTOR = active_TOOLS_SELECTOR
      )
      
      ### Running server module - 04 - ORRS (Output R Results Shiny)
      do.call(my_str_MM_server, args_server)
      
      ### Running ui module - 04 - ORRS (Output R Results Shiny)
      args_ui <- list(id = ns(str_local_id))
      the_rendered_iu <- do.call(my_str_MM_ui, args_ui)
      
      div(
        style = "height: 100%;",  
        the_rendered_iu  
      )
    })
 
    ############################################################################
    
    
    ############################################################################
    
    
    # active_R_CODE   <- do.call(reactiveValues, default_structure)
    
   
    
    # Tab06 - Quarto
    the_quarto_file <- reactive({
      req( MY_SELECTED_TOOL())
      req(OK_ALL_ACTIVE())
      # req(mis_valores())
      # fn_R_pk_folder_path()
      the_folder_quarto <- fn_PK_quarto_folder_path()
      str_sub_folder <- MY_SELECTED_TOOL()
      the_folder_path <- file.path(the_folder_quarto, str_sub_folder)
        
    })
    
    the_pack <- reactive({
      req(OK_ALL_ACTIVE())
      # req(my_list_str_rv())
      
      # print(reactiveValuesToList(active_DATASET_SELECTOR))
      
      shiny_path <- reactiveValuesToList(active_DATASET_SELECTOR)$"pack_output"$"str_import_internal" 
      my_factor <-  reactiveValuesToList(active_VARIABLE_SELECTOR)$"pack_output"$"factor"
      my_vr <-      reactiveValuesToList(active_VARIABLE_SELECTOR)$"pack_output"$"respuesta"
      my_cartel <-  reactiveValuesToList(active_TOOLS_SELECTOR)$"pack_output"$"selected_cartel"

      list_output <- list(shiny_path = shiny_path,
                          my_factor = my_factor,
                          my_vr = my_vr,
                          my_cartel = my_cartel)
      
      # print(list_output)
      return(list_output)
      
      
    })
    # observe(print(the_pack()))
    
    observe({
      req(the_quarto_file())
      # my_list <- reactiveValuesToList(active_R_CODE)
      # print(my_list$"pack_output"$"Rcode_script")
      
    module_quartoRenderer_server(id="quarto_doc", 
                                 documento = the_quarto_file(),
                                 Rcode_script = reactive(active_R_CODE$"pack_output"$"Rcode_script"),
                                 Rcode_quarto = reactive(active_R_CODE$"pack_output"$"Rcode_quarto"),
                                 active_TOOLS_SELECTOR)
    })
    output$card07_download <- renderUI({
      req(OK_ALL_ACTIVE())
      # req(my_list_str_rv())
      module_quartoRenderer_ui(id=ns("quarto_doc"))
    })
    
    }
    
  })
}