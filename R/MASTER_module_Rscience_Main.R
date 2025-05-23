
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
              bslib::nav_panel(title = "user_selection",
                               uiOutput(ns("card02_user_selection"))
              ),
              bslib::nav_panel(title = "crystal02",
                               uiOutput(ns("crystal02"))
              ),
              bslib::nav_panel(title = "dataset",
                               DT::DTOutput(ns("visual_dataset"))
              ),
              bslib::nav_panel(title = "crystal01_run_code",
                               uiOutput(ns("crystal01_run_code"))
                               # verbatimTextOutput(ns("crystal01_run_code"))
              ),
              bslib::nav_panel(title = "crystal04_ALL",
                               uiOutput(ns("crystal04_ALL"))
                               # verbatimTextOutput(ns("crystal01_run_code"))
              ),
              bslib::nav_panel(title = "output",
                               uiOutput(ns("card05_output"))
              ),
              bslib::nav_panel(title = "output22",
                               uiOutput(ns("card05_output22"))
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
    
    
    library(crayon)
    
    message(green("**********************************************"))
    message(green("*                                            *"))
    message(green("*          WELCOME TO THE RSCIENCE WORLD!    *"))
    message(green("*                                            *"))
    message(green("**********************************************"))
    
    
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
    
    # Card 01) Botonera "main menu"
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
    
    
    # Default structure for many objects
    default_structure <- list(
      pack_input = "",
      check_input = FALSE,
      pack_output = "",
      check_output = FALSE,
      button_class = "initial"
    )    
    
    ### ### ### Initialization -------------------------------------------------
    MY_SELECTED_TOOL <- reactiveVal(NULL)
    
   
    # Init Dataset
    internal_DATASET_SELECTOR <- do.call(reactiveValues, default_structure)
    active_DATASET_SELECTOR   <- do.call(reactiveValues, default_structure)
    
    # Init tools
    internal_TOOLS_SELECTOR <- do.call(reactiveValues, default_structure)
    active_TOOLS_SELECTOR   <- do.call(reactiveValues, default_structure)
    
    # Init STR
    internal_STR  <- do.call(reactiveValues, default_structure)
    active_STR    <- do.call(reactiveValues, default_structure)
    
    # Init Variable
    internal_VARIABLE_SELECTOR <- do.call(reactiveValues, default_structure)
    active_VARIABLE_SELECTOR   <- do.call(reactiveValues, default_structure)
    
    # Init Play
    internal_PLAY_SELECTOR    <- do.call(reactiveValues, default_structure)
    active_PLAY_SELECTOR    <- do.call(reactiveValues, default_structure)
    
    ### ### ### END Initialization -------------------------------------------------
    
    
    # Button Dataselect
    Sbutton_01_dataselector2_server(id = "dataset_selector2", internal_DATASET_SELECTOR)
    
    # Button Tools
    Sbutton_02_tools2_server(id = "tools_selector2", default_structure, internal_DATASET_SELECTOR, internal_TOOLS_SELECTOR, internal_PLAY_SELECTOR)
    
    # # # # # # - - - MY_SELECTED_TOOL - - - - - - - - - - - - - - - - - - - - - 
    # Observe - MY_SELECTED_TOOL()
    observe({
      MY_SELECTED_TOOL(NULL)
      valores_internos_list <- reactiveValuesToList(internal_TOOLS_SELECTOR)
      
      info_output <- valores_internos_list$"pack_output"
      
      req(info_output)
      
      MY_SELECTED_TOOL(info_output$"selected_tool")
      
      # Reset internal y active STR
      fn_shiny_apply_changes_reactiveValues(rv = internal_STR, default_structure)
      fn_shiny_apply_changes_reactiveValues(rv = active_STR, default_structure)
      
    })

    # Observe - internal_STR
    observe({
         
      req(MY_SELECTED_TOOL())      

      # Selected tools
      str_selected_tool <- MY_SELECTED_TOOL()

      # Reemplaza 'nombre_del_paquete' por el nombre del paquete que te interesa
      package_name <- "Rscience.GeneralLM"
      vector_pk_fn_vector <- getNamespaceExports(package_name)
    
      # ------------------------------------------------------------------------
      #
      # 1) Settings...
      df_01_settings <- data.frame(
        "short_name" = c("MM_server", "MM_ui", "FN_validate_vars"),
        "str_end_file" = c("_MM_variable_selector_server",
                           "_MM_variable_selector_ui", 
                           "_FN_validate_vars")
      )
      df_01_settings$"resource_name"   <- paste0(str_selected_tool, df_01_settings$"str_end_file")
      df_01_settings$"resource_exists" <- df_01_settings$"resource_name" %in% vector_pk_fn_vector
      check_01_settings <- all(df_01_settings$"resource_exists")
      # ------------------------------------------------------------------------
      #
      # 02) Zocalo selected vars (ZSV)
      df_02_ZSV <- data.frame(
        "short_name" = c("FN_zocalo"),
        "str_end_file" = c("_FN_shiny_zocalo")
      )
      df_02_ZSV$"resource_name"   <- paste0(str_selected_tool, df_02_ZSV$"str_end_file")
      df_02_ZSV$"resource_exists" <- df_02_ZSV$"resource_name" %in% vector_pk_fn_vector
      check_02_ZSV <- all(df_02_ZSV$"resource_exists")
      # ------------------------------------------------------------------------
      #
      # 03) Run R Code (RRC)
      df_03_RRC <- data.frame(
        "short_name" = c("MM_server", "MM_ui"),
        "str_end_file" = c("_MM_run_code_server",
                           "_MM_run_code_ui")
      )
      df_03_RRC$"resource_name"   <- paste0(str_selected_tool, df_03_RRC$"str_end_file")
      df_03_RRC$"resource_exists" <- df_03_RRC$"resource_name" %in% vector_pk_fn_vector
      check_03_RRC <- all(df_03_RRC$"resource_exists")
      # ------------------------------------------------------------------------
      #
      # 04) Output R Results on Shiny (ORRS)
      df_04_ORRS <- data.frame(
        "short_name" = c("MM_server", "MM_ui"),
        "str_end_file" = c("_MM_output_server",
                           "_MM_output_ui")
      )
      df_04_ORRS$"resource_name"   <- paste0(str_selected_tool, df_04_ORRS$"str_end_file")
      df_04_ORRS$"resource_exists" <- df_04_ORRS$"resource_name" %in% vector_pk_fn_vector
      check_04_ORRS <- all(df_04_ORRS$"resource_exists")
      # ------------------------------------------------------------------------
      
      
    
      
    check_all_fn <- all(check_01_settings, check_02_ZSV, 
                        check_03_RRC, check_04_ORRS)
      
    output_list <- list(
      "df_01_settings" = df_01_settings,
      "check_01_settings" = check_01_settings,
      "df_02_ZSV" = df_02_ZSV,
      "check_02_ZSV" = check_02_ZSV,
      "df_03_RRC" = df_03_RRC,
      "check_03_RRC" = check_03_RRC,
      "df_04_ORRS" = df_04_ORRS,
      "check_04_ORRS" = check_04_ORRS,
      
      "check_all_fn" = check_all_fn
      
      )
    
    fn_shiny_apply_changes_reactiveValues(rv = internal_STR, list(
        "pack_input"   = "",
        "check_input"  = TRUE,
        "pack_output"  = output_list,
        "check_output" = check_all_fn,
        "button_class" = "confirmed"))
      
    })
    
    output[["crystal04_A"]] <- renderPrint({ MY_SELECTED_TOOL() })
    output[["crystal04_B1"]] <- renderPrint({ reactiveValuesToList(internal_STR) })
    output[["crystal04_B2"]] <- renderPrint({ reactiveValuesToList(active_STR) })
    
    output$"crystal04_ALL" <- renderUI({
      
      div(
        h2("Selected tool"), 
        verbatimTextOutput(ns("crystal04_A")),
        hr(),
        fluidRow(
          column(6, 
             h2("Internal STR"),
             verbatimTextOutput(ns("crystal04_B1"))
             ),
          column(6, 
             h2("Active STR"),
             verbatimTextOutput(ns("crystal04_B2"))
          )
        )
      )
      
    })
    # # # # # # - - - - - - - - - - - - - - - - - -
    # # # # # # - - - - - - - - - - - - - - - - - -
    # # # # # # - - - - - - - - - - - - - - - - - -
    
    
    Sbutton_03_variable_selector2_server(id = "variable_selector2", 
                                         internal_DATASET_SELECTOR, 
                                         internal_TOOLS_SELECTOR, 
                                         internal_STR, 
                                         internal_VARIABLE_SELECTOR)
    
    Sbutton_reset2_server(id = "reset2", 
                          default_structure, 
                          internal_DATASET_SELECTOR,  active_DATASET_SELECTOR,
                          internal_TOOLS_SELECTOR,    active_TOOLS_SELECTOR,
                          internal_STR,               active_STR,
                          internal_VARIABLE_SELECTOR, active_VARIABLE_SELECTOR,
                          internal_PLAY_SELECTOR,     active_PLAY_SELECTOR)
    
    
    Sbutton_play2_server(id = "play2", 
                         default_structure, 
                         internal_DATASET_SELECTOR,  active_DATASET_SELECTOR,
                         internal_TOOLS_SELECTOR,    active_TOOLS_SELECTOR,
                         internal_STR,               active_STR,
                         internal_VARIABLE_SELECTOR, active_VARIABLE_SELECTOR,
                         internal_PLAY_SELECTOR,     active_PLAY_SELECTOR)
    
    
  
    
    
    
    ############################################################################
    output[["internal_DATASET_SELECTOR"]] <- renderPrint({ reactiveValuesToList(internal_DATASET_SELECTOR) })
    output[["active_DATASET_SELECTOR"]] <- renderPrint({ reactiveValuesToList(active_DATASET_SELECTOR) })
    
    output[["internal_TOOLS_SELECTOR"]] <- renderPrint({ reactiveValuesToList(internal_TOOLS_SELECTOR) })
    output[["active_TOOLS_SELECTOR"]] <- renderPrint({ reactiveValuesToList(active_TOOLS_SELECTOR) })
    
    output[["internal_VARIABLE_SELECTOR"]] <- renderPrint({ reactiveValuesToList(internal_VARIABLE_SELECTOR) })
    output[["active_VARIABLE_SELECTOR"]] <- renderPrint({ reactiveValuesToList(active_VARIABLE_SELECTOR) })
    
    output[["internal_PLAY_SELECTOR"]] <- renderPrint({ reactiveValuesToList(internal_PLAY_SELECTOR) })
    output[["active_PLAY_SELECTOR"]] <- renderPrint({ reactiveValuesToList(active_PLAY_SELECTOR) })
    
    
    output$"crystal02" <- renderUI({
    
      div(
      fluidRow(
        column(3, 
               h2("Internal Dataset"), 
                  verbatimTextOutput(ns("internal_DATASET_SELECTOR"))),
        column(3, 
               h2("Internal Tools"), 
               verbatimTextOutput(ns("internal_TOOLS_SELECTOR"))),
        column(3, 
               h2("Internal Variable"), 
                  verbatimTextOutput(ns("internal_VARIABLE_SELECTOR"))),
        column(3, 
               h2("Internal Play"),
                  verbatimTextOutput(ns("internal_PLAY_SELECTOR")))
        
      ),
      hr(),
      fluidRow(
        column(3, 
               h2("External Dataset"), 
               verbatimTextOutput(ns("active_DATASET_SELECTOR"))),
        column(3, 
               h2("External Tools"), 
               verbatimTextOutput(ns("active_TOOLS_SELECTOR"))),
        column(3, 
               h2("External Variable"), 
               verbatimTextOutput(ns("active_VARIABLE_SELECTOR"))),
        column(3, 
               h2("External Play"), 
               verbatimTextOutput(ns("active_PLAY_SELECTOR")))
        
      )
      )
      
    })
    
    
    ############################################################################
    output$visual_dataset <- DT::renderDataTable({
      req(internal_DATASET_SELECTOR)
      req(internal_DATASET_SELECTOR$"check_output")
      req(internal_DATASET_SELECTOR$"pack_output"$"database")
      
      my_dataset <- internal_DATASET_SELECTOR$"pack_output"$"database"
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
      req(internal_STR$"check_output")
      mi_super_lista <- reactiveValuesToList(internal_STR) #print(paste0("AVER: ", internal_STR$"pack_output"$"vector_str"$"str_01_MM_variable_selector"))
      
      # ------------------------------------------------------------------------
      #
      # Hardcoded
      my_df <- mi_super_lista$"pack_output"$"df_02_ZSV"
      vector_short_names  <- my_df$"short_name"
      vector_full_names   <- my_df$"resource_name"
      str_FN        <- "FN_zocalo"
      # ------------------------------------------------------------------------
      #
      # Funtion name detection
      dt_str_FN    <- vector_short_names == str_FN
      full_name_FN <- vector_full_names[dt_str_FN]
      # ------------------------------------------------------------------------
      #
      # Running fn for obtain zocalo
      args <- list(internal_VARIABLE_SELECTOR = internal_VARIABLE_SELECTOR)
      the_zocalo <- do.call(full_name_FN, args)
      the_zocalo
      # ------------------------------------------------------------------------
      
      
      
    })
    
    
    
    
    ############################################################################
    
    
    # # # # CONTROL POINT # # # # 
    OK_ALL_ACTIVE <- reactive({
      req(active_DATASET_SELECTOR, active_TOOLS_SELECTOR, active_STR, 
          active_VARIABLE_SELECTOR, active_PLAY_SELECTOR)
      
      vector_check <- c(active_DATASET_SELECTOR$"check_output", 
                        active_TOOLS_SELECTOR$"check_output",
                        active_STR$"check_output",
                        active_VARIABLE_SELECTOR$"check_output",
                        active_PLAY_SELECTOR$"check_output")
      
      the_final_value <- all(vector_check)
      the_final_value
    })
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
    
  
    # Crystal01 - the_R_objects
    # AQUI ESTAN LAS SALIDAS ESTADISTICAS!!!!
    default_R_OBJECTS <- list(
      check_previous_items = FALSE,
      check_init_modal     = FALSE,
      check_init_proc      = FALSE,
      check_end_proc       = FALSE,
      output               = "",
      check_output         = FALSE,
      check_end_modal      = FALSE,
      check_general        = FALSE,
      button_class         = "initial"
    )
    
    active_R_OBJECTS   <- do.call(reactiveValues, default_R_OBJECTS)
    

    
    
    observe({
      
      # Mostrar modal de carga
      
      if (!OK_ALL_ACTIVE()) {
        fn_shiny_apply_changes_reactiveValues(rv = active_R_OBJECTS, changes_list = default_structure)
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
          args <- list(
            id = str_local_id, 
            show_dev = FALSE,
            active_DATASET_SELECTOR, 
            active_TOOLS_SELECTOR,
            active_VARIABLE_SELECTOR,
            active_PLAY_SELECTOR
          )
        #  
        ### Run Run Run
        the_results <- NULL
        the_results <- do.call(my_str_MM_server, args)
        
        ### Saving results on active_R_OBJECTS
        if (!is.null(the_results)) {
            fn_shiny_apply_changes_reactiveValues(rv = active_R_OBJECTS,  changes_list = list(
              "pack_output" = the_results(),
              "check_output" = TRUE,
              "button_class" = "confirmed"
            ))
          }
          
          
        
      }, error = function(e) {
        # THE_MODAL(FALSE)
        
        # Aquí capturamos *todo* error: muestre modal con el mensaje
        showModal(modalDialog(
          title = paste0("Error en el procesamiento de: ", str_selected_modulo),
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

  
    if(FALSE){
    observe({
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
      args <- list(id = str_local_id, 
                   show_dev = FALSE,
                   mis_valores = reactive(active_R_OBJECTS$"pack_output"),
                   active_TOOLS_SELECTOR = active_TOOLS_SELECTOR
      )
      
      ### Running server module - 04 - ORRS (Output R Results Shiny)
      do.call(my_str_MM_server, args)
      
      args <- list(id = ns(str_local_id))
      resultado <- do.call(my_str_MM_ui, args)
      resultado
    })
    }
    
    # # Renderizar la UI del selector de variables
    output$card05_output<- renderUI({
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
      args <- list(id = str_local_id, 
                   show_dev = FALSE,
                   mis_valores = reactive(active_R_OBJECTS$"pack_output"),
                   active_TOOLS_SELECTOR = active_TOOLS_SELECTOR
      )
      
      ### Running server module - 04 - ORRS (Output R Results Shiny)
      do.call(my_str_MM_server, args)
      
      args <- list(id = ns(str_local_id))
      resultado <- do.call(my_str_MM_ui, args)
      resultado
    })
    if(FALSE){
    output$card05_output<- renderUI({
      req(OK_ALL_ACTIVE())
      req(my_list_str_rv())
      
      str_selected_modulo <- my_list_str_rv()$"str_05_MM_output"
      new_modulo_ui <- paste0(str_selected_modulo, "_ui")
      new_id <- "the_output"
      
      # print(new_modulo_ui)
      args <- list(id = ns(new_id))
      
      div(
        style = "height: 100%;",  # Altura del contenedor (100% del contenedor padre)
        do.call(new_modulo_ui, args)  # Altura del contenido (100% del contenedor padre)
      )
    })
    }
    ############################################################################
    
    if(FALSE){
    observe({
      req(OK_ALL_ACTIVE())
      req(my_list_str_rv())
      str_selected_modulo <- my_list_str_rv()$"str_05_MM_output22"
      new_server <- paste0(str_selected_modulo, "_server")
      new_ui <- paste0(str_selected_modulo, "_ui")
      new_id <- "the_output22"
      
      args <- list(id = new_id, show_dev = FALSE,
                   mis_valores = reactive(active_R_OBJECTS$"pack_output"))
      
      vector_funciones <- ls("package:Rscience.GeneralLM")
      check_in <- new_server %in% vector_funciones
      # print(check_in)
      
      # print(str_server())
      # Verificar si la función existe y ejecutarla
      if (check_in) {
        do.call(new_server, args)
        # print(resultado)  # Output: 5
      } else {
        print("El modulo no existe.")
      }
      
      
    })
    
    
    # # Renderizar la UI del selector de variables
    output$card05_output22<- renderUI({
      req(OK_ALL_ACTIVE())
      req(my_list_str_rv())
      
      str_selected_modulo <- my_list_str_rv()$"str_05_MM_output22"
      new_modulo_ui <- paste0(str_selected_modulo, "_ui")
      new_id <- "the_output22"
      
      # print(new_modulo_ui)
      args <- list(id = ns(new_id))
      
      div(
        style = "height: 100%;",  # Altura del contenedor (100% del contenedor padre)
        do.call(new_modulo_ui, args)  # Altura del contenido (100% del contenedor padre)
      )
    })
  
    
    ############################################################################
  
    active_R_CODE   <- do.call(reactiveValues, default_structure)
      
    
    
    observe({
      req(OK_ALL_ACTIVE())
      req(my_list_str_rv())
      str_selected_modulo <- my_list_str_rv()$"str_06_MM_script"
      new_server <- paste0(str_selected_modulo, "_server")
      new_ui <- paste0(str_selected_modulo, "_ui")
      new_id <- "the_script"
      
      # print(new_server)
      # str_server(new_server)
      # str_ui(new_ui)
      # my_id(new_id)
      args <- list(id = new_id, show_dev = FALSE,
                   active_DATASET_SELECTOR, 
                   active_TOOLS_SELECTOR,
                   active_VARIABLE_SELECTOR,
                   active_PLAY_SELECTOR,
                   active_R_CODE)
      
      vector_funciones <- ls("package:Rscience.GeneralLM")
      check_in <- new_server %in% vector_funciones
      # print(check_in)
      
      # print(str_server())
      # Verificar si la función existe y ejecutarla
      if (check_in) {
        do.call(new_server, args)
        # print(resultado)  # Output: 5
      } else {
        print("El modulo no existe.")
      }
      
      
    })
    
    
    # # Renderizar la UI del selector de variables
    output$card06_script <- renderUI({
      req(OK_ALL_ACTIVE())
      req(my_list_str_rv())
      
      str_selected_modulo <- my_list_str_rv()$"str_06_MM_script"
      new_modulo_ui <- paste0(str_selected_modulo, "_ui")
      new_id <- "the_script"
      
      # print(new_modulo_ui)
      args <- list(id = ns(new_id))
      
      div(
        style = "height: 100%;",  # Altura del contenedor (100% del contenedor padre)
        do.call(new_modulo_ui, args)  # Altura del contenido (100% del contenedor padre)
      )
    })
    
    
    
    ############################################################################
    
    active_R_CODE   <- do.call(reactiveValues, default_structure)
    
    
    
    observe({
      req(OK_ALL_ACTIVE())
      req(my_list_str_rv())
      str_selected_modulo <- my_list_str_rv()$"str_06_MM_script"
      new_server <- paste0(str_selected_modulo, "_server")
      new_ui <- paste0(str_selected_modulo, "_ui")
      new_id <- "the_script"
      
      # print(new_server)
      # str_server(new_server)
      # str_ui(new_ui)
      # my_id(new_id)
      args <- list(id = new_id, show_dev = FALSE,
                   active_DATASET_SELECTOR, 
                   active_TOOLS_SELECTOR,
                   active_VARIABLE_SELECTOR,
                   active_PLAY_SELECTOR,
                   active_R_CODE)
      
      vector_funciones <- ls("package:Rscience.GeneralLM")
      check_in <- new_server %in% vector_funciones
      # print(check_in)
      
      # print(str_server())
      # Verificar si la función existe y ejecutarla
      if (check_in) {
        do.call(new_server, args)
        # print(resultado)  # Output: 5
      } else {
        print("El modulo no existe.")
      }
      
      
    })
    
    
    # # Renderizar la UI del selector de variables
    output$card06_script <- renderUI({
      req(OK_ALL_ACTIVE())
      req(my_list_str_rv())
      
      str_selected_modulo <- my_list_str_rv()$"str_06_MM_script"
      new_modulo_ui <- paste0(str_selected_modulo, "_ui")
      new_id <- "the_script"
      
      # print(new_modulo_ui)
      args <- list(id = ns(new_id))
      
      div(
        style = "height: 100%;",  # Altura del contenedor (100% del contenedor padre)
        do.call(new_modulo_ui, args)  # Altura del contenido (100% del contenedor padre)
      )
    })
    
    ############################################################################
    
    # Tab06 - Quarto
    the_quarto_file <- reactive({
      # req(mis_valores())
      GeneralLM_fix_anova1_quarto_file_path()
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
    
    module_quartoRenderer_server(id="quarto_doc", 
                                 documento = the_quarto_file(),
                                 Rcode_script = reactive(active_R_CODE$"pack_output"$"Rcode_script"),
                                 Rcode_quarto = reactive(active_R_CODE$"pack_output"$"Rcode_quarto"),
                                 active_TOOLS_SELECTOR)
    
    output$card07_download <- renderUI({
      req(OK_ALL_ACTIVE())
      # req(my_list_str_rv())
      module_quartoRenderer_ui(id=ns("quarto_doc"))
    })
    
    }
    
  })
}