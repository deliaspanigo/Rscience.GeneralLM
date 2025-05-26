
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
              bslib::nav_panel(title = "super",
                               uiOutput(ns("SUPER_ALL"))
              ),
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
              bslib::nav_panel(title = "crystal05_ALL",
                               uiOutput(ns("crystal05_ALL"))
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
              Sbutton_play2_ui(id = ns("play2"))
            )
          )
        )
      )
    })
    ############################################################################
    
    # List steps
    # 1) Initial OK!
    # 2) Import tool options OK!
    # 2) Import dataset    
    # 3) Tool selection
    # 4) Choosing settings...
    # 5) Make script for user and quarto
    # 6) Press PLAY and pass internal to active
    # 7) Create temporal folder
    # 8) Execute fn to obtain R objets
    # 9) Download
    # -----------------------------------------
    # Reset
    

    number_current_step  <- reactiveVal(1)
    APP_TOTEM <- reactiveValues()
    
    STR_STEP_NAME <- "step"
    
    default_list_step <- list(
      "current_step" = NA,
      "current_label" = NA,
      "key_obj" = NA, 
      "check_previous" = FALSE,
      "pack_output" = "",
      "check_output" = FALSE,
      "button_class" = "initial",
      "the_time"           = "",
      "error_message" = ""
    )
    
    
    
    # List steps
    # Step 01) Initial
    step01_initial <- reactive({
      
      
      # Requeriments -----------------------------------------------------------
      req(number_current_step() == 1)
      print(number_current_step())
      
      # Hardcoded --------------------------------------------------------------
      current_label <- "Step 01 - Initial"
      current_step <- number_current_step()
      
      # Basic for step and plague control --------------------------------------
      current_step_name <- paste0(STR_STEP_NAME, current_step)
      fn_shiny_remove_future_steps(APP_TOTEM, current_step, STR_STEP_NAME)
      
      # The new list step ------------------------------------------------------
      new_list_step <- default_list_step
      new_list_step$"current_step"   <- current_step
      new_list_step$"current_label"  <- current_label
      new_list_step$"key_obj"        <- "initial"#sys.function()
      new_list_step$"check_previous" <- TRUE
      new_list_step$"pack_output"    <- ""
      new_list_step$"check_output"   <- TRUE
      new_list_step$"button_class"   <- "confirmed"
      new_list_step$"the_time"           <- fn_R_the_time_beauty()
      new_list_step$"error_message"  <- ""
      
      # New list totem 
      new_list_totem <- new_list_step
      new_list_totem$"pack_output" <- "Good luck!"

    
      # Validating the new totem list ------------------------------------------
      check_list_step <- fn_R_validate_new_list(new_list = new_list_step,  
                                                 ref_list = default_list_step)
      
      # Error message for new list step ----------------------------------------
      if (!check_list_step) {
        fn_shiny_show_error_new_list(step_number = current_step, 
                                     new_list = new_list_step, 
                                     ref_list = default_list_step)
        
        return()  # Stop further execution
      }
      
      # Add --------------------------------------------------------------------
      isolate({
        APP_TOTEM[[current_step_name]] <- new_list_step
        number_current_step(current_step+1)
      })
      
      # Return
      return(new_list_step)
      
    })
    
    
    
    # 1) Import tool options OK!
    step02_upload <- reactive({
      
      # Requeriments -----------------------------------------------------------
      req(number_current_step() == 2)
      print(number_current_step())
      
      # Hardcoded --------------------------------------------------------------
      current_label <- "Step 02: Up resources"
      current_step <- number_current_step()
      
      # Basics and plague control ----------------------------------------------
      current_step_name <- paste0(STR_STEP_NAME, current_step)
      fn_shiny_remove_future_steps(APP_TOTEM, current_step, STR_STEP_NAME)
      
      # Check previous totem ---------------------------------------------------
      my_previous_step <- current_step - 1
      my_previous_step_name <- paste0(STR_STEP_NAME, my_previous_step)
      check_previous <- APP_TOTEM[[my_previous_step_name]]$"check_output"

      # Error message for check previous totem
      if(!check_previous){
        error_message <- paste0("Step: ", current_step)
        return(NULL)
      }
      
      # Action for this step - Create pack_output!!!!!!!!!! --------------------
      list_all_config02_tools <- fn_R_load_config02_yaml()
      pack_output <- list_all_config02_tools
      
      # Check output and more --------------------------------------------------
      check_output <- !is.null(pack_output)
      button_class <- "confirmed"
      
      
      # Message error for check output -----------------------------------------
      if(!check_output){
        error_message <- paste0("Step: ", current_step)
        return(NULL)
      }
      
      
      # The new list step ------------------------------------------------------
      new_list_step <- default_list_step
      new_list_step$"current_step"   <- current_step
      new_list_step$"current_label"  <- current_label
      new_list_step$"key_obj"        <- "upload"#sys.function()
      new_list_step$"check_previous" <- check_previous
      new_list_step$"pack_output"    <- pack_output
      new_list_step$"check_output"   <- check_output
      new_list_step$"button_class"   <- button_class
      new_list_step$"the_time"       <- fn_R_the_time_beauty()
      new_list_step$"error_message"  <- ""
      
      # New list totem 
      new_list_totem <- new_list_step
      new_list_totem$"pack_output" <- "Good luck!"
      
      
      # Validating the new totem list ------------------------------------------
      check_list_step <- fn_R_validate_new_list(new_list = new_list_step,  
                                                ref_list = default_list_step)
      
      # Error message for new list step ----------------------------------------
      if (!check_list_step) {
        fn_shiny_show_error_new_list(step_number = current_step, 
                                     new_list = new_list_step, 
                                     ref_list = default_list_step)
        
        return()  # Stop further execution
      }
      
      # Add --------------------------------------------------------------------
      isolate({
        APP_TOTEM[[current_step_name]] <- new_list_step
        number_current_step(current_step+1)
      })
      
      # Return
      return(new_list_step)
    })
    
    # Hacer todo con modulos...
    # my_modulo_server(id = "LALA", step_module = 2, current_step, APP_TOTEM)
    # No hay mas objetos intermedios o separados.
    # el detalle "key_obj" es para poder buscar un step en particular aunque
    # se agreguen mas modulos intermedios.
    # por ejemplo el modulo de RUN necesita al modulo de Script, y al modulo de
    # settings, y al modulo de data set, indistintamente de si hay otros modulos
    # o si se agregan o cambiar los modulos. Solo debe ocurrir que el modulo de script
    # este despues de los modulos que esta requiriendo.
    
    # y en los "crystal" podemos trabajar con o sin los product_output, para poder tener
    # una vison de lo que está siendo ejecutado sin tener que ver tooodos los resultados
    # obtendios en cada caso.
    
    all_step_OK <- reactiveVal()
    observe({
      print(step01_initial())
      print(step02_upload())
    })
    # Card 01) Botonera "main menu"
    output$"SUPER_A" <- renderPrint({
      
      # "current_step" = NA,
      # "current_label" = NA,
      # "check_previous" = FALSE,
      # "check_output" = FALSE,
      # "pack_output" = "",
      # "button_class" = "initial",
      # "error_message" = ""
      # 
      number_current_step()
    })
    output$"SUPER_B" <- renderPrint({
      the_list <- reactiveValuesToList(APP_TOTEM)
      the_list
    })
    output$"SUPER_ALL" <- renderUI({
      div(
        h2("Por iniciar"),
        verbatimTextOutput(ns("SUPER_A")),
        br(),
        h2("Finalizados"),
        verbatimTextOutput(ns("SUPER_B"))
      )
    })
    ############################################################################
    
    

    if(FALSE){
    
    
    # Init STR
    internal_STR <- do.call(reactiveValues, default_structure_internal)
    active_STR   <- do.call(reactiveValues, default_structure_internal)
    
    # Init Variable
       
    # Init Play
    internal_PLAY_SELECTOR <- do.call(reactiveValues, default_structure_internal)
    active_PLAY_SELECTOR   <- do.call(reactiveValues, default_structure_internal)
    
    Sbutton_reset2_server(id = "reset2", 
                          default_structure_internal, 
                          number_current_step,
                          internal_DATASET_SELECTOR,  active_DATASET_SELECTOR,
                          internal_TOOLS_SELECTOR,    active_TOOLS_SELECTOR,
                          internal_STR,               active_STR,
                          internal_VARIABLE_SELECTOR, active_VARIABLE_SELECTOR,
                          internal_PLAY_SELECTOR,     active_PLAY_SELECTOR)
    
    # Inicialización de casa paso
    
    
   
    
    #---------------------------------------------------------------------------
    # Step 01 - Activation
    # 
    #
    #---------------------------------------------------------------------------    
    # Step02 - Load conf02 yaml
    ### All config tools
    
    
    ### Step02 - Observe
    observe({
      # Requeriments -----------------------------------------------------------
      
      
    })
    #
    #---------------------------------------------------------------------------    
    # Step03 - Data Selector
    #
    ### Internal ReactiveValues
    internal_DATASET_SELECTOR <- do.call(reactiveValues, default_structure_internal)
    active_DATASET_SELECTOR   <- do.call(reactiveValues, default_structure_internal)
    
    ### Button Dataselect - Server
    Sbutton_01_dataselector_server(id = "dataset_selector2", internal_DATASET_SELECTOR)
    
    ### Step03 - Observe
    observe({
      # Requerments ------------------------------------------------------------
      req(number_current_step() == 3)
      req(internal_DATASET_SELECTOR, active_DATASET_SELECTOR)
      req(internal_DATASET_SELECTOR$"check_output")
      
      # Bacis and plague control -----------------------------------------------
      current_step <- number_current_step()
      fn_shiny_remove_future_steps(APP_TOTEM, current_step = current_step, STR_STEP_NAME)
      
      # New list ---------------------------------------------------------------
      new_list_step <- default_list_step
      error_message <- ""
      new_list_step$"current_step"   <- current_step
      new_list_step$"current_label" <- "Step 03: Import Dataset"
      
      
      # Check previous ---------------------------------------------------------
      my_previous_step <- current_step - 1
      my_previous_step_name <- paste0(STR_STEP_NAME, my_previous_step)
      check_previous <- APP_TOTEM[[my_previous_step_name]]$"check_output"
      new_list_step$"check_previous" <- check_previous
      if(!check_previous){
        error_message <- paste0("Step: ", current_step)
        
      }
      
      # Check output -----------------------------------------------------------
      check_output <- !is.null(internal_DATASET_SELECTOR)
      
      # Message error for check output -----------------------------------------
      if(!check_output){
        error_message <- paste0("Step: ", current_step)
        
      }
      
      # Filling new list -------------------------------------------------------
      new_list_step$"check_output"   <- check_output
      new_list_step$"pack_output"    <- ""
      new_list_step$"button_class"   <- "confirmed"
      new_list_step$"error_message"  <- error_message
      
      # Validate the list ------------------------------------------------------
      check_list <- fn_R_validate_new_list(new_list = new_list_step, 
                                           ref_list = default_list_step)
      
      # Message for error on new list-- ----------------------------------------
      if (!check_list) {
        fn_shiny_show_error_new_list(step_number = current_step, 
                                     new_list = new_list_step, 
                                     ref_list = default_list_step)
        
        return()  # Stop further execution
      }
      
      # Add --------------------------------------------------------------------
      isolate({
        current_step_name <- paste0(STR_STEP_NAME, current_step)
        APP_TOTEM[[current_step_name]] <- new_list_step
        number_current_step(current_step+1)
        fn_shiny_remove_future_steps(APP_TOTEM, current_step = current_step, STR_STEP_NAME)
        
      })
      
    })
    #
    #---------------------------------------------------------------------------    
    # Step04 - Tool Selector
    #
    ### Internal ReactiveValues
    internal_TOOLS_SELECTOR <- do.call(reactiveValues, default_structure_internal)
    active_TOOLS_SELECTOR   <- do.call(reactiveValues, default_structure_internal)
    
    ### Button Tool Selector - Server
    Sbutton_02_tools_server(id = "tools_selector2", default_structure_internal, internal_DATASET_SELECTOR, internal_TOOLS_SELECTOR)
    
    # Observe - MY_SELECTED_TOOL()
    MY_SELECTED_TOOL <- reactiveVal(NULL)
    observe({
      req(internal_TOOLS_SELECTOR, active_TOOLS_SELECTOR)
      req(internal_TOOLS_SELECTOR$"check_output")
      
      MY_SELECTED_TOOL(NULL)
      valores_internos_list <- reactiveValuesToList(internal_TOOLS_SELECTOR)
      
      info_output <- valores_internos_list$"pack_output"
      
      req(info_output)
      
      MY_SELECTED_TOOL(info_output$"selected_tool")
    })

    ### Step04 - Observe
    observe({
      
      # Requeriments -----------------------------------------------------------
      req(number_current_step() == 4)
      req(internal_TOOLS_SELECTOR, active_TOOLS_SELECTOR)
      req(internal_TOOLS_SELECTOR$"check_output")
      req(MY_SELECTED_TOOL())
      
      # Basics and plague control ----------------------------------------------
      current_step <- number_current_step()
      current_step_name <- paste0(STR_STEP_NAME, current_step)
      fn_shiny_remove_future_steps(APP_TOTEM, current_step = current_step, STR_STEP_NAME)
      
      # New list ---------------------------------------------------------------
      new_list_step <- default_list_step
      error_message <- ""
      new_list_step$"current_step"   <- current_step
      new_list_step$"current_label" <- "Step 04: Tools selection"
      
      
      # Check previous ---------------------------------------------------------
      my_previous_step <- current_step - 1
      my_previous_step_name <- paste0(STR_STEP_NAME, my_previous_step)
      check_previous <- APP_TOTEM[[my_previous_step_name]]$"check_output"
      new_list_step$"check_previous" <- check_previous
      
      # Error message check previous -------------------------------------------
      if(!check_previous){
        error_message <- paste0("Step: ", current_step)
        return()
      }
      
      # Check output -----------------------------------------------------------
      check_output <- !is.null(internal_TOOLS_SELECTOR)
      if(!check_output){
        error_message <- paste0("Step: ", current_step)
        return()
      }
      
      # Filling new list -------------------------------------------------------
      new_list_step$"check_output"   <- check_output
      new_list_step$"pack_output"    <- MY_SELECTED_TOOL()
      new_list_step$"button_class"   <- "confirmed"
      new_list_step$"error_message"  <- error_message
      
      # Validating the new list ------------------------------------------------
      check_list <- fn_R_validate_new_list(new_list = new_list_step, 
                                           ref_list = default_list_step)
      
      # Error message for new list ---------------------------------------------
      if (!check_list) {
        fn_shiny_show_error_new_list(step_number = current_step, 
                                     new_list = new_list_step, 
                                     ref_list = default_list_step)
        
        return()  # Stop further execution
      }
      
      # Add --------------------------------------------------------------------
      isolate({
        APP_TOTEM[[current_step_name]] <- new_list_step
        number_current_step(current_step+1)
        fn_shiny_remove_future_steps(APP_TOTEM, current_step = current_step, STR_STEP_NAME)
        
      })
      
    })
    #---------------------------------------------------------------------------    
    # Step05 - CFG (config from tool selector)
    #
    ### ReactiveValues
    internal_CFG <- do.call(reactiveValues, default_structure_internal)
    active_CFG   <- do.call(reactiveValues, default_structure_internal)
    
    ### Step05 - Observe
    observe({
      
      # Requeriments -----------------------------------------------------------
      req(number_current_step() == 5)
      req(internal_CFG, active_CFG)
      req(MY_SELECTED_TOOL(), list_all_config02_tools)
      
      # Basics and plague control ----------------------------------------------
      current_step <- number_current_step()
      current_step_name <- paste0(STR_STEP_NAME, current_step)
      fn_shiny_remove_future_steps(APP_TOTEM, current_step = current_step, STR_STEP_NAME)
      
      # New list ---------------------------------------------------------------      
      new_list_step <- default_list_step
      error_message <- ""
      new_list_step$"current_step"   <- current_step
      new_list_step$"current_label" <- "Step 05: CFG from selected tool"
      
      
      # Check previous ---------------------------------------------------------
      my_previous_step <- current_step - 1
      my_previous_step_name <- paste0(STR_STEP_NAME, my_previous_step)
      check_previous <- APP_TOTEM[[my_previous_step_name]]$"check_output"
      new_list_step$"check_previous" <- check_previous
      
      # Error message for check previous ---------------------------------------
      if(!check_previous){
        error_message <- paste0("Step: ", current_step)
        
      }

      # Check output -----------------------------------------------------------
      str_selected_tool <- MY_SELECTED_TOOL()
      vector_names_all_tools <- names(list_all_config02_tools)
      check_output <- str_selected_tool %in% vector_names_all_tools
      pack_output <- list_all_config02_tools[[str_selected_tool]]
      button_class <- "confirmed"
      
      # Error message for check_output -----------------------------------------
      if (!check_output) {
        text_error <- "Rscience error: selected tools is not available.\n
        Selected tool: _the_tool_"
        text_error <- sub(pattern = "_the_tool_", replacement = str_selected_tool, text_error)
        shiny::showNotification(
          text_error,
          type = "warning"
        )
        return()  # Salir del observe
      }
      
      # Filling internal_CFG ---------------------------------------------------
      fn_shiny_apply_changes_reactiveValues(rv = internal_CFG, list(
        "pack_input"   = "",
        "check_input"  = TRUE,
        "pack_output"  = pack_output,
        "check_output" = check_output,
        "button_class" = button_class))
      
      # Filling new list -------------------------------------------------------
      new_list_step$"check_output"   <- check_output
      new_list_step$"pack_output"    <- ""
      new_list_step$"button_class"   <- button_class
      new_list_step$"error_message"  <- error_message
      
      # Check new list ---------------------------------------------------------
      check_list <- fn_R_validate_new_list(new_list = new_list_step, 
                                           ref_list = default_list_step)
      
      # Error message for check new list ---------------------------------------
      if (!check_list) {
        fn_shiny_show_error_new_list(step_number = current_step, 
                                     new_list = new_list_step, 
                                     ref_list = default_list_step)
        
        return()  # Stop further execution
      }
      
      # Add --------------------------------------------------------------------
      isolate({
        APP_TOTEM[[current_step_name]] <- new_list_step
        number_current_step(current_step+1)
        fn_shiny_remove_future_steps(APP_TOTEM, current_step = current_step, STR_STEP_NAME)
        
      })
      
    })
    #
    #---------------------------------------------------------------------------    
    # 
    # Step06 - Settings Selector
    #
    ### ReactiveValues
    internal_VARIABLE_SELECTOR <- do.call(reactiveValues, default_structure_internal)
    active_VARIABLE_SELECTOR   <- do.call(reactiveValues, default_structure_internal)
    
    ### Module server
    Sbutton_03_settings_server(id = "settings", 
                               internal_DATASET_SELECTOR, 
                               internal_TOOLS_SELECTOR,
                               internal_CFG,
                               internal_VARIABLE_SELECTOR)
    
    ### Step06 - Observe
    observe({
      
      # Requeriments -----------------------------------------------------------
      req(number_current_step() == 6)
      req(internal_VARIABLE_SELECTOR, active_VARIABLE_SELECTOR)
      req(internal_VARIABLE_SELECTOR$"check_output")

      # Bacics and plague control ----------------------------------------------
      current_step <- number_current_step()
      current_step_name <- paste0(STR_STEP_NAME, current_step)
      fn_shiny_remove_future_steps(APP_TOTEM, current_step = current_step, STR_STEP_NAME)
      
      # Info for selected step -------------------------------------------------
      new_list_step <- default_list_step
      error_message <- ""
      new_list_step$"current_step"   <- current_step
      new_list_step$"current_label" <- "Step 06: Setting Selector"
      
      
      # Check Previous Step ----------------------------------------------------
      my_previous_step <- current_step - 1
      my_previous_step_name <- paste0(STR_STEP_NAME, my_previous_step)
      check_previous <- APP_TOTEM[[my_previous_step_name]]$"check_output"
      new_list_step$"check_previous" <- check_previous

      # Error message for check previous ---------------------------------------
      if(!check_previous){
        error_message <- paste0("Step: ", current_step)
        
      }

      # Check_out --------------------------------------------------------------
      check_output <- internal_VARIABLE_SELECTOR$"check_out"
      if(!check_output){
        error_message <- paste0("Step: ", current_step)
        
      }

      # Filling new_list -------------------------------------------------------
      new_list_step$"check_output"   <- check_output
      new_list_step$"pack_output"    <- ""
      new_list_step$"button_class"   <- "confirmed"
      new_list_step$"error_message"  <- error_message
      
      # Validating new list ----------------------------------------------------
      check_list <- fn_R_validate_new_list(new_list = new_list_step, 
                                           ref_list = default_list_step)
      
      # Error message for new list ---------------------------------------------
      if (!check_list) {
        fn_shiny_show_error_new_list(step_number = current_step, 
                                     new_list = new_list_step, 
                                     ref_list = default_list_step)
        
        return()  # Stop further execution
      }
      
      # Add --------------------------------------------------------------------
      isolate({
        APP_TOTEM[[current_step_name]] <- new_list_step
        number_current_step(current_step+1)
        fn_shiny_remove_future_steps(APP_TOTEM, current_step = current_step, STR_STEP_NAME)
        
      })
      
    })
    
    #
    #---------------------------------------------------------------------------
    #
    # Step07 - R Code
    
    
    ### ReactiveValues
    internal_R_CODE <- do.call(reactiveValues, default_structure_internal)
    active_R_CODE   <- do.call(reactiveValues, default_structure_internal)
    
    ### Observe
    observe({
      
      # Requeriments -----------------------------------------------------------
      req(number_current_step() == 7)
      req(internal_R_CODE, active_R_CODE)
      
      # Basics and plague control ----------------------------------------------
      current_step <- number_current_step()
      fn_shiny_remove_future_steps(APP_TOTEM, current_step = current_step, STR_STEP_NAME)
      
      # New list ---------------------------------------------------------------
      new_list_step <- default_list_step
      error_message <- ""
      new_list_step$"current_step"   <- current_step
      new_list_step$"current_label" <- "Step 07: R Code"
      
      
      # Check previous ---------------------------------------------------------
      my_previous_step <- current_step - 1
      my_previous_step_name <- paste0(STR_STEP_NAME, my_previous_step)
      check_previous <- APP_TOTEM[[my_previous_step_name]]$"check_output"
      new_list_step$"check_previous" <- check_previous
      if(!check_previous){
        error_message <- paste0("Step: ", current_step)
        
      }
      
      # pack output - The R CODE! ----------------------------------------------
      the_file_path <- fn_PK_copies_folder_path()
      vector_code_lines <- readLines(the_file_path)
      
      Rcode_original <- fn_R_extract_code_between_markers(vector_code_lines = vector_code_lines, 
                                                    start_marker = "### INIT CODE ###", 
                                                    end_marker   = "### END CODE ###")
      

      # Extraer todas las coincidencias en cada posición del vector
      base_A <- "_A_"
      patron_A <- paste0(base_A, ".*?", base_A)
      vector_pattern_A  <- regmatches(Rcode_original, gregexpr(patron_A, Rcode_original))
      vector_pattern_A  <- unlist(vector_pattern_A)
      vector_names_A <- gsub(pattern = base_A, replacement = "", vector_pattern_A)
      
      vector_replacement_A_script <- active_DATASET_SELECTOR$"pack_output"$"str_import_external"
      vector_changes_A_script <- vector_replacement_A_script
      names(vector_changes_A_script) <- vector_pattern_A
      
      vector_replacement_A_quarto <- active_DATASET_SELECTOR$"pack_output"$"str_import_internal"
      vector_changes_A_quarto <- vector_replacement_A_quarto
      names(vector_changes_A_quarto) <- vector_pattern_A
      
      
      base_B <- "_B_"
      patron_B <- paste0(base_B, ".*?", base_B)
      vector_pattern_B  <- regmatches(Rcode_original, gregexpr(patron_B, Rcode_original))
      vector_pattern_B  <- unlist(vector_pattern_B)
      vector_names_B <- gsub(pattern = base_B, replacement = "", vector_pattern_B)
      vector_replacement_B <- active_VARIABLE_SELECTOR$"pack_output"[vector_names_B]
      vector_changes_B <- vector_replacement_B
      names(vector_changes_B) <- vector_pattern_B
      
      vector_changes_C <- c("#---" =  "")
      
      # Rcode Script (user visual use)
      list_vector_changes_script <- list(vector_changes_A_script, vector_changes_B, vector_changes_C)
      Rcode_script <- Rcode_original
      for (k in 1:length(list_vector_changes_script)){
        Rcode_script <- stringi::stri_replace_all_fixed(str = Rcode_script, 
                                                        pattern = names(list_vector_changes_script[[k]]), 
                                                        replacement = list_vector_changes_script[[k]],
                                                        vectorize_all = FALSE)
      }
      
      # Rcode Quarto (processing use)
      list_vector_changes_quarto <- list(vector_changes_A_quarto, vector_changes_B, vector_changes_C)
      Rcode_quarto <- Rcode_original
      for (k in 1:length(list_vector_changes_quarto)){
        Rcode_quarto <- stringi::stri_replace_all_fixed(str = Rcode_quarto, 
                                                        pattern = names(list_vector_changes_quarto[[k]]), 
                                                        replacement = list_vector_changes_quarto[[k]],
                                                        vectorize_all = FALSE)
      }
      
      # Pack output
      pack_output <- list("Rcode_original" = Rcode_original, 
                          "Rcode_script" = Rcode_script, 
                          "Rcode_quarto" = Rcode_quarto)

      # Check output -----------------------------------------------------------
      check_output <- !is.null(pack_output)
      button_class <- "confirmed"
      
      # Error message for check output -----------------------------------------
      if(!check_output){
        error_message <- paste0("Step: ", current_step)
        
      }
      
      # Filling internal_R_CODE ------------------------------------------------
      fn_shiny_apply_changes_reactiveValues(rv = internal_R_CODE, list(
        "pack_input"   = "",
        "check_input"  = TRUE,
        "pack_output"  = pack_output,
        "check_output" = check_output,
        "button_class" = button_class))
      
      # Filling new list -------------------------------------------------------
      new_list_step$"check_output"   <- check_output
      new_list_step$"pack_output"    <- ""
      new_list_step$"button_class"   <- button_class
      new_list_step$"error_message"  <- error_message
      
      # Validate the list ------------------------------------------------------
      check_list <- fn_R_validate_new_list(new_list = new_list_step, ref_list = default_list_step)
      
      # Error message for new list ---------------------------------------------
      if (!check_list) {
        fn_shiny_show_error_new_list(step_number = current_step, 
                                     new_list = new_list_step, 
                                     ref_list = default_list_step)
        
        return()  # Stop further execution
      }
      
      
      # Add --------------------------------------------------------------------
      isolate({
        current_step_name <- paste0(STR_STEP_NAME, current_step)
        APP_TOTEM[[current_step_name]] <- new_list_step
        number_current_step(current_step+1)
        fn_shiny_remove_future_steps(APP_TOTEM, current_step = current_step, STR_STEP_NAME)
        
      })
      
    })
    
    
    the_RS_UI <- reactiveVal()
    
    observe({
      req(OK_ALL_ACTIVE())
      # req(my_list_str_rv())
      
      req(internal_STR$"check_output")
      mi_super_lista <- reactiveValuesToList(internal_STR) #print(paste0("AVER: ", internal_STR$"pack_output"$"vector_str"$"str_01_MM_variable_selector"))
      
      # ----------------------------------------------------------------------
      #
      # Hardcoded master
      my_pack_name  <- "df_06_script"
      str_local_id  <- "the_06_script"
      #
      # Hardcoded basics
      my_df <- mi_super_lista$"pack_output"[[my_pack_name]]
      vector_short_names  <- my_df$"short_name"
      vector_full_names   <- my_df$"resource_name"
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
      my_str_MM_ui     <- full_name_MM_ui
      #-----------------------------------------------------------------------
      #
      ### ARGs
      args_server <- list(id = str_local_id, 
                          show_dev = FALSE,
                          active_DATASET_SELECTOR, 
                          active_TOOLS_SELECTOR,
                          active_VARIABLE_SELECTOR,
                          active_PLAY_SELECTOR,
                          active_R_CODE
      )
      
      ### Running server module - 04 - ORRS (Output R Results Shiny)
      do.call(my_str_MM_server, args_server)
      
      ### Running ui module - 04 - ORRS (Output R Results Shiny)
      args_ui <- list(id = ns("the_06_script"))
      the_rendered_iu <- do.call(my_str_MM_ui, args_ui)
      the_RS_UI(the_rendered_iu)
    })
    
    output$card06_script <- renderUI({
      req(the_RS_UI)
      
      div(
        style = "height: 100%;",  
        the_RS_UI()
      )
    })
    #---------------------------------------------------------------------------
    
    
    

    #---------------------------------------------------------------------------
    ### Initialization ---------------------------------------------------------
    
    default_structure <- list(
      pack_input = "",
      check_input = FALSE,
      pack_output = "",
      check_output = FALSE,
      button_class = "initial"
    )   
    

    ### ### ### END Initialization ---------------------------------------------
    #---------------------------------------------------------------------------
    
    
    
    # Button Tools - Server

    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

    
    output[["crystal05_A"]]  <- renderPrint({ MY_SELECTED_TOOL() })
    output[["crystal05_B1"]] <- renderPrint({ reactiveValuesToList(internal_CFG) })
    output[["crystal05_B2"]] <- renderPrint({ reactiveValuesToList(active_CFG) })
    
    output$"crystal05_ALL" <- renderUI({
      
      div(
        h2("Selected tool"), 
        verbatimTextOutput(ns("crystal05_A")),
        hr(),
        fluidRow(
          column(6, 
                 h2("Internal CFG"),
                 verbatimTextOutput(ns("crystal05_B1"))
          ),
          column(6, 
                 h2("Active CFG"),
                 verbatimTextOutput(ns("crystal05_B2"))
          )
        )
      )
      
    })
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    
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
      #
      # 05) Theory
      df_05_theory <- data.frame(
        "short_name" = c("MM_server", "MM_ui"),
        "str_end_file" = c("_MM_theory_server",
                           "_MM_theory_ui")
      )
      df_05_theory$"resource_name"   <- paste0(str_selected_tool, df_05_theory$"str_end_file")
      df_05_theory$"resource_exists" <- df_05_theory$"resource_name" %in% vector_pk_fn_vector
      check_05_theory <- all(df_05_theory$"resource_exists")
      # ------------------------------------------------------------------------
      #
      # 06) Script
      df_06_script <- data.frame(
        "short_name" = c("MM_server", "MM_ui"),
        "str_end_file" = c("_server",
                           "_ui")
      )
      df_06_script$"resource_name"   <- paste0("module_Render_script", df_06_script$"str_end_file")
      df_06_script$"resource_exists" <- df_05_theory$"resource_name" %in% vector_pk_fn_vector
      check_06_script <- all(df_06_script$"resource_exists")
      # ------------------------------------------------------------------------
      
    
      
    check_all_fn <- all(check_02_ZSV, 
                        check_03_RRC, check_04_ORRS, check_05_theory,
                        check_06_script)
      
    output_list <- list(
      "df_02_ZSV" = df_02_ZSV,
      "check_02_ZSV" = check_02_ZSV,
      "df_03_RRC" = df_03_RRC,
      "check_03_RRC" = check_03_RRC,
      "df_04_ORRS" = df_04_ORRS,
      "check_04_ORRS" = check_04_ORRS,
      "df_05_theory" = df_05_theory,
      "check_05_theory" = check_05_theory,
      "df_06_script" = df_06_script,
      "check_06_script" = check_06_script,
      
      "check_all_fn" = check_all_fn
      
      )
    
    fn_shiny_apply_changes_reactiveValues(rv = internal_STR, list(
        "pack_input"   = "",
        "check_input"  = TRUE,
        "pack_output"  = output_list,
        "check_output" = check_all_fn,
        "button_class" = "confirmed"))
      
    })
    
    output[["crystal04_A"]]  <- renderPrint({ MY_SELECTED_TOOL() })
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
    

    

    
    
    Sbutton_play2_server(id = "play2", 
                         default_structure, 
                         internal_DATASET_SELECTOR,  active_DATASET_SELECTOR,
                         internal_TOOLS_SELECTOR,    active_TOOLS_SELECTOR,
                         internal_STR,               active_STR,
                         internal_CFG,               active_CFG,
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
    
    ############################################################################
    
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
                        active_CFG$"check_output",
                        active_STR$"check_output",
                        active_VARIABLE_SELECTOR$"check_output",
                        active_PLAY_SELECTOR$"check_output")
      
      the_final_value <- all(vector_check)
      the_final_value
    })
    ############################################################################
    
    active_R_CODE   <- do.call(reactiveValues, default_structure)
    
    if(FALSE){
    the_RS_UI <- reactiveVal()
    
    observe({
      req(OK_ALL_ACTIVE())
      # req(my_list_str_rv())
      
      req(internal_STR$"check_output")
      mi_super_lista <- reactiveValuesToList(internal_STR) #print(paste0("AVER: ", internal_STR$"pack_output"$"vector_str"$"str_01_MM_variable_selector"))
      
      # ----------------------------------------------------------------------
      #
      # Hardcoded master
      my_pack_name  <- "df_06_script"
      str_local_id  <- "the_06_script"
      #
      # Hardcoded basics
      my_df <- mi_super_lista$"pack_output"[[my_pack_name]]
      vector_short_names  <- my_df$"short_name"
      vector_full_names   <- my_df$"resource_name"
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
      my_str_MM_ui     <- full_name_MM_ui
      #-----------------------------------------------------------------------
      #
      ### ARGs
      args_server <- list(id = str_local_id, 
                          show_dev = FALSE,
                          active_DATASET_SELECTOR, 
                          active_TOOLS_SELECTOR,
                          active_VARIABLE_SELECTOR,
                          active_PLAY_SELECTOR,
                          active_R_CODE
      )
      
      ### Running server module - 04 - ORRS (Output R Results Shiny)
      do.call(my_str_MM_server, args_server)
      
      ### Running ui module - 04 - ORRS (Output R Results Shiny)
      args_ui <- list(id = ns("the_06_script"))
      the_rendered_iu <- do.call(my_str_MM_ui, args_ui)
      the_RS_UI(the_rendered_iu)
    })
    
    output$card06_script <- renderUI({
      req(the_RS_UI)
      
      div(
        style = "height: 100%;",  
        the_RS_UI()
      )
    })
    }
    # observe(print(reactiveValuesToList( active_R_CODE)))
    
    
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
    
    ############################################################################
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
              "button_class" = "confirmed"
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
    
    # # Render 05 - Theory (output R Results Shiny)
    # Rendedir server and ui modules inside of renderUI!!!!!!
    output$card05_output22 <- renderUI({
      req(OK_ALL_ACTIVE())
      # req(my_list_str_rv())
      
      req(internal_STR$"check_output")
      mi_super_lista <- reactiveValuesToList(internal_STR) #print(paste0("AVER: ", internal_STR$"pack_output"$"vector_str"$"str_01_MM_variable_selector"))
      
      # ----------------------------------------------------------------------
      #
      # Hardcoded master
      my_pack_name  <- "df_05_theory"
      str_local_id  <- "the_05_theory"
      #
      # Hardcoded basics
      my_df <- mi_super_lista$"pack_output"[[my_pack_name]]
      vector_short_names  <- my_df$"short_name"
      vector_full_names   <- my_df$"resource_name"
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
      my_str_MM_ui     <- full_name_MM_ui
      #-----------------------------------------------------------------------
      #
      ### ARGs
      args_server <- list(id = str_local_id, 
                   show_dev = FALSE,
                   mis_valores = reactive(active_R_OBJECTS$"pack_output")
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