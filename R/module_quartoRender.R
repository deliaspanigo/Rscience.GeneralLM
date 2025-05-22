#' @export
module_quartoRenderer_ui <- function(id) {
  ns <- NS(id)
  
  div(
    uiOutput(ns("el_cartel")),
    card(
      card_header(
        h3("Download")
      ),
      card_body(
        fluidRow(
          column(10,
            uiOutput(ns("set01_RCode")),
            hr(),
            uiOutput(ns("set02_RLong")),
            hr()#,
            #uiOutput(ns("set03_RReport"))
        )#,
        # column(6, actionButton(ns("download_ALL"), "Download ALL", class = "btn-primary", icon = icon("download"))),
        ),
        br(),
        uiOutput(ns("visual_para_archivos")),
        # br(),
        # actionButton(ns("renderizar"), "Render VIEJO", class = "btn-primary"),
        
        # uiOutput(ns("render_status")),
        
        # Aquí se mostrará el HTML renderizado
        # div(
        #   id = ns("contenedor_html"),
        #   #style = "display: none; border: 1px solid #ddd; border-radius: 5px; padding: 15px; margin-top: 20px; overflow: visible; max-height: none;",
        #   htmlOutput(ns("quarto_iframe"))
        # )
        
      ),
      # Permitir que el card crezca según sea necesario
      height = "auto"
    )
  )
}

#' @export
module_quartoRenderer_server <- function(id, documento, Rcode_script, Rcode_quarto, active_TOOLS_SELECTOR) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$el_cartel <- renderUI({
      my_cartel <- reactiveValuesToList(active_TOOLS_SELECTOR)$"pack_output"$"selected_cartel"
      fn_html_cartel(my_text = my_cartel)
    })
    
    output$"set01_RCode" <- renderUI({
      
      the_class_render   <- internal_01_FILE_RCODE$"button_class"
#      the_class_download <- internal_01_FILE_RCODE$"button_class"
      
      btn_class_render <- switch(the_class_render,
                          "initial"   = "btn-outline-primary",    # Azul inicial
                          "confirmed" = "btn-success",    # Verde después de confirmar
                          "modified"  = "btn-outline-primary") 
      
        fluidRow(
          column(3, "R code"),
          column(2, actionButton(ns("render_RCode"), "Render", class = btn_class_render)),
          # column(2, "Status"),
          column(2, downloadButton(ns("download_RCode"), "Download", class = "btn-primary", icon = icon("download"))),
        )
    })
    
    output$"set02_RLong" <- renderUI({
      the_class_render   <- internal_02_RLONG$"button_class"
      #      the_class_download <- internal_01_FILE_RCODE$"button_class"
      
      btn_class_render <- switch(the_class_render,
                                 "initial"   = "btn-outline-primary",    # Azul inicial
                                 "confirmed" = "btn-success",    # Verde después de confirmar
                                 "modified"  = "btn-outline-primary") 
      
      fluidRow(
        column(3, "R code and Outputs"),
        column(2, actionButton(ns("render_RLong"), "Render", class = btn_class_render)),
        # column(2, "Status"),
        column(2, downloadButton(ns("download_RLong"), "Download", class = "btn-primary", icon = icon("download"))),
      )
    })
    
    # output$"set03_RReport" <- renderUI({
    #   
    #   the_class_render   <- internal_03_RREPORT$"button_class"
    #   #      the_class_download <- internal_01_FILE_RCODE$"button_class"
    #   
    #   btn_class_render <- switch(the_class_render,
    #                              "initial"   = "btn-outline-primary",    # Azul inicial
    #                              "confirmed" = "btn-success",    # Verde después de confirmar
    #                              "modified"  = "btn-outline-primary") 
    #   
    #   fluidRow(
    #     column(3, "Report"),
    #     column(2, actionButton(ns("render_RReport"), "Render", class = btn_class_render)),
    #     column(2, "Status"),
    #     column(2, actionButton(ns("download_RReport"), "Download", class = "btn-primary", icon = icon("download"))),
    #   )
    # })
    # 
    # 1) PK - Folder and file, names and paths
    # the_package_path <- find.package("Rscience.GeneralLM")
    # the_folder_path <- file.path(the_package_path, "quarto")
    
    # External input
    str_sub_folder <- reactive("GeneralLM_fix_anova1")
    
    ############################################################################    
    default_totem <- list(
      the_sys_time      = "",
      str_search        = "",
      folder_path       = "",
      check_folder_path = FALSE,
      vector_file_names = NA,
      vector_check_file_names = NA,
      check_general = FALSE,
      button_status = "initial"
      )
      

    internal_01_TOTEM_PK       <- do.call(reactiveValues, default_totem)
    internal_02_TOTEM_COPY     <- do.call(reactiveValues, default_totem)
    internal_03_TOTEM_DOWNLOAD <- do.call(reactiveValues, default_totem)
    
    ############################################################################
    
    # internal_01_TOTEM_PK
    observeEvent(str_sub_folder(),{
      fn_shiny_apply_changes_reactiveValues(rv = internal_01_TOTEM_PK, changes_list = default_totem)
      # fn_shiny_apply_changes_reactiveValues(rv = internal_02_TOTEM_COPY, changes_list = default_totem) 
      # fn_shiny_apply_changes_reactiveValues(rv = internal_03_TOTEM_DOWNLOAD, changes_list = default_totem)  

      current_time <- Sys.time()
      formatted_time <- format(current_time, "%Y_%m_%d_%H_%M_%S")
      the_sys_time <- formatted_time
      str_search   <- str_sub_folder()
      fn_shiny_apply_changes_reactiveValues(rv = internal_01_TOTEM_PK, changes_list = list(
        the_sys_time = the_sys_time,
        str_search   = str_search
      ))
      
      # PK folder
      pk_folder_path_quarto         <- fn_PK_quarto_folder_path()
      CHECK_PK_FOLDER <- dir.exists(pk_folder_path_quarto)
      if(!CHECK_PK_FOLDER){
        # print(pk_folder_path_quarto)
        showNotification("No encuentra o no existe la carpeta del package.", type = "error")
        return(NULL)
      }
      
            
      folder_path <- file.path(pk_folder_path_quarto, str_sub_folder())
      check_folder_path <- dir.exists(folder_path)
      if(!check_folder_path){
        showNotification("No encuentra o no existe la sub carpeta del package.", type = "error")
        return(NULL)
      }
      fn_shiny_apply_changes_reactiveValues(rv = internal_01_TOTEM_PK, changes_list = list(
        folder_path = folder_path,
        check_folder_path = check_folder_path
      ))
      
      
      vector_file_names <- list.files(folder_path, full.names = TRUE, recursive = TRUE)
      vector_check_file_names <- sapply(vector_file_names, function(x){file.exists(x)})
      check_general <- all(vector_check_file_names)
      if(!check_general){
        showNotification("No se encuentran los archivos de la sub carpeta.", type = "error")
        return(NULL)
      }
      
      button_status = "confirmed"
      
      fn_shiny_apply_changes_reactiveValues(rv = internal_01_TOTEM_PK, changes_list = list(
        vector_file_names = vector_file_names,
        vector_check_file_names = vector_check_file_names,
        check_general = check_general,
        button_status = button_status
      ))
      
      # print(reactiveValuesToList(internal_01_TOTEM_PK))
      # print("##################################################")
    })
    
    # internal_02_TOTEM_COPY 
    observeEvent(internal_01_TOTEM_PK,{
                 
      req(internal_01_TOTEM_PK$"check_general")
      
      fn_shiny_apply_changes_reactiveValues(rv = internal_02_TOTEM_COPY, changes_list = default_totem)
      
      the_sys_time <- internal_01_TOTEM_PK$"the_sys_time"
      str_search   <- internal_01_TOTEM_PK$"str_search"
      
      fn_shiny_apply_changes_reactiveValues(rv = internal_02_TOTEM_COPY, changes_list = list(
        the_sys_time = the_sys_time,
        str_search   = str_search
      ))    
      
      # 2) Folder path
      folder_path <- file.path(tempdir(), the_sys_time, str_search)
      if (dir.exists(folder_path)) unlink(folder_path, recursive = TRUE)
      dir.create(folder_path, recursive = TRUE)
      check_folder_path <- dir.exists(folder_path)
      
      if (!check_folder_path) {
        
      }
      fn_shiny_apply_changes_reactiveValues(rv = internal_02_TOTEM_COPY, changes_list = list(
        folder_path = folder_path,
        check_folder_path   = check_folder_path
      )) 
      
      ##########################################################################
      TOTEM_PK_folder_path <-  internal_01_TOTEM_PK$"folder_path"
      all_file_paths_TOTEM_PK <- list.files(TOTEM_PK_folder_path, full.names = TRUE, recursive = TRUE)
      
      # Copiar cada archivo a la carpeta destino
      for (archivo in all_file_paths_TOTEM_PK) {
        # Crear la ruta de destino manteniendo la estructura de subcarpetas
        ruta_destino <- file.path(folder_path, basename(archivo))
        
        # Copiar el archivo
        file.copy(archivo, ruta_destino)
      }
      
      vector_file_names <- list.files(folder_path, full.names = TRUE, recursive = TRUE)
      vector_check_file_names <- sapply(vector_file_names, function(x){file.exists(x)})
      check_general <- all(vector_check_file_names)
      if(!check_general){
        showNotification("No encuentra o no existe la carpeta del package.", type = "error")
        return(NULL)
      }
      
      button_status = "confirmed"
      fn_shiny_apply_changes_reactiveValues(rv = internal_02_TOTEM_COPY, changes_list = list(
        vector_file_names = vector_file_names,
        vector_check_file_names = vector_check_file_names,
        check_general = check_general,
        button_status = button_status
      ))
      
     
    })
    
    
    observeEvent(internal_02_TOTEM_COPY,{
      
      req(internal_02_TOTEM_COPY$"check_general")
      
      # fn_shiny_apply_changes_reactiveValues(rv = internal_03_TOTEM_DOWNLOAD, changes_list = default_totem)
      fn_shiny_apply_changes_reactiveValues(rv = internal_03_TOTEM_DOWNLOAD, 
                                            changes_list = reactiveValuesToList(internal_02_TOTEM_COPY))
      
      
      
    })
    
    
    
  
    
    
    default_structure_file_download <- list(
      # Stage 1 - Time and str_search
      the_sys_time            = "",
      str_search              = "",
      
      # Stage 2 - folder input path
      folder_input_path       = "",
      check_folder_input_path = FALSE,
      
      # Stage 3 - name and path input file
      name_input_file       = "",
      path_input_file       = "",
      check_path_input_file = FALSE,
      
      # Stage 4 - folder path output file
      folder_output_path       = "",
      check_folder_output_path = FALSE,
      

      # Stage 5 - fiel anme and file path output file
      name_output_file   = "",
      path_output_file   = "",
      check_path_output_file  = FALSE,
      
      button_class = "initial"
    )   
    
    
    
    #############################################################################
    THE_MODAL_RCODE <- reactiveVal(NULL)
    observeEvent(THE_MODAL_RCODE(),{
      
      if(THE_MODAL_RCODE()){
        # Mostrar el modal de carga
        # Mostrar el modal de carga con un spinner
        
      }
      
      if(!THE_MODAL_RCODE()){
        shinyjs::delay(2000, {
          removeModal()
        })
        THE_MODAL_RCODE(NULL)
      }
      
    })
    
    internal_01_FILE_RCODE <- do.call(reactiveValues, default_structure_file_download)
    observeEvent(input$"render_RCode", {
      
      req(internal_03_TOTEM_DOWNLOAD$"check_general")
          
      fn_shiny_apply_changes_reactiveValues(rv = internal_01_FILE_RCODE,  
                                            changes_list = default_structure_file_download
      )
      
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
      # Stage 1 - Time and str_search
      current_time <- Sys.time()
      formatted_time <- format(current_time, "%Y_%m_%d_%H_%M_%S")
      the_sys_time <- formatted_time
      
      str_search <- internal_03_TOTEM_DOWNLOAD$"str_search"
      fn_shiny_apply_changes_reactiveValues(rv = internal_01_FILE_RCODE, changes_list = list(
        the_sys_time = the_sys_time,
        str_search = str_search
      ))
      
      
      # Stage 2 - folder input path
      folder_input_path <- ""
      check_folder_input_path <- TRUE
      fn_shiny_apply_changes_reactiveValues(rv = internal_01_FILE_RCODE, changes_list = list(
        folder_input_path = folder_input_path,
        check_folder_input_path = check_folder_input_path
      ))
      
      # Stage 3 - name and path input file
      name_input_file       <- ""
      path_input_file       <- ""
      check_path_input_file <- TRUE
      fn_shiny_apply_changes_reactiveValues(rv = internal_01_FILE_RCODE, changes_list = list(
        name_input_file = name_input_file,
        path_input_file = path_input_file,
        check_path_input_file = check_path_input_file
      ))
      
      # Stage 4 - folder path output file
      folder_output_path       <- internal_03_TOTEM_DOWNLOAD$"folder_path"
      check_folder_output_path <- dir.exists(folder_output_path)
      if(!check_folder_output_path){
        showNotification("Problema - File 01 - Stage 4.", type = "error")
        return(NULL)
      }
      fn_shiny_apply_changes_reactiveValues(rv = internal_01_FILE_RCODE, changes_list = list(
        folder_output_path = folder_output_path,
        check_folder_output_path = check_folder_output_path
      ))
      
      # Stage 5 - fiel name and file path output file
      ._str_file_name   <- "anova1way"
      name_output_file  <- paste0(._str_file_name, "_", the_sys_time, ".R") 
      path_output_file  <- file.path(folder_output_path, name_output_file)
      
      #################################################################
      
      message(green("Starting process..."))
      message(green("Please, wait..."))
      writeLines(Rcode_script(), path_output_file)
      message(green("Process completed!"))
      message("")
      

      Sys.sleep(0.5)
      file_path <- path_output_file
      
      # Tiempo máximo de espera en segundos
      timeout <- 10
      
      # Intervalo de verificación en segundos
      check_interval <- 0.5
      
      # Objeto que comienza con un valor (FALSE)
      check_path_output_file <- FALSE
      
      # Contador de tiempo
      start_time <- Sys.time()
      
      # Bucle de verificación
      while (!file.exists(file_path)) {
        # Verificar si se ha superado el tiempo máximo de espera
        if (difftime(Sys.time(), start_time, units = "secs") > timeout) {
          cat("El archivo no se creó dentro del tiempo esperado.\n")
          break
        }
        
        # Esperar un intervalo antes de verificar nuevamente
        Sys.sleep(check_interval)
      }
      
      # # Si el archivo existe, cambiar el valor del objeto
      # if (file.exists(file_path)) {
      #   check_path_output_file <- TRUE
      # }
      #################################################################
      
      check_path_output_file  <- file.exists(path_output_file)
      if(!check_path_output_file){
        showNotification("Problema - File 01 - Stage 5.", type = "error")
        return(NULL)
      }
      button_class <- "confirmed"
      fn_shiny_apply_changes_reactiveValues(rv = internal_01_FILE_RCODE, changes_list = list(
        name_output_file = name_output_file,
        path_output_file =  path_output_file,
        check_path_output_file = check_path_output_file,
        button_class = button_class
      ))

      
      
      THE_MODAL_RCODE(FALSE)
    })
    
    
    output$visual_RCode <- renderText({
      req(internal_01_FILE_RCODE)
      mi_lista <- reactiveValuesToList(internal_01_FILE_RCODE)
      
      req(mi_lista$"check_path_output_file")
      # print(mi_lista)
      
      paste(readLines(mi_lista$"path_output_file"), collapse = "\n")
      
    })
    
    
    
    output$download_RCode <- downloadHandler(
      filename = function() {
        internal_01_FILE_RCODE$"name_output_file"
      },
      content = function(file) {
        req(internal_01_FILE_RCODE$"check_path_output_file")
        file.copy(internal_01_FILE_RCODE$"path_output_file", file)
      }
    )
    
    
    ###########################################################################################
    
    internal_02_RLONG <- do.call(reactiveValues, default_structure_file_download)

    THE_MODAL_RLong <- reactiveVal(NULL)
    observeEvent(THE_MODAL_RLong(),{
      
      if(THE_MODAL_RLong()){
        # Mostrar el modal de carga
        # Mostrar el modal de carga con un spinner
      
      }
      
      if(!THE_MODAL_RLong()){
        shinyjs::delay(2000, {
          removeModal()
        })
        THE_MODAL_RLong(NULL)
      }
      
    })

    
        
    observeEvent(input$"render_RLong", {
      
      req(internal_03_TOTEM_DOWNLOAD$"check_general")
      
      fn_shiny_apply_changes_reactiveValues(rv = internal_02_RLONG,  
                                            changes_list = default_structure_file_download
      )
      
      showModal(
        modalDialog(
          id = "miModalEspecifico2",  # Asignar un ID al modal
          title = "Procesando RLong...",
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
            tags$p("This may take a few moments. Please wait."),
            tags$p("It might take more than a minute. Please wait.")
          ),
          footer = NULL,  # No incluir botones en el modal
          easyClose = FALSE  # Evitar que el usuario cierre el modal manualmente
        )
      )
      
      ###
      # Stage 1 - Time and str_search
      current_time <- Sys.time()
      formatted_time <- format(current_time, "%Y_%m_%d_%H_%M_%S")
      the_sys_time <- formatted_time
      
      str_search <- internal_03_TOTEM_DOWNLOAD$"str_search"
      fn_shiny_apply_changes_reactiveValues(rv = internal_02_RLONG, changes_list = list(
        the_sys_time = the_sys_time,
        str_search = str_search
      ))
      
      
      # Stage 2 - folder input path
      folder_input_path <-  internal_02_TOTEM_COPY$"folder_path"
      check_folder_input_path <- TRUE
      fn_shiny_apply_changes_reactiveValues(rv = internal_02_RLONG, changes_list = list(
        folder_input_path = folder_input_path,
        check_folder_input_path = check_folder_input_path
      ))
      
      # Stage 3 - name and path input file
      name_input_file       <- "anova_RLong.qmd"
      path_input_file       <- file.path(folder_input_path, name_input_file)
      check_path_input_file <- file.exists(path_input_file)
      if(!check_path_input_file){
        showNotification("Problema - File 01 - Stage 3.", type = "error")
        return(NULL)
      }
      fn_shiny_apply_changes_reactiveValues(rv = internal_02_RLONG, changes_list = list(
        name_input_file = name_input_file,
        path_input_file = path_input_file,
        check_path_input_file = check_path_input_file
      ))
      
      # Stage 4 - folder path output file
      folder_output_path       <- folder_input_path
      check_folder_output_path <- dir.exists(folder_output_path)
      if(!check_folder_output_path){
        showNotification("Problema - File 01 - Stage 4.", type = "error")
        return(NULL)
      }
      fn_shiny_apply_changes_reactiveValues(rv = internal_02_RLONG, changes_list = list(
        folder_output_path = folder_output_path,
        check_folder_output_path = check_folder_output_path
      ))
      
      # Stage 5 - fiel name and file path output file
      ._str_file_name   <- tools::file_path_sans_ext(basename(name_input_file))
      ._output_ext <- ".html"
      name_output_file  <- paste0(._str_file_name, ._output_ext) 
      path_output_file  <- file.path(folder_output_path, name_output_file)
      
      #################################################################
      
      # file.copy(from = path_input_file, 
      #             to = path_output_file, overwrite = TRUE)
      
      dir_actual <- getwd()
      
      my_temporal_folder <- folder_output_path
      # Cambiar al directorio temporal
      setwd(my_temporal_folder)
      
      # Ejecutar el comando quarto render
      # comando_render <- paste0("quarto render ", nombre_archivo)
      # sistema_output <- system(comando_render, intern = TRUE)
      
      codigo_extra <- paste(paste(Rcode_quarto(), collapse = "\n"))
      
      lineas <- readLines(name_input_file)
      lineas <- sub(pattern = '"_mi_codigo_"', replacement = codigo_extra, x = lineas)
      writeLines(lineas, name_input_file)
      
      message(green("Starting process..."))
      message(green("Please, wait..."))
      quarto::quarto_render(input = name_input_file, 
                            output_file = name_output_file,
                            quiet = TRUE)
      message(green("Process completed!"))
      message("")
      
      # Volver al directorio original
      setwd(dir_actual)
      
      Sys.sleep(0.5)
      
      #################################################################
      file_path <- path_output_file
      
      # Tiempo máximo de espera en segundos
      timeout <- 10
      
      # Intervalo de verificación en segundos
      check_interval <- 0.5
      
      # Objeto que comienza con un valor (FALSE)
      check_path_output_file <- FALSE
      
      # Contador de tiempo
      start_time <- Sys.time()
      
      # Bucle de verificación
      while (!file.exists(file_path)) {
        # Verificar si se ha superado el tiempo máximo de espera
        if (difftime(Sys.time(), start_time, units = "secs") > timeout) {
          cat("El archivo no se creó dentro del tiempo esperado.\n")
          break
        }
        
        # Esperar un intervalo antes de verificar nuevamente
        Sys.sleep(check_interval)
      }
      
      
      
      check_path_output_file  <- file.exists(path_output_file)
      
      if(!check_path_output_file){
        showNotification("Problema - File 01 - Stage 5.", type = "error")
        return(NULL)
      }
      button_class <- "confirmed"
      fn_shiny_apply_changes_reactiveValues(rv = internal_02_RLONG, changes_list = list(
        name_output_file = name_output_file,
        path_output_file =  path_output_file,
        check_path_output_file = check_path_output_file,
        button_class = button_class
      ))
      
      
      
      THE_MODAL_RLong(FALSE)
    })
    
    
    
    output$visual_RLong <- renderText({
      req(internal_02_RLONG)
      mi_lista <- reactiveValuesToList(internal_02_RLONG)
      req(mi_lista$check_path_output_file)
      
      file_name_html <- mi_lista$"name_output_file"
      dir_temp <- mi_lista$"folder_output_path"
      
      # check_file_RReport()
      addResourcePath(prefix = "output_temp_folder", directoryPath = dir_temp)
      my_local_file <- file.path("output_temp_folder", file_name_html)
      
      
      # tags$iframe(
      #   src = my_local_file,
      #   width = "100%",
      #   height = "800px",
      #   frameborder = 0
      # )
      armado_v <- paste('<div style="height: 100%; width: 100%; overflow: hidden;"><iframe style="height: 2500vh; width:100%; border: none;" src="', my_local_file, '"></iframe></div>', sep = "")
      
      return(armado_v)
    })
    
    
    output$download_RLong <- downloadHandler(
      filename = function() {
        internal_02_RLONG$"name_output_file"
      },
      content = function(file) {
        req(internal_02_RLONG$"check_path_output_file")
        file.copy(internal_02_RLONG$"path_output_file", file)
      }
    )
    
    
    
    ###########################################################################################
    
    
    internal_03_RREPORT    <- do.call(reactiveValues, default_structure_file_download)
    observeEvent(input$"render_RReport", {
      
      req(internal_03_TOTEM_DOWNLOAD$"check_general")
      
      fn_shiny_apply_changes_reactiveValues(rv = internal_03_RREPORT,  
                                            changes_list = default_structure_file_download
      )
      
      
      ###
      # Stage 1 - Time and str_search
      current_time <- Sys.time()
      formatted_time <- format(current_time, "%Y_%m_%d_%H_%M_%S")
      the_sys_time <- formatted_time
      
      str_search <- internal_03_TOTEM_DOWNLOAD$"str_search"
      fn_shiny_apply_changes_reactiveValues(rv = internal_03_RREPORT, changes_list = list(
        the_sys_time = the_sys_time,
        str_search = str_search
      ))
      
      
      # Stage 2 - folder input path
      folder_input_path <-  internal_02_TOTEM_COPY$"folder_path"
      check_folder_input_path <- TRUE
      fn_shiny_apply_changes_reactiveValues(rv = internal_03_RREPORT, changes_list = list(
        folder_input_path = folder_input_path,
        check_folder_input_path = check_folder_input_path
      ))
      
      # Stage 3 - name and path input file
      name_input_file       <- "anova_RReport.qmd"
      path_input_file       <- file.path(folder_input_path, name_input_file)
      check_path_input_file <- file.exists(path_input_file)
      if(!check_path_input_file){
        showNotification("Problema - File 01 - Stage 3.", type = "error")
        return(NULL)
      }
      fn_shiny_apply_changes_reactiveValues(rv = internal_03_RREPORT, changes_list = list(
        name_input_file = name_input_file,
        path_input_file = path_input_file,
        check_path_input_file = check_path_input_file
      ))
      
      # Stage 4 - folder path output file
      folder_output_path       <- folder_input_path
      check_folder_output_path <- dir.exists(folder_output_path)
      if(!check_folder_output_path){
        showNotification("Problema - File 01 - Stage 4.", type = "error")
        return(NULL)
      }
      fn_shiny_apply_changes_reactiveValues(rv = internal_03_RREPORT, changes_list = list(
        folder_output_path = folder_output_path,
        check_folder_output_path = check_folder_output_path
      ))
      
      # Stage 5 - fiel name and file path output file
      ._str_file_name   <- tools::file_path_sans_ext(basename(name_input_file))
      ._output_ext <- ".html"
      name_output_file  <- paste0(._str_file_name, ._output_ext) 
      path_output_file  <- file.path(folder_output_path, name_output_file)
      
      #################################################################
      
      # file.copy(from = path_input_file, 
      #             to = path_output_file, overwrite = TRUE)
      
      dir_actual <- getwd()
      
      my_temporal_folder <- folder_output_path
      # Cambiar al directorio temporal
      setwd(my_temporal_folder)
      
      # Ejecutar el comando quarto render
      # comando_render <- paste0("quarto render ", nombre_archivo)
      # sistema_output <- system(comando_render, intern = TRUE)
      
      quarto::quarto_render(input = name_input_file, output_file = name_output_file)
      
      # Volver al directorio original
      setwd(dir_actual)
      
      Sys.sleep(0.5)
      
      #################################################################
      file_path <- path_output_file
      
      # Tiempo máximo de espera en segundos
      timeout <- 10
      
      # Intervalo de verificación en segundos
      check_interval <- 0.5
      
      # Objeto que comienza con un valor (FALSE)
      check_path_output_file <- FALSE
      
      # Contador de tiempo
      start_time <- Sys.time()
      
      # Bucle de verificación
      while (!file.exists(file_path)) {
        # Verificar si se ha superado el tiempo máximo de espera
        if (difftime(Sys.time(), start_time, units = "secs") > timeout) {
          cat("El archivo no se creó dentro del tiempo esperado.\n")
          break
        }
        
        # Esperar un intervalo antes de verificar nuevamente
        Sys.sleep(check_interval)
      }
      
    
      
      check_path_output_file  <- file.exists(path_output_file)
      
      if(!check_path_output_file){
        showNotification("Problema - File 01 - Stage 5.", type = "error")
        return(NULL)
      }
      button_class <- "confirmed"
      fn_shiny_apply_changes_reactiveValues(rv = internal_03_RREPORT, changes_list = list(
        name_output_file = name_output_file,
        path_output_file =  path_output_file,
        check_path_output_file = check_path_output_file,
        button_class = button_class
      ))
      
      
      
      
    })
    
    

    output$visual_RReport <- renderText({
      req(internal_03_RREPORT)
      mi_lista <- reactiveValuesToList(internal_03_RREPORT)
      req(mi_lista$check_path_output_file)
      
      file_name_html <- mi_lista$"name_output_file"
      dir_temp <- mi_lista$"folder_output_path"
    
      # check_file_RReport()
      addResourcePath(prefix = "output_temp_folder", directoryPath = dir_temp)
      my_local_file <- file.path("output_temp_folder", file_name_html)
      

      # tags$iframe(
      #   src = my_local_file,
      #   width = "100%",
      #   height = "800px",
      #   frameborder = 0
      # )
      armado_v <- paste('<div style="height: 100%; width: 100%; overflow: hidden;"><iframe style="height: 2500vh; width:100%; border: none;" src="', my_local_file, '"></iframe></div>', sep = "")
      
      return(armado_v)
    })
    
    
    output$download_RReport <- downloadHandler(
      filename = function() {
        internal_03_RREPORT$"name_output_file"
      },
      content = function(file) {
        req(internal_03_RREPORT$"check_path_output_file")
        file.copy(internal_03_RREPORT$"path_output_file", file)
      }
    )
    
    
    ###########################################################################################
    
    output$"visual_para_archivos" <- renderUI({
      
      
      bslib::navset_card_tab(
        # title = "R for Science",
        id = ns("mynav"),
        height = "100%",  # Especificar altura explícitamente
        bslib::nav_panel(title = "R Code",
                         verbatimTextOutput(ns("visual_RCode"))
        ),
        bslib::nav_panel(title = "R Code and Outputs",
                         htmlOutput(ns("visual_RLong"))# htmlOutput(ns("visual_RLong"))
        )#,
        # bslib::nav_panel(title = "Report",
        #                  # uiOutput(ns("visual_RReport"))
        #                  htmlOutput(ns("visual_RReport"))
        #                  # "El RCODE AND OUTPUS")
        # )
      )
      
    })
    
    ###########################################################################################
    ###########################################################################################
    ###########################################################################################
    ###########################################################################################
    ###########################################################################################
    
    if(FALSE){
    ############################################################################################
    
    
    
    ############################################################################
    
    
      observeEvent(input$"render_RReport", {
        # print(dir.exists(temp_folder_path_01_LAB))
        # print(list.files(temp_folder_path_01_LAB, all.files = TRUE, recursive = TRUE))
        
        fn_shiny_apply_changes_reactiveValues(rv = internal_03_RREPORT,  
                                              changes_list = default_structure_download
        )
        
        # Inputs
        original_pk_file_path <- pk_file_path_03_RReport_qmd
        ext_output <- ".html"
        the_temp_output_folder_path <- temp_folder_path_01_LAB
        
        # Basics
        original_pk_file_name <- basename(original_pk_file_path)
        original_pk_folder_path <- dirname(original_pk_file_path)
        
        
        # Obtener el tiempo del sistema
        current_time <- Sys.time()
        formatted_time <- format(current_time, "%Y_%m_%d_%H_%M_%S")
        str_file_name <- tools::file_path_sans_ext(original_pk_file_name)
        str_mod <- paste0(str_file_name, "_", formatted_time, ext_output) 
        
        # 1) Folder input --------------------------------------------------------
        folder_input_path       <- original_pk_folder_path
        check_folder_input_path <- dir.exists(folder_input_path) 
        if(!check_folder_input_path){
          showNotification("La carpeta de entrada no existe.", type = "error")
        }
        
        fn_shiny_apply_changes_reactiveValues(rv = internal_03_RREPORT,  changes_list = list(
          "folder_input_path" = folder_input_path,
          "check_folder_input_path" = check_folder_input_path
        ))
        
        
        # 2) File Input #-----------------------------------
        name_input_file       <- original_pk_file_name
        path_input_file       <- original_pk_file_path
        check_path_input_file <- file.exists(path_input_file) 
        if(!check_path_input_file){
          showNotification("El path del archivo de entrada no existe.", type = "error")
        }
        fn_shiny_apply_changes_reactiveValues(rv = internal_03_RREPORT,  changes_list = list(
          "name_input_file" = name_input_file,
          "path_input_file" = path_input_file,
          "check_path_input_file" = check_path_input_file
        ))
        
        # 3) Output folder -------------------------------------------------------
        folder_output_path       <- the_temp_output_folder_path
        check_folder_output_path <- dir.exists(folder_output_path)
        if(!check_folder_output_path){
          showNotification("La carpeta de salida no existe.", type = "error")
          
        }
        fn_shiny_apply_changes_reactiveValues(rv = internal_03_RREPORT,  changes_list = list(
          "folder_output_path" = folder_output_path,
          "check_folder_output_path" = check_folder_output_path
        ))
        
        
        # 4) Output file name ----------------------------------------------------
        name_output_file   <- str_mod
        path_output_file   <- file.path(folder_output_path, name_output_file)
        fn_shiny_apply_changes_reactiveValues(rv = internal_03_RREPORT,  changes_list = list(
          "name_output_file" = name_output_file,
          "path_output_file" = path_output_file
        ))
        
        # writeLines(Rcode_script(), path_output_file)
        file.copy(from = path_input_file, 
                    to = path_output_file, overwrite = TRUE)
        
        dir_actual <- getwd()
        
        # Cambiar al directorio temporal
        setwd(folder_output_path)
        
        # Ejecutar el comando quarto render
        # comando_render <- paste0("quarto render ", nombre_archivo)
        # sistema_output <- system(comando_render, intern = TRUE)
        
        quarto::quarto_render(input = name_output_file)
        
        # Volver al directorio original
        setwd(dir_actual)
        
        Sys.sleep(0.5)
        
        #################################################################
        file_path <- path_output_file
        
        # Tiempo máximo de espera en segundos
        timeout <- 10
        
        # Intervalo de verificación en segundos
        check_interval <- 0.5
        
        # Objeto que comienza con un valor (FALSE)
        check_path_output_file <- FALSE
        
        # Contador de tiempo
        start_time <- Sys.time()
        
        # Bucle de verificación
        while (!file.exists(file_path)) {
          # Verificar si se ha superado el tiempo máximo de espera
          if (difftime(Sys.time(), start_time, units = "secs") > timeout) {
            cat("El archivo no se creó dentro del tiempo esperado.\n")
            break
          }
          
          # Esperar un intervalo antes de verificar nuevamente
          Sys.sleep(check_interval)
        }
        
        # Si el archivo existe, cambiar el valor del objeto
        if (file.exists(file_path)) {
          check_path_output_file <- TRUE
        }
        
        ################################################################
        # check_path_output_file <- file.exists(path_output_file)
        
        
        if(!check_path_output_file){
          showNotification("El archivo de salida no se genero o paso algo.", type = "error")
          
        }
        button_class <- "confirmed"
        
        fn_shiny_apply_changes_reactiveValues(rv = internal_03_RREPORT,  changes_list = list(
          "check_path_output_file" = check_path_output_file,
          "button_class" = button_class
        ))
        
        
        # print(list.files(temp_folder_path_01_LAB, all.files = TRUE, recursive = TRUE))
        # print(reactiveValuesToList(internal_03_RREPORT))
      })
      
    observeEvent(input$"render_RReport", {
      # print(list.files(temp_folder_path_01_LAB, all.files = T, recursive = T))
      

      file.copy(from = pk_file_path_03_RReport_qmd , 
                  to = input_path_file_temporal, overwrite = TRUE)
      ###
      # Guardar el directorio actual
      dir_actual <- getwd()
      
      # Cambiar al directorio temporal
      setwd(temp_folder_path_01_LAB)
      
      # Ejecutar el comando quarto render
      # comando_render <- paste0("quarto render ", nombre_archivo)
      # sistema_output <- system(comando_render, intern = TRUE)
      
      quarto::quarto_render(input = input_file_name_qmd, 
                            output_file = output_file_name_html)
      
      # Volver al directorio original
      setwd(dir_actual)
      ###
      
      # Verificar si el archivo existe después de escribir
      if (file.exists(output_path_file_html)) {
        check_file_RReport(TRUE)
      } else {
        check_file_RReport(FALSE)
      }
      # print(list.files(temp_folder_path_01_LAB, all.files = T, recursive = T))
    })
    
    output$visual_RReport <- renderText({
      
      req(check_file_RReport())
      
      file_name_html <- "anova.html"
      dir_temp <- temp_folder_path_01_LAB
      
      # check_file_RReport()
      addResourcePath(prefix = "output_temp_folder", directoryPath = dir_temp)
      my_local_file <- file.path("output_temp_folder", file_name_html)
      
      armado_v <- paste('<div style="height: 100%; width: 100%; overflow: hidden;"><iframe style="height: 2500vh; width:100%; border: none;" src="', my_local_file, '"></iframe></div>', sep = "")
      
      return(armado_v)
    })
    
    output$download_RReport <- downloadHandler(
      filename = function() {
        output_file_name_RReport
      },
      content = function(file) {
        if (file.exists(outputfile_path_internal_RReport)) {
          file.copy(file_path_internal_RReport, file)
        } else {
          stop("El archivo no existe en la ruta esperada.")
        }
      }
    )
    ############################################################################
    
    
      
    
    output$"visual_para_archivos" <- renderUI({
      
    
          bslib::navset_card_tab(
            # title = "R for Science",
            id = ns("mynav"),
            height = "100%",  # Especificar altura explícitamente
            bslib::nav_panel(title = "R code",
                             verbatimTextOutput(ns("visual_RCode"))
            ),
            bslib::nav_panel(title = "R Code and Outputs",
                             "R code + Outputs + Plots (html)")#,
            # bslib::nav_panel(title = "Report",
            #                  htmlOutput(ns("visual_RReport")))
          )
          
    
    })
    
    
    
    
    
    
    
    
    ############################################################################
    ############################################################################
    ############################################################################
    ############################################################################
    
    # Estado del renderizado
    estado_renderizado <- reactiveVal("no_iniciado") # Puede ser: no_iniciado, en_proceso, finalizado, error
    
    # Estados de los pasos
    pasos_estado <- reactiveValues(
      crear_carpeta = "pendiente",
      copiar_archivo = "pendiente",
      renderizar = "pendiente",
      verificar = "pendiente"
    )
    
    # Función para actualizar un paso
    actualizar_paso <- function(paso, estado) {
      pasos_estado[[paso]] <- estado
      
      # Actualizar la UI del paso en el modal
      icono <- ""
      color <- ""
      
      if (estado == "pendiente") {
        icono <- "circle"
        color <- "#6c757d"
      } else if (estado == "en_proceso") {
        icono <- "spinner fa-spin"
        color <- "#0d6efd"
      } else if (estado == "completado") {
        icono <- "check-circle"
        color <- "#198754"
      } else if (estado == "error") {
        icono <- "times-circle"
        color <- "#dc3545"
      }
      
      html_actualizado <- paste0(
        '<i class="fa fa-', icono, '" style="color: ', color, '; margin-right: 10px;"></i>',
        obtener_texto_paso(paso)
      )
      
      shinyjs::html(id = paste0("paso_", paso), html = html_actualizado)
    }
    
    # Función para obtener el texto descriptivo de cada paso
    obtener_texto_paso <- function(paso) {
      switch(paso,
             crear_carpeta = "Crear carpeta temporal",
             copiar_archivo = "Copiar archivo QMD",
             renderizar = "Renderizar documento",
             verificar = "Verificar existencia del archivo HTML"
      )
    }
    
    # Observa el clic en el botón de renderizar
    observeEvent(input$renderizar, {
      # Actualizar estado general
      estado_renderizado("en_proceso")
      
      # Reiniciar estados de los pasos
      pasos_estado$crear_carpeta <- "pendiente"
      pasos_estado$copiar_archivo <- "pendiente"
      pasos_estado$renderizar <- "pendiente"
      pasos_estado$verificar <- "pendiente"
      
      # Ocultar contenedor del HTML (por si se vuelve a renderizar)
      shinyjs::hide("contenedor_html")
      
      # Crear un ID para el modal
      modal_id <- session$ns("modal_procesamiento")
      
      # Contenido de la ventana modal con la lista de pasos
      modal_content <- div(
        id = session$ns("modal_content"),
        style = "padding: 20px;",
        
        # Spinner central visible durante el proceso
        div(
          id = session$ns("spinner_container"),
          style = "text-align: center; margin-bottom: 25px;",
          div(class = "spinner-border text-primary", style = "width: 3rem; height: 3rem;", role = "status"),
          div(style = "margin-top: 15px; font-size: 16px; font-weight: bold;", "Procesando documento Quarto")
        ),
        
        
        # Lista de pasos con sus estados
        div(
          style = "background-color: #f8f9fa; border-radius: 8px; padding: 15px; margin-bottom: 20px;",
          
          # Paso 1: Crear carpeta temporal
          div(
            id = session$ns("paso_crear_carpeta"),
            style = "margin-bottom: 12px; font-size: 15px; display: flex; align-items: center;",
            HTML('<i class="fa fa-circle" style="color: #6c757d; margin-right: 10px;"></i>Crear carpeta temporal')
          ),
          
          # Paso 2: Copiar archivo QMD
          div(
            id = session$ns("paso_copiar_archivo"),
            style = "margin-bottom: 12px; font-size: 15px; display: flex; align-items: center;",
            HTML('<i class="fa fa-circle" style="color: #6c757d; margin-right: 10px;"></i>Copiar archivo QMD')
          ),
          
          # Paso 3: Renderizar documento
          div(
            id = session$ns("paso_renderizar"),
            style = "margin-bottom: 12px; font-size: 15px; display: flex; align-items: center;",
            HTML('<i class="fa fa-circle" style="color: #6c757d; margin-right: 10px;"></i>Renderizar documento')
          ),
          
          # Paso 4: Verificar existencia del archivo HTML
          div(
            id = session$ns("paso_verificar"),
            style = "margin-bottom: 12px; font-size: 15px; display: flex; align-items: center;",
            HTML('<i class="fa fa-circle" style="color: #6c757d; margin-right: 10px;"></i>Verificar existencia del archivo HTML')
          )
        ),
        
        # Contenedor de mensaje de finalización
        div(
          id = session$ns("mensaje_final"),
          style = "display: none; text-align: center;",
          div(
            id = session$ns("icono_final"),
            style = "font-size: 40px; margin-bottom: 15px;"
          ),
          div(
            id = session$ns("texto_final"),
            style = "font-size: 18px; font-weight: bold; margin-bottom: 10px;"
          ),
          div(
            id = session$ns("descripcion_final"),
            style = "font-size: 14px; margin-bottom: 20px;"
          ),
          actionButton(session$ns("cerrar_modal"), "Cerrar", class = "btn-primary")
        ),
        
        # Mensaje mientras está procesando
        div(
          id = session$ns("mensaje_procesando"),
          style = "text-align: center; color: #6c757d; font-size: 14px; margin-top: 15px;",
          "Esta ventana se actualizará automáticamente a medida que avance el proceso."
        )
      )
      
      # Mostrar ventana modal de procesamiento que bloquea la interacción
      showModal(
        modalDialog(
          id = modal_id,
          title = "Procesando documento Quarto",
          modal_content,
          footer = NULL,
          easyClose = FALSE,
          size = "m"
        )
      )
      
      # Ejecutar renderizado
      tryCatch({
        # PASO 1: Crear carpeta temporal
        actualizar_paso("crear_carpeta", "en_proceso")
        
        # Ruta al archivo quarto especificado en el argumento
        ruta_quarto <- documento
        
        # Crear directorio temporal para el procesamiento
        dir_temp <- file.path(tempdir(), "quarto_temp")
        if (!dir.exists(dir_temp)) {
          dir.create(dir_temp, recursive = TRUE)
        }
        
        # Marcar como completado
        actualizar_paso("crear_carpeta", "completado")
        Sys.sleep(0.5) # Pequeña pausa para visualizar el cambio
        
        # PASO 2: Copiar archivo QMD
        actualizar_paso("copiar_archivo", "en_proceso")
        
        # Extraer el nombre del archivo sin la ruta
        nombre_archivo <- basename(ruta_quarto)
        
        # Copiar el archivo .qmd a la carpeta temporal
        archivo_temp <- file.path(dir_temp, nombre_archivo)
        file.copy(from = ruta_quarto, to = archivo_temp, overwrite = TRUE)
        
        # Marcar como completado
        actualizar_paso("copiar_archivo", "completado")
        Sys.sleep(0.5) # Pequeña pausa para visualizar el cambio
        
        # PASO 3: Renderizar documento
        actualizar_paso("renderizar", "en_proceso")
        
        # Guardar el directorio actual
        dir_actual <- getwd()
        
        # Cambiar al directorio temporal
        setwd(dir_temp)
        
        # Ejecutar el comando quarto render
        comando_render <- paste0("quarto render ", nombre_archivo)
        sistema_output <- system(comando_render, intern = TRUE)
        
        # Volver al directorio original
        setwd(dir_actual)
        
        # Marcar como completado
        actualizar_paso("renderizar", "completado")
        Sys.sleep(0.5) # Pequeña pausa para visualizar el cambio
        
        # PASO 4: Verificar existencia del archivo HTML
        actualizar_paso("verificar", "en_proceso")
        
        # Obtener nombre del archivo de salida (cambiando la extensión a .html)
        nombre_archivo_base <- tools::file_path_sans_ext(nombre_archivo)
        file_name_html <- paste0(nombre_archivo_base, ".html")
        file_path_html <- file.path(dir_temp, file_name_html)
        
        # Verificar la existencia del archivo HTML generado
        if (!file.exists(file_path_html)) {
          stop("No se pudo encontrar el archivo HTML generado")
        }
        
        # Marcar como completado
        actualizar_paso("verificar", "completado")
        Sys.sleep(0.5) # Pequeña pausa para visualizar el cambio
        
        # Leer el contenido del archivo HTML
        html_content <- readLines(file_path_html, warn = FALSE)
        html_content <- paste(html_content, collapse = "\n")
        
        # Actualizar el UI con el contenido HTML
        # Actualizar el UI con el iframe que muestra el documento HTML
        output$quarto_iframe <- renderText({
          # En lugar de usar file://, usar addResourcePath para servir el archivo
          addResourcePath(prefix = "output_temp_folder", directoryPath = dir_temp)
          my_local_file <- file.path("output_temp_folder", file_name_html)
          
          armado_v <- paste('<div style="height: 100%; width: 100%; overflow: hidden;"><iframe style="height: 2500vh; width:100%; border: none;" src="', my_local_file, '"></iframe></div>', sep = "")
          
          return(armado_v)
          
        })
        
        
        
        # Mostrar el contenedor del HTML
        shinyjs::show("contenedor_html")
        
        # Actualizar estado
        estado_renderizado("finalizado")
        
        # Ocultar mensaje de procesando
        shinyjs::hide("mensaje_procesando")
        
        # Cambiar mensaje final
        # Ocultar el spinner y mostrar un check grande verde
        shinyjs::hide(selector = ".spinner-border.text-primary")
        shinyjs::html("icono_final", '<i class="fa fa-check-circle" style="color: #198754; font-size: 80px;"></i>')
        shinyjs::html("texto_final", "¡Proceso completado con éxito!")
        shinyjs::html("descripcion_final", "El documento Quarto ha sido renderizado correctamente y está listo para ser visualizado.")
        shinyjs::show("mensaje_final")
        
        
        
        
        # Cambiar clase del botón
        shinyjs::removeClass("cerrar_modal", "btn-primary")
        shinyjs::addClass("cerrar_modal", "btn-success")
        
        # Notificación personalizada de éxito (aparecerá después de cerrar el modal)
        showNotification(
          ui = tags$div(
            style = "background-color: #d1e7dd; color: #0f5132; font-size: 15px; font-weight: bold; padding: 10px; border-radius: 4px; border-left: 5px solid #0f5132; display: flex; align-items: center;",
            tags$i(
              class = "fa fa-check-circle",
              style = "font-size: 24px; margin-right: 10px;"
            ),
            "Documento Quarto renderizado exitosamente"
          ),
          duration = 6,
          closeButton = TRUE
        )
        
        
      }, error = function(e) {
        # Marcar el último paso ejecutado como error
        for (paso in c("crear_carpeta", "copiar_archivo", "renderizar", "verificar")) {
          if (pasos_estado[[paso]] == "en_proceso") {
            actualizar_paso(paso, "error")
            break
          }
        }
        
        # Actualizar estado general
        estado_renderizado("error")
        
        # Ocultar mensaje de procesando
        shinyjs::hide("mensaje_procesando")
        
        # Cambiar mensaje final
        shinyjs::html("icono_final", '<i class="fa fa-times-circle" style="color: #dc3545;"></i>')
        shinyjs::html("texto_final", "Error en el procesamiento")
        shinyjs::html("descripcion_final", paste("Se ha producido un error:", as.character(e)))
        shinyjs::show("mensaje_final")
        
        # Cambiar clase del botón
        shinyjs::removeClass("cerrar_modal", "btn-primary")
        shinyjs::addClass("cerrar_modal", "btn-danger")
        
        # Notificación de error (aparecerá después de cerrar el modal)
        showNotification(
          ui = tags$div(
            style = "background-color: #f8d7da; color: #721c24; font-size: 15px; font-weight: bold; padding: 10px; border-radius: 4px; border-left: 5px solid #721c24; display: flex; align-items: center;",
            tags$i(
              class = "fa fa-exclamation-circle",
              style = "font-size: 24px; margin-right: 10px;"
            ),
            paste("Error al renderizar:", as.character(e))
          ),
          duration = 8,
          closeButton = TRUE
        )
      })
    })
    
    # Observar clic en botón "Cerrar"
    # Observar clic en botón "Cerrar"
    if(FALSE){
    observeEvent(input$cerrar_modal, {
      # Intentar el método estándar
      removeModal()
      
      # Script JavaScript mejorado que también restaura el scrolling
      shinyjs::runjs('
    // Eliminar modales visibles
    $(".modal").hide().remove();

    // Eliminar todos los backdrops
    $(".modal-backdrop").hide().remove();

    // Restaurar el scrolling y limpiar efectos en el body
    $("body").removeClass("modal-open");
    $("body").css({
      "padding-right": "",
      "overflow": "",     // Restaurar overflow
      "overflow-y": ""    // Asegurar que overflow-y también se restaure
    });

    // Desbloquear el scroll en html también
    $("html").css({
      "overflow": "",
      "overflow-y": ""
    });

    // Remover cualquier contenido modal persistente
    $("#shiny-modal").remove();
    $(".shiny-modal").remove();

    // Forzar actualización del DOM y restaurar comportamiento de scroll
    setTimeout(function() {
      $(window).trigger("resize");

      // Asegurar que el scrolling está habilitado en todos los elementos importantes
      $("html, body, .container-fluid, .tab-content").css("overflow", "");

      // Verificar si aún hay algún elemento con overflow:hidden
      $("*").each(function() {
        if ($(this).css("overflow") === "hidden" &&
            !$(this).hasClass("dropdown-menu") &&
            !$(this).hasClass("collapse")) {
          $(this).css("overflow", "");
        }
      });
    }, 100);
  ')
    })
    }
    
    
    
    
    # Renderizar el estado actual
    output$render_status <- renderUI({
      estado <- estado_renderizado()
      
      if (estado == "no_iniciado") {
        return(NULL)
      } else if (estado == "en_proceso") {
        return(
          div(
            class = "alert alert-info",
            tags$div(
              style = "display: flex; align-items: center;",
              tags$div(class = "spinner-border spinner-border-sm me-2", role = "status"),
              "Renderizando documento, por favor espere..."
            )
          )
        )
      } else if (estado == "finalizado") {
        return(
          div(
            class = "alert alert-success",
            tags$div(
              style = "display: flex; align-items: center;",
              tags$i(class = "fa fa-check-circle me-2"),
              "Documento renderizado correctamente"
            )
          )
        )
      } else if (estado == "error") {
        return(
          div(
            class = "alert alert-danger",
            tags$div(
              style = "display: flex; align-items: center;",
              tags$i(class = "fa fa-exclamation-circle me-2"),
              "Error al renderizar el documento"
            )
          )
        )
      }
    })
    
    } # FIN IF
    
    
  })
  

}

