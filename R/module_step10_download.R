#' @export
module_step10_download_ui <- function(id) {
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
module_step10_download_server <- function(id, step_pos, number_current_step, 
                                           STR_STEP_NAME, default_list_step, 
                                           APP_TOTEM, internal_TOOLS_SELECTOR,
                                           internal_CFG, internal_PLAY) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Valores por defecto
    THE_CAPSULE <- reactiveValues()
    sub_step <- reactiveVal(1)
    ALL_DONE <- reactiveVal(FALSE)
    local_reset <- reactiveVal(FALSE)
    
    # Determinar el reset local
    observe({
      
      check_there_is_content <- sub_step()>1 
      check_no_play_pressed  <- !internal_PLAY$"check_output" 
      
      super_check <- all(check_there_is_content, check_no_play_pressed)
      local_reset(super_check)
    })
    
    # Aplicar reset local
    observeEvent(local_reset(), {
      req(local_reset())
      
      for (name in ls(THE_CAPSULE)) {
        THE_CAPSULE[[name]] <- NULL
      }
      sub_step(1)
      ALL_DONE(FALSE)
      local_reset(FALSE)
      
    })  
    
    ###-------------------------------------------------------------------------
    
    # Sub_step 01 - Enviroment
    observe({
      # Requeriments -----------------------------------------------------------
      req(sub_step() == 1)
      req(!ALL_DONE())
      req(number_current_step() == step_pos)
      req(internal_PLAY, internal_TOOLS_SELECTOR, internal_CFG)
      req(internal_PLAY$"check_output", internal_TOOLS_SELECTOR$"check_output", internal_CFG$"check_output")
      
      isolate({
        THE_CAPSULE$"current_step" = number_current_step()
        THE_CAPSULE$"current_step_name" = paste0(STR_STEP_NAME, step_pos)
        THE_CAPSULE$"current_label" ="Step 10: Download"
        THE_CAPSULE$"new_list_step" = NA # Not necesary at download???
        THE_CAPSULE$"selected_tool" = internal_CFG$"pack_output"$"id"
        THE_CAPSULE$"Rcode_script"  = APP_TOTEM[["step7"]]$"Rcode_script"
        THE_CAPSULE$"folder_path_work" = APP_TOTEM[["step8"]]$"pack_output"$"path_folder_work"
        THE_CAPSULE$"folder_path_delivery" = APP_TOTEM[["step8"]]$"pack_output"$"path_folder_output"
      })
      
      
      sub_step(sub_step()+1)
    })
    
    ok_show <- reactive({
      sub_step() >= 2
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
    
    internal_01_FILE_RCODE <- do.call(reactiveValues, default_structure_file_download)
    internal_02_RLONG  <- do.call(reactiveValues, default_structure_file_download)
    
    output$el_cartel <- renderUI({
      my_cartel <- reactiveValuesToList(internal_TOOLS_SELECTOR)$"pack_output"$"selected_cartel"
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
      
   
##################################################################################
    # Verificacion de existencia de la carpeta work y delivery.
    # Cuando da clic en renderizar el archivo, toma la hora.
    # Armado del nombre del nuevo archivo con lo detallado en yaml y la hora del paso anterior.
    # Armar el path a la carpeta delivery
    # Verificar que no existe en la carpeta delivery, si existe borrarlo o dar error, no se.
    # Guardar el script en delivery.
    # Dar al downloader el path del archivo en la carpeta delivery
    
    # Crear un "internal" para cada archivo, y luego un new_list_step que contiene a todos los
    # internal de cada archivo. 
    
    # Crear un modulo visor de archivos. Servira para mostrar los archivos .R, pero tambien
    # para mostrar archivos txt, yaml o cualquier otra cos de texto. 
    # En descargas debe haber si o si la posibilidades de descargar un txt que da detalles
    # de la version de R, Rscience, las librerias, etc.
    # ---- EN Script, debe estar el script completo de R, pero tambien las sentencias
    # de la libreria Rscience para ejecutar eso mismo completo.
    
    
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
      # print(reactiveValuesToList(internal_03_TOTEM_DOWNLOAD))
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
    
################################################################################
    
    
  })
  
  
}

