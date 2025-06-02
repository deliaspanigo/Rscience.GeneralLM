#' @export
module_render_03_full_output_download_ui <- function(id) {
  ns <- NS(id)
  
  div(
    # Agregar el código JavaScript
    tags$head(
      tags$script(HTML(paste0("
        $(document).on('click', '#", ns("download_RCode"), "', function() {
          Shiny.setInputValue('", ns("clic_descarga"), "', true);
        });
      ")))
    ),
    uiOutput(ns("set01_RCode"))
    
    
  )
}


#' @export
module_render_03_full_output_download_server <- function(id, step_pos, number_current_step, 
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
    # Default File Download state
    list_DFDS <- list(
      "check_clic_render" = FALSE,
      "current_time_pritty"     = "",
      "str_current_time" = "",
      "MY_SCRIPT" = "",
      "folder_path_work" = "",
      "file_name_work" = "",
      "file_path_work" = "",
      "check_work" = FALSE,
      "folder_path_delivery" = "", 
      "file_name_delivery" = "", 
      "file_path_delivery" = "", 
      "check_delivery" = FALSE,
      "check_output" = FALSE,
      "button_state_render" = "initial",
      "check_clic_download" = FALSE,
      "button_state_download" = "initial"
    )
    
    internal_01_FILE_RCODE <- do.call(reactiveValues, list_DFDS)
    
    
    output$el_cartel <- renderUI({
      my_cartel <- reactiveValuesToList(internal_TOOLS_SELECTOR)$"pack_output"$"selected_cartel"
      fn_html_cartel(my_text = my_cartel)
    })
    
    output$"set01_RCode" <- renderUI({
      
      the_state_render   <- internal_01_FILE_RCODE$"button_state_render"
      #      the_class_download <- internal_01_FILE_RCODE$"button_class"
      
      btn_class_render <- switch(the_state_render,
                                 "initial"   = "btn-outline-primary",    # Azul inicial
                                 "confirmed" = "btn-success",    # Verde después de confirmar
                                 "modified"  = "btn-outline-primary") 
      
      the_state_download <-  internal_01_FILE_RCODE$"button_state_download"
      btn_class_download <- switch(the_state_download,
                                   "initial"   = "btn-outline-primary",    # Azul inicial
                                   "confirmed" = "btn-success",    # Verde después de confirmar
                                   "modified"  = "btn-outline-primary") 
      fluidRow(
        style = "display: flex; align-items: center; justify-content: space-between;",
        column(3, "R code + All outputs"),
        column(4, 
               actionButton(inputId = ns("render_RCode"), 
                            label = NULL, 
                            icon = icon("play", class = "fa-2x"),
                            class = btn_class_render),
               actionButton(ns("show_RCode"), 
                            label = NULL,
                            icon = icon("binoculars", class = "fa-2x"),  # Cambiado a un ojo grande
                            class = "btn-info",
                            title = "Mostrar Visualización"
               ),
               downloadButton(outputId = ns("download_RCode"), 
                              label = NULL, 
                              icon = icon("download", class = "fa-2x"), 
                              class = btn_class_download)
        ),
        column(5)
      )
    })
    
    
    # Sub_step 01 - Enviroment and THE_CAPSULE
    observe({
      # Requeriments -----------------------------------------------------------
      req(sub_step() == 1)
      req(!ALL_DONE())
      req(number_current_step() == step_pos)
      req(internal_PLAY, internal_TOOLS_SELECTOR, internal_CFG)
      req(internal_PLAY$"check_output", internal_TOOLS_SELECTOR$"check_output", internal_CFG$"check_output")
      
      # THE_CAPSULE$"file_name_work" = APP_TOTEM[["step8"]]$"pack_output"$"download"$"file01_Rcode"$"work_file_name"
      # THE_CAPSULE$"file_name_delivery" = APP_TOTEM[["step8"]]$"pack_output"$"download"$"file01_Rcode"$"delivery_file_name"
      # 
      isolate({
        THE_CAPSULE$"current_step" = number_current_step()
        THE_CAPSULE$"current_step_name" = paste0(STR_STEP_NAME, step_pos)
        THE_CAPSULE$"current_label" ="Step 10: Download"
        THE_CAPSULE$"new_list_step" = NA # Not necesary at download???
        THE_CAPSULE$"selected_tool" = internal_CFG$"pack_output"$"id"
        THE_CAPSULE$"folder_path_work" = APP_TOTEM[["step8"]]$"pack_output"$"path_folder_work"
        THE_CAPSULE$"folder_path_delivery" = APP_TOTEM[["step8"]]$"pack_output"$"path_folder_output"
        THE_CAPSULE$"Rcode_quarto"  = APP_TOTEM[["step7"]]$"pack_output"$"Rcode_quarto"
      })
      
      
      sub_step(sub_step()+1)
    })
    
    ok_show <- reactive({
      sub_step() >= 2
    })
    
    
    
    
    
    
    fn_shiny_ACTIVE_SHOW_MODAL <- function(){
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
    
    
    observeEvent(input$"render_RCode", {
      
      req(sub_step() == 2)
      
      # Abrir show modal
      fn_shiny_ACTIVE_SHOW_MODAL()
      
      
      
      # Reset por las dudas
      fn_shiny_apply_changes_reactiveValues(rv = internal_01_FILE_RCODE,  
                                            changes_list = list_DFDS
      )
      
      
      # From Capsule
      MY_SCRIPT <- THE_CAPSULE$"Rcode_quarto"
      folder_path_work <- THE_CAPSULE$"folder_path_work"
      folder_path_delivery <- THE_CAPSULE$"folder_path_delivery"
      
      # From yaml config
      file_name_work       <- APP_TOTEM[["step5"]]$"pack_output"$"download"$"file02"$"file_name_work" #file01_Rscript_GeneralLM_fix_anova1.R"
      file_name_delivery   <- APP_TOTEM[["step5"]]$"pack_output"$"download"$"file02"$"file_name_delivery"
      
      
      
      if(internal_01_FILE_RCODE[["check_clic_render"]] == list_DFDS[["check_clic_render"]]) local({
        internal_01_FILE_RCODE[["check_clic_render"]] <- TRUE
      })
      
      if(internal_01_FILE_RCODE[["current_time_pritty"]] == list_DFDS[["current_time_pritty"]]) local({
        internal_01_FILE_RCODE[["current_time_pritty"]] <- fn_R_the_time_beauty()
      })
      
      if(internal_01_FILE_RCODE[["str_current_time"]] == list_DFDS[["str_current_time"]])local({
        current_time_pritty   <-   internal_01_FILE_RCODE[["current_time_pritty"]]
        str_current_time      <-  gsub("[^0-9]", "_", current_time_pritty)
        internal_01_FILE_RCODE[["str_current_time"]] <- str_current_time
      })
      
      if(internal_01_FILE_RCODE[["MY_SCRIPT"]] == list_DFDS[["MY_SCRIPT"]])local({
        internal_01_FILE_RCODE[["MY_SCRIPT"]] <- MY_SCRIPT
      })
      
      if(internal_01_FILE_RCODE[["folder_path_work"]] == list_DFDS[["folder_path_work"]])local({
        
        internal_01_FILE_RCODE[["folder_path_work"]] <- folder_path_work
      })
      
      if(internal_01_FILE_RCODE[["file_name_work"]] == list_DFDS[["file_name_work"]])local({
        
        internal_01_FILE_RCODE[["file_name_work"]] <- file_name_work
      })
      
      if(internal_01_FILE_RCODE[["file_path_work"]] == list_DFDS[["file_path_work"]])local({
        file_path_work <-file.path(folder_path_work, file_name_work)
        
        internal_01_FILE_RCODE[["file_path_work"]] <- file_path_work
      })
      
      if(internal_01_FILE_RCODE[["check_work"]] == list_DFDS[["check_work"]])local({
        file_path_work <- internal_01_FILE_RCODE[["file_path_work"]]
        
        check_work <- file.exists(file_path_work)
        internal_01_FILE_RCODE[["check_work"]] <- check_work
      })
      
      if(internal_01_FILE_RCODE[["folder_path_delivery"]] == list_DFDS[["folder_path_delivery"]])local({
        
        internal_01_FILE_RCODE[["folder_path_delivery"]] <- folder_path_delivery
      })
      
      if(internal_01_FILE_RCODE[["file_name_delivery"]] == list_DFDS[["file_name_delivery"]])local({
        # new file name for delivery
        str_current_time <- internal_01_FILE_RCODE[["str_current_time"]]
        file_ext <- paste0(".", tools::file_ext(file_name_delivery))
        file_name_no_ext <- tools::file_path_sans_ext(file_name_delivery)
        file_name_mod <- paste0(file_name_no_ext, "_", str_current_time, file_ext)
        file_name_delivery <- file_name_mod
        internal_01_FILE_RCODE[["file_name_delivery"]] <- file_name_delivery
      })
      
      if(internal_01_FILE_RCODE[["file_path_delivery"]] == list_DFDS[["file_path_delivery"]])local({
        file_path_delivery <-file.path(folder_path_delivery, file_name_delivery)
        
        internal_01_FILE_RCODE[["file_path_delivery"]] <- file_path_delivery
      })
      
      if(internal_01_FILE_RCODE[["check_delivery"]] == list_DFDS[["check_delivery"]])local({
        file_path_delivery <- internal_01_FILE_RCODE[["file_path_delivery"]]
        
        check_delivery <- !file.exists(file_path_delivery)
        internal_01_FILE_RCODE[["check_delivery"]] <- check_delivery
      })
      
      ###
      file_path_work <- internal_01_FILE_RCODE[["file_path_work"]]
      file_path_delivery <- internal_01_FILE_RCODE[["file_path_delivery"]]
      file_path_from <- file.path(folder_path_work, file_name_delivery)
      ###
      
      message(crayon::green("Starting process..."))
      message(crayon::green("Please, wait..."))

      dir_actual <- getwd()
      my_temporal_folder <- folder_path_work
      setwd(my_temporal_folder)
      

      
      codigo_extra <- paste(paste(MY_SCRIPT, collapse = "\n"))
      lineas <- readLines(file_name_work)
      lineas <- sub(pattern = '"_mi_codigo_"', replacement = codigo_extra, x = lineas)
      writeLines(lineas, file_name_work)
      
      quarto::quarto_render(input = file_name_work, 
                            output_file = file_name_delivery,
                            quiet = TRUE)
      
      message(crayon::green("Process completed!"))
      message("")
      
      setwd(dir_actual)
      
      file.copy(from = file_path_from, to = file_path_delivery, overwrite = TRUE)
      
      
      if(internal_01_FILE_RCODE[["check_output"]] == list_DFDS[["check_output"]])local({
        file_path_delivery <- internal_01_FILE_RCODE[["file_path_delivery"]]
        
        check_output <- file.exists(file_path_delivery)
        # print(reactiveValuesToList(internal_01_FILE_RCODE))
        # print(check_output)
        internal_01_FILE_RCODE[["check_output"]] <- check_output
      })
      
      if(internal_01_FILE_RCODE[["button_state_render"]] == list_DFDS[["button_state_render"]])local({
        check_output <- internal_01_FILE_RCODE[["check_output"]]
        
        if(check_output){
          button_state_render <- "confirmed"
          internal_01_FILE_RCODE[["button_state_render"]] <- button_state_render
        }
        
      })
      
      
      THE_MODAL_RCODE(FALSE)
      
    })
    
    observeEvent(input$"download_RCode", {
      # print("HOLA")
    })
    
    
    # Crear un reactiveValues para almacenar el contador de clics
    contador <- reactiveValues(clics = 0)
    
    # Observar cuando se hace clic en el botón de descarga
    observeEvent(input$clic_descarga, {
      # Incrementar el contador de clics
      req(internal_01_FILE_RCODE$"check_output")
      contador$clics <- contador$clics + 1
      
      internal_01_FILE_RCODE$"check_clic_download" <- TRUE
      internal_01_FILE_RCODE$"button_state_download" <- "confirmed"
      
    })
    
    output$download_RCode <- downloadHandler(
      filename = function() {
        internal_01_FILE_RCODE$"file_name_delivery"
      },
      content = function(file) {
        req(internal_01_FILE_RCODE$"check_output")
        file.copy(internal_01_FILE_RCODE$"file_path_delivery", file)
      }
    )
    ############################################################################
    
    output$el_cartel2 <- renderUI({
      my_cartel <- reactiveValuesToList(internal_TOOLS_SELECTOR)$"pack_output"$"selected_cartel"
      fn_html_cartel(my_text = my_cartel)
    })
    # style = "height: 100%; width: 100%; max-width: 100%; box-sizing: border-box; overflow-x: hidden;",  # Ajustes para evitar el scroll horizontal
    
    
    
    ##############################################################################
    # Tab05 - RCode
    # Botón para mostrar modal con visualización
    observeEvent(input$show_RCode, {
      
      showModal(
        tagList(
          # CSS personalizado para este modal específico
          tags$div(
            tags$style(HTML("
            .modal-dialog.modal-xl {
              margin: 30px 60px; /* Ajusta el margen superior/inferior y izquierda/derecha */
              width: calc(100% - 120px); /* Ancho ajustado para respetar los márgenes */
              max-width: none; /* Sobrescribe el max-width predeterminado */
            }
            
            .modal-body {
              max-height: calc(100vh - 300px); /* Altura máxima con espacio para header y footer */
              overflow-y: auto; /* Agrega scroll vertical cuando sea necesario */
              padding: 20px; /* Ajusta el padding interno */
            }
          "))
          ),
          modalDialog(
            title = "Visualización",
            div(
              htmlOutput(ns("visual_RLong"))
            ),
            size = "xl",
            easyClose = TRUE,
            footer = modalButton("Close")
          )
        )
      )
      
      
      
      
    })
    

    

    
    output$"visual_RLong" <- renderText({
      req(internal_01_FILE_RCODE$"check_output")

      the_list <- reactiveValuesToList(internal_01_FILE_RCODE)
      # print(the_list)

      file_path_delivery <- internal_01_FILE_RCODE[["file_path_delivery"]]

      # print(file.exists(file_path_delivery))

      file_name_html <- basename(file_path_delivery)
      dir_temp <- dirname(file_path_delivery)
      # print(dir_temp)

      # check_file_RReport()
      addResourcePath(prefix = "super_delivery_folder", directoryPath = dir_temp)
      my_local_file <- file.path("super_delivery_folder", file_name_html)


      # tags$iframe(
      #   src = my_local_file,
      #   width = "100%",
      #   height = "800px",
      #   frameborder = 0
      # )
      armado_v <- paste('<div style="height: 100%; width: 100%; overflow: hidden;"><iframe style="height: 5000vh; width:100%; border: none;" src="', my_local_file, '"></iframe></div>', sep = "")

      return(armado_v)
    })
    ################################################################################
    
    
  })
  
  
}

