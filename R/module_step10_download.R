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
                 # uiOutput(ns("set02_RLong")),
                 # hr()#,
                 #uiOutput(ns("set03_RReport"))
          )#,
          # column(6, actionButton(ns("download_ALL"), "Download ALL", class = "btn-primary", icon = icon("download"))),
        ),
        br()
        # uiOutput(ns("visual_para_archivos")),
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
    internal_02_RLONG  <- do.call(reactiveValues, list_DFDS)
    
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
      
      fluidRow(
        column(3, "R code"),
        column(2, actionButton(ns("render_RCode"), "Render", class = btn_class_render)),
        # column(2, "Status"),
        column(2, downloadButton(ns("download_RCode"), "Download", class = "btn-primary", icon = icon("download"))),
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
        THE_CAPSULE$"Rcode_script"  = APP_TOTEM[["step7"]]$"pack_output"$"Rcode_script"
      })
      
      
      sub_step(sub_step()+1)
    })
    
    ok_show <- reactive({
      sub_step() >= 2
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
        MY_SCRIPT <- THE_CAPSULE$"Rcode_script"
        folder_path_work <- THE_CAPSULE$"folder_path_work"
        folder_path_delivery <- THE_CAPSULE$"folder_path_delivery"
        
        # From yaml config
        file_name_work   <- "file01_Rscript_GeneralLM_fix_anova1.R"
        file_name_delivery   <- "file01_Rscript_GeneralLM_fix_anova1.R"
        
          
        if(internal_01_FILE_RCODE[["check_clic_render"]] == list_DFDS[["check_clic_render"]]){
          internal_01_FILE_RCODE[["check_clic_render"]] <- TRUE
        }
        
        if(internal_01_FILE_RCODE[["current_time_pritty"]] == list_DFDS[["current_time_pritty"]]){
          internal_01_FILE_RCODE[["current_time_pritty"]] <- fn_R_the_time_beauty()
        }
        
        if(internal_01_FILE_RCODE[["str_current_time"]] == list_DFDS[["str_current_time"]]){
          current_time_pritty   <-   internal_01_FILE_RCODE[["current_time_pritty"]]
          str_current_time      <-  gsub("[^0-9]", "_", current_time_pritty)
          internal_01_FILE_RCODE[["str_current_time"]] <- str_current_time
        }
        
        if(internal_01_FILE_RCODE[["MY_SCRIPT"]] == list_DFDS[["MY_SCRIPT"]]){
          internal_01_FILE_RCODE[["MY_SCRIPT"]] <- MY_SCRIPT
        }
        
        if(internal_01_FILE_RCODE[["folder_path_work"]] == list_DFDS[["folder_path_work"]]){
          
          internal_01_FILE_RCODE[["folder_path_work"]] <- folder_path_work
        }
        
        if(internal_01_FILE_RCODE[["file_name_work"]] == list_DFDS[["file_name_work"]]){
          
          internal_01_FILE_RCODE[["file_name_work"]] <- file_name_work
        }
        
        if(internal_01_FILE_RCODE[["file_path_work"]] == list_DFDS[["file_path_work"]]){
          file_path_work <-file.path(folder_path_work, file_name_work)
          
          internal_01_FILE_RCODE[["file_path_work"]] <- file_path_work
        }
        
        if(internal_01_FILE_RCODE[["check_work"]] == list_DFDS[["check_work"]]){
          file_path_work <- internal_01_FILE_RCODE[["file_path_work"]]
          
          check_work <- file.exists(file_path_work)
          internal_01_FILE_RCODE[["check_work"]] <- check_work
        }
        
        if(internal_01_FILE_RCODE[["folder_path_delivery"]] == list_DFDS[["folder_path_delivery"]]){
          
          internal_01_FILE_RCODE[["folder_path_delivery"]] <- folder_path_delivery
        }
        
        if(internal_01_FILE_RCODE[["file_name_delivery"]] == list_DFDS[["file_name_delivery"]]){
          # new file name for delivery
          file_ext <- tools::file_ext(file_name_delivery)
          file_name_no_ext <- tools::file_path_sans_ext(file_name_delivery)
          file_name_mod <- paste0(file_name_no_ext, "_", str_current_time, file_ext)
          internal_01_FILE_RCODE[["file_name_delivery"]] <- file_name_delivery
        }
        
        if(internal_01_FILE_RCODE[["file_path_delivery"]] == list_DFDS[["file_path_delivery"]]){
          file_path_delivery <-file.path(folder_path_delivery, file_name_delivery)
          
          internal_01_FILE_RCODE[["file_path_delivery"]] <- file_path_delivery
        }
        
        if(internal_01_FILE_RCODE[["check_delivery"]] == list_DFDS[["check_delivery"]]){
          file_path_delivery <- internal_01_FILE_RCODE[["file_path_delivery"]]
          
          check_delivery <- !file.exists(file_path_delivery)
          internal_01_FILE_RCODE[["check_delivery"]] <- check_delivery
        }
        
        message(green("Starting process..."))
        message(green("Please, wait..."))
        file_path_delivery <- internal_01_FILE_RCODE[["file_path_delivery"]]
        writeLines(MY_SCRIPT, file_path_delivery)
        message(green("Process completed!"))
        message("")
        
        if(internal_01_FILE_RCODE[["check_output"]] == list_DFDS[["check_output"]]){
          file_path_delivery <- internal_01_FILE_RCODE[["file_path_delivery"]]
          
          check_output <- file.exists(file_path_delivery)
          internal_01_FILE_RCODE[["check_output"]] <- check_output
        }
        
        if(internal_01_FILE_RCODE[["button_state_render"]] == list_DFDS[["button_state_render"]]){
          check_output <- internal_01_FILE_RCODE[["check_output"]]
          
          if(check_output){
            button_state_render <- "confirmed"
            internal_01_FILE_RCODE[["button_state_render"]] <- button_state_render
          }

        }
        
     print(reactiveValuesToList(THE_CAPSULE))
     print(reactiveValuesToList(internal_01_FILE_RCODE))
      
      THE_MODAL_RCODE(FALSE)
      
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
    
################################################################################
    
    
  })
  
  
}

