#' @export
module_quartoRenderer_ui <- function(id) {
  ns <- NS(id)
  
  div(
    card(
      card_header(
        h3("Renderizar documento Quarto")
      ),
      card_body(
        p("Haz clic en el botón para renderizar el documento Quarto."),
        actionButton(ns("renderizar"), "Renderizar documento", class = "btn-primary mb-3"),
        uiOutput(ns("render_status")),
        
        # Aquí se mostrará el HTML renderizado
        div(
          id = ns("contenedor_html"),
          #style = "display: none; border: 1px solid #ddd; border-radius: 5px; padding: 15px; margin-top: 20px; overflow: visible; max-height: none;",
          htmlOutput(ns("quarto_iframe"))
        )
        
      ),
      # Permitir que el card crezca según sea necesario
      height = "auto"
    )
  )
}

#' @export
module_quartoRenderer_server <- function(id, documento) {
  moduleServer(id, function(input, output, session) {
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
  })
}