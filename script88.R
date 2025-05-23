library(shiny)
library(DT)
library(plotly)

# Lista de objetos variados
objetos_muestra <- list(
  texto1 = "Este es un texto simple.",
  df_mtcars = mtcars,
  plot_mtcars = plot_ly(data=mtcars, x=~wt, y=~mpg, type='scatter', mode='markers'),
  texto2 = "Otra línea en inglés.",
  df_iris = iris,
  plot_iris = plot_ly(data=iris, x=~Sepal.Length, y=~Sepal.Width, type='scatter', mode='markers')
)

ui <- fluidPage(
  titlePanel("Render múltiple de objetos de la lista"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("seleccion", "Selecciona objetos:", choices = names(objetos_muestra))
    ),
    mainPanel(
      uiOutput("dinamico")
    )
  )
)

server <- function(input, output, session) {
  
  # Cuando la selección cambie, procesamos todos los objetos seleccionados
  observeEvent(input$seleccion, {
    objetos_seleccionados <- lapply(input$seleccion, function(n) {
      get(n, envir = as.environment(objetos_muestra))
    })
    
    # Limpiamos lo anterior
    output$dinamico <- renderUI({ NULL })
    
    # Lista para agregar los outputs en UI
    ui_list <- list()
    # Lista para agregar los renderers
    for (i in seq_along(objetos_seleccionados)) {
      obj <- objetos_seleccionados[[i]]
      name_obj <- input$seleccion[i]
      
      if (is.character(obj) && length(obj) == 1) {
        # Texto
        ui_list <- c(ui_list, list(
          tags$h4(paste("Texto:", name_obj)),
          textOutput(paste0("txt_", i))
        ))
        local({
          j <- i
          val <- obj
          output[[paste0("txt_", j)]] <- renderText({ val })
        })
      } else if (is.data.frame(obj) && nrow(obj) > 0 && ncol(obj) > 0) {
        # DataFrame
        ui_list <- c(ui_list, list(
          tags$h4(paste("DataFrame:", name_obj)),
          dataTableOutput(paste0("dt_", i))
        ))
        local({
          j <- i
          dati <- obj
          output[[paste0("dt_", j)]] <- renderDataTable({ dati })
        })
      } else if (inherits(obj, "plotly")) {
        # Plotly
        ui_list <- c(ui_list, list(
          tags$h4(paste("Plot:", name_obj)),
          plotlyOutput(paste0("plt_", i))
        ))
        local({
          j <- i
          p <- obj
          output[[paste0("plt_", j)]] <- renderPlotly({ p })
        })
      } else {
        # Tipo no soportado o vacío
        ui_list <- c(ui_list, list(
          HTML(paste0("<b style='color:red;'>Tipo no soportado: ", name_obj, "</b>"))
        ))
      }
    }
    # Renderizamos toda la UI construida
    output$dinamico <- renderUI({ ui_list })
  }, ignoreNULL = FALSE)
}

shinyApp(ui, server)
