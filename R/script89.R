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
  
  observeEvent(input$seleccion, {
    # Limpiar todos los outputs anteriores para evitar conflictos
    # Y crear los outputs necesarios para cada objeto seleccionado
    # Como los IDs van dinámicamente, hay que hacerlo con superview o similar
    # Pero esto puede complicarse.
    # Alternativa sencilla: crear todos los outputs estáticos en UI y solo llenar o vaciar.
    # Tenemos que preparar la UI con un espacio para cada posible output.
    # Pero por lo que quieres, lo mejor es crear la UI con un uiOutput y crear todos los outputs en el servidor
    
    # En este ejemplo, creamos en UI todos los outputs con un prefix
    # Y en servidor, llenamos los necesarios según selección
    
    # Primero, vaciamos toda la UI
    output$dinamico <- renderUI({ NULL })
    
    # Lista para la UI
    ui_list <- list()
    
    # Recolectamos objetos seleccionados
    vector_names_objetos <- reactive({
      input$seleccion
    })
    
    objetos_seleccionados <- lapply(vector_names_objetos(), function(n)
      get(n, envir=as.environment(objetos_muestra))
    )
    
    # Para cada uno, creamos los outputs y UI
    for(i in seq_along(objetos_seleccionados)) {
      obj <- objetos_seleccionados[[i]]
      name_obj <- input$seleccion[i]
      
      if (is.character(obj) && length(obj)==1) {
        # Texto
        ui_list <- c(ui_list,
                     list(tags$h4(paste("Texto:", name_obj)),
                          textOutput(paste0("txt_", i))))
        # Render en servidor
        local({
          j <- i
          val <- obj
          output[[paste0("txt_", j)]] <- renderText({ val })
        })
      } else if (is.data.frame(obj) && nrow(obj) > 0 && ncol(obj) > 0) {
        # DataFrame
        ui_list <- c(ui_list,
                     list(tags$h4(paste("DataFrame:", name_obj)),
                          dataTableOutput(paste0("dt_", i))))
        local({
          j <- i
          datos <- obj
          output[[paste0("dt_", j)]] <- renderDataTable({ datos })
        })
      } else if (inherits(obj, "plotly")) {
        # Plot
        ui_list <- c(ui_list,
                     list(tags$h4(paste("Gráfico:", name_obj)),
                          plotlyOutput(paste0("plt_", i))))
        local({
          j <- i
          p <- obj
          output[[paste0("plt_", j)]] <- renderPlotly({ p })
        })
      } else {
        # No soportado
        ui_list <- c(ui_list,
                     list(HTML(paste0("<b style='color:red;'>Tipo no soportado: ", name_obj, "</b>"))))
      }
    }
    
    # Mostrar toda la UI
    output$dinamico <- renderUI({ ui_list })
  }, ignoreNULL=FALSE)
  
}

shinyApp(ui, server)
