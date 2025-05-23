library(shiny)
library(DT)
library(plotly)

# Lista de objetos ejemplo
objetos_muestra <- list(
  texto1 = "Este es un texto simple.",
  df_mtcars = mtcars,
  plot_mtcars = plot_ly(data=mtcars, x=~wt, y=~mpg, type='scatter', mode='markers'),
  texto2 = "Otra línea en inglés.",
  df_iris = iris,
  plot_iris = plot_ly(data=iris, x=~Sepal.Length, y=~Sepal.Width, type='scatter', mode='markers')
)

# Función que clasifica objetos
# Función que clasifica objetos
classify_objects <- function(objects) {
  sapply(objects, function(obj) {
    if (is.character(obj) && length(obj) == 1) {
      return("text")
    } else if (is.data.frame(obj)) {
      return("df")
    } else if (inherits(obj, "plotly")) {
      return("plotly")
    } else if (is.matrix(obj)) {
      return("matrix")
    } else if (is.list(obj)) {
      return("list")  # categoría adicional
    } else if (is.vector(obj) && length(obj) > 1) {
      return("vector")
    } else if (is.vector(obj) && length(obj) == 1 && is.numeric(obj)) {
      return("numeric")
    } else {
      return("unknown")
    }
  })
}


# UI
ui <- fluidPage(
  titlePanel("Advanced internal management with reactiveValues"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("seleccion_input", "Select objects:", choices = names(objetos_muestra))
    ),
    mainPanel(
      uiOutput("dynamic_ui")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Internal reactive structure
  internal_state <- reactiveValues(
    all_objects = objetos_muestra,
    selected_names = character(0),
    categories = character(0)
  )
  
  # Actualizar colección interna cuando cambia la selección
  observeEvent(input$seleccion_input, {
    internal_state$selected_names <- input$seleccion_input
    internal_state$categories <- classify_objects(
      lapply(internal_state$all_objects[internal_state$selected_names], function(obj) obj)
    )
  }, ignoreNULL = FALSE)
  
  # Renderizar UI y outputs dinámicos
  output$dynamic_ui <- renderUI({
    req(internal_state$selected_names)
    n <- length(internal_state$selected_names)
    # ui_list <- list()
    
    my_fn <- function(internal_state){
      
      vector_names_espected <- c("all_objects", "selected_names", "categories")
      vector_names_observed <- names(internal_state)
      all_ok <- all(vector_names_observed %in% vector_names_espected)
      
      print(all_ok)
      
      ui_list <- list()
      for (i in seq_along(internal_state$selected_names)) {
      name_obj <- internal_state$selected_names[i]
      tipo_obj <- internal_state$categories[i]
      obj <- internal_state$all_objects[[name_obj]]
      
      if (tipo_obj == "text") {
        ui_list <- c(ui_list, list(
          tags$h4(paste("Text:", name_obj)),
          textOutput(paste0("txt_", i))
        ))
        # Render en server
        local({
          j <- i
          val <- obj
          output[[paste0("txt_", j)]] <- renderText({ val })
        })
        
      } else if (tipo_obj == "df") {
        ui_list <- c(ui_list, list(
          tags$h4(paste("DataFrame:", name_obj)),
          dataTableOutput(paste0("dt_", i))
        ))
        local({
          j <- i
          dt <- obj
          output[[paste0("dt_", j)]] <- renderDataTable({ dt })
        })
        
      } else if (tipo_obj == "plotly") {
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
        ui_list <- c(ui_list, list(
          HTML(paste0("<b style='color:red;'>Unsupported type: ", name_obj, "</b>"))
        ))
      }
    }
    
      return(ui_list)
    }
    
    ui_list <- my_fn(internal_state)
  })
}

# Ejecutar app
shinyApp(ui, server)
