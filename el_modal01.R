library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  actionButton("show_modal", "Mostrar Modal")
)

server <- function(input, output, session) {
  
  # ReactiveValues para controlar el estado de cada línea
  line1_ready <- reactiveVal(FALSE)
  line2_ready <- reactiveVal(FALSE)
  line3_ready <- reactiveVal(FALSE)
  
  # Función para crear el contenido del modal
  modal_content <- function() {
    modalDialog(
      title = "Proceso",
      tags$ul(
        tags$li(span(id = "line1_icon", uiOutput("line1_icon_ui")), " Línea 1"),
        tags$li(span(id = "line2_icon", uiOutput("line2_icon_ui")), " Línea 2"),
        tags$li(span(id = "line3_icon", uiOutput("line3_icon_ui")), " Línea 3")
      ),
      footer = modalButton("Cerrar")
    )
  }
  
  observeEvent(input$show_modal, {
    showModal(modal_content())
    
    # JavaScript para cambiar los reactiveValues después de un tiempo
    runjs('
      setTimeout(function() {
        Shiny.setInputValue("line1_ready", true, {priority: "event"});
      }, 3000); // 3 segundos

      setTimeout(function() {
        Shiny.setInputValue("line2_ready", true, {priority: "event"});
      }, 6000); // 6 segundos

      setTimeout(function() {
        Shiny.setInputValue("line3_ready", true, {priority: "event"});
      }, 9000); // 9 segundos
    ')
  })
  
  # Observers para actualizar los reactiveVals basados en los inputs
  observeEvent(input$line1_ready, {
    line1_ready(input$line1_ready)
  })
  observeEvent(input$line2_ready, {
    line2_ready(input$line2_ready)
  })
  observeEvent(input$line3_ready, {
    line3_ready(input$line3_ready)
  })
  
  # RenderUI para los iconos
  output$line1_icon_ui <- renderUI({
    if (line1_ready()) {
      tags$i(class = "fas fa-check-circle green-icon")
    } else {
      tags$i(class = "fas fa-spinner fa-spin blue-icon")
    }
  })
  
  output$line2_icon_ui <- renderUI({
    if (line2_ready()) {
      tags$i(class = "fas fa-check-circle green-icon")
    } else {
      tags$i(class = "fas fa-spinner fa-spin blue-icon")
    }
  })
  
  output$line3_icon_ui <- renderUI({
    if (line3_ready()) {
      tags$i(class = "fas fa-check-circle green-icon")
    } else {
      tags$i(class = "fas fa-spinner fa-spin blue-icon")
    }
  })
}

css <- HTML("
  <style>
    .blue-icon {
      color: blue;
    }
    .green-icon {
      color: green;
    }
  </style>
")

ui <- tagList(ui, css)

shinyApp(ui, server)
