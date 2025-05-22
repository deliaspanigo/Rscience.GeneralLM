library(shiny)
library(bslib)
library(shinyjs)
library(quarto)
library(shinyjs)
library(plotly)
library(htmlwidgets)
library(knitr)
library(Rscience.menu)
library(Rscience.import)
library(shiny)
library(bslib)
library(shinyjs)
library(shinyAce)
#library(fontawesome)
library(DT)
library(shinycssloaders)
library(EnvStats)
library(crayon)

# library(fontawesome)
# source("global.R")
# 
# # Cargar los módulos
# source("modulos.R")
# source("modules_model_selection.R")
# source("modules_variables_anova.R")
# source("modules_quarto.R")


# Definir la interfaz de usuario principal
ui <- page_fluid( # Cambiado de page_sidebar a page_fluid para más flexibilidad
  #theme = bs_theme(version = 5, bootswatch = "minty"),
  theme = bs_theme(version = 5),
  # tags$head(
  #   tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css")
  # ),
  # tags$head(
  #   tags$link(rel = "stylesheet", href = "fontawesome/css/all.min.css")
  # ),
  # Necesario para manipular clases de CSS
  useShinyjs(),
  
  
  # Layout con dos columnas - una para la barra lateral y otra para el contenido principal
   MASTER_module_Rscience_Main_ui(id = "MASTER_MAIN")#,
  
    
     # MASTER_module_fixed_anova_1_way_ui(id = "MASTER_GeneralLM") 
  
)

# Definir el servidor
server <- function(input, output, session) {
  # Valores predeterminados para reseteo
  
  
    
  MASTER_module_Rscience_Main_server(id = "MASTER_MAIN", show_dev = F)
  
  # observe(print(reactiveValuesToList(internal_DATASET_SELECTOR)))
  # MASTER_module_fixed_anova_1_way_server(id = "MASTER_GeneralLM", show_dev = F)
  
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
