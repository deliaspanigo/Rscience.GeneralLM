library(shiny)
library(bslib)
library(shinyjs)
library(quarto)
library(shinyjs)
library(plotly)
library(htmlwidgets)
library(knitr)

library(shiny)
library(bslib)
library(shinyjs)
library(shinyAce)
# 
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
  
  # Necesario para manipular clases de CSS
  useShinyjs(),
  
  
  # Layout con dos columnas - una para la barra lateral y otra para el contenido principal
  
    
    MASTER_module_fixed_anova_1_way_ui(id = "MASTER_fixed_anova_1_way") 
  
)

# Definir el servidor
server <- function(input, output, session) {
  # Valores predeterminados para reseteo
  
  MASTER_module_fixed_anova_1_way_server(id = "MASTER_fixed_anova_1_way", show_dev = F)
  
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
