library(shiny)
library(bslib)
library(shinyjs)
library(quarto)
library(shinyjs)
library(plotly)
library(htmlwidgets)
library(knitr)
library(Rscience.menu)
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
  
  Sbutton_01_dataselector2_ui("dataset_selector2"), 
  Sbutton_02_tools2_ui(id = "tools_selector2"),
  Sbutton_reset2_ui(id = "reset2"),
  Sbutton_play2_ui(id = "play2"),
    
    MASTER_module_fixed_anova_1_way_ui(id = "MASTER_GeneralLM") 
  
)

# Definir el servidor
server <- function(input, output, session) {
  # Valores predeterminados para reseteo
  
  default_structure <- list(
    pack_input = "",
    check_input = FALSE,
    pack_output = "",
    check_output = FALSE,
    button_class = "initial"
  )    
  
  internal_DATASET_SELECTOR <- do.call(reactiveValues, default_structure)
  active_DATASET_SELECTOR   <- do.call(reactiveValues, default_structure)
  
  internal_TOOLS_SELECTOR <- do.call(reactiveValues, default_structure)
  active_TOOLS_SELECTOR   <- do.call(reactiveValues, default_structure)
  
  internal_PLAY_SELECTOR    <- do.call(reactiveValues, default_structure)
  active_PLAY_SELECTOR    <- do.call(reactiveValues, default_structure)
  
  
  Sbutton_01_dataselector2_server(id = "dataset_selector2", internal_DATASET_SELECTOR)
  
  Sbutton_02_tools2_server(id = "tools_selector2", internal_TOOLS_SELECTOR)
  
  
  Sbutton_reset2_server(id = "reset2", default_structure, 
                        internal_DATASET_SELECTOR, active_DATASET_SELECTOR,
                        internal_PLAY_SELECTOR,    active_PLAY_SELECTOR)

  
  Sbutton_play2_server(id = "play2", 
                       internal_DATASET_SELECTOR, active_DATASET_SELECTOR,
                       internal_PLAY_SELECTOR,    active_PLAY_SELECTOR)
    
  # observe(print(reactiveValuesToList(internal_DATASET_SELECTOR)))
  MASTER_module_fixed_anova_1_way_server(id = "MASTER_GeneralLM", show_dev = F)
  
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
